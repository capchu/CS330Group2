#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define marker 'no)
(define prev-free-ptr -1)
 
(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (make-free 0 (heap-size) -1)))

;; make-free: number number number -> number
;; creates a free block starting at the given address
(define (make-free ptr size next)
  (begin
    (heap-set! ptr 'free)
    (heap-set! (+ 1 ptr) marker)
    (heap-set! (+ 2 ptr) size)
    (heap-set! (+ 3 ptr) next)))

;; get-free-block: number number -> number
;; starts up a recursive search through the free list
(define (get-free-block free-ptr size)
  (set! prev-free-ptr -1)
  (get-free-block-rec free-ptr size))

;; get-free-block-rec: number number -> number
;; recursively searches for a free block in the free list
(define (get-free-block-rec free-ptr size)
  (cond
    [(not (location? free-ptr)) -1]
    [(<= size (heap-ref (+ free-ptr 2))) free-ptr]
    [else
     (begin
       (set! prev-free-ptr free-ptr)
       (get-free-block-rec (heap-ref (+ free-ptr 3)) size))]))

(define (gc:alloc-flat p)
  (begin
    (when (not (location? (get-free-block heap-ptr 4)))
      (mark-sweep (get-root-set))
      (when (not (location? (get-free-block heap-ptr 4)))
        (error 'gc:alloc-flat "out of memory")))
    (let ([free-ptr (get-free-block heap-ptr 4)]
          [free-size (heap-ref (+ heap-ptr 2))]
          [free-next (heap-ref (+ heap-ptr 3))])
      (heap-set! free-ptr 'prim)
      (heap-set! (+ free-ptr 1) marker)
      (heap-set! (+ free-ptr 2) p)
     
      (if (> free-size 4)
        (make-free (+ free-ptr 4) (- free-size 4) free-next)
        (when (location? prev-free-ptr)
          (heap-set! (+ prev-free-ptr 3) free-next)))
      (when (= free-ptr heap-ptr)
        (if (> free-size 4)
            (set! heap-ptr (+ free-ptr 4))
            (set! heap-ptr free-next)))
      free-ptr)))

(define (gc:cons f r)
  (begin
    (when (not (location? (get-free-block heap-ptr 4)))
      (mark-sweep (get-root-set f r))
      (when (not (location? (get-free-block heap-ptr 4)))
        (error 'gc:alloc-flat "out of memory")))
    (let ([free-ptr (get-free-block heap-ptr 4)]
          [free-size (heap-ref (+ heap-ptr 2))]
          [free-next (heap-ref (+ heap-ptr 3))])

      (heap-set! free-ptr 'cons)
      (heap-set! (+ free-ptr 1) marker)
      (heap-set! (+ free-ptr 2) f)
      (heap-set! (+ free-ptr 3) r)
      
      (if (> free-size 4)
        (make-free (+ free-ptr 4) (- free-size 4) free-next)
        (when (location? prev-free-ptr)
          (heap-set! (+ prev-free-ptr 3) free-next)))
      (when (= free-ptr heap-ptr)
        (if (> free-size 4)
            (set! heap-ptr (+ free-ptr 4))
            (set! heap-ptr free-next)))
      free-ptr)))

;; toggle-marker: nothing -> nothing
;; toggles the marker between 'yes and 'no; now we don't have to reset all of the markers each time
(define (toggle-marker)
  (if (symbol=? marker 'no)
      (set! marker 'yes)
      (set! marker 'no)))

;; mark-sweep: list[roots] -> void
;; starts the garbage collection
(define (mark-sweep root-set)
  (toggle-marker)
  (mark root-set)
  (sweep))

;; mark: list[roots] -> void
;; initiates the mark phase
(define (mark roots)
  (when (not (empty? roots))
         (mark-rec (read-root (first roots)))
         (mark (rest roots))))

;; mark-rec: number -> void
;; recursively marks all blocks associated w/ obj-ptr
(define (mark-rec obj-ptr)
  (printf (number->string obj-ptr))
  (when (not (eq? (heap-ref (+ obj-ptr 1)) marker)) ;prevent cycles
    (heap-set! (+ obj-ptr 1) marker)
    (cond
      [(and (symbol? (heap-ref obj-ptr))
            (symbol=? 'cons (heap-ref obj-ptr)))
       (mark-rec (heap-ref (+ obj-ptr 2)))
       (mark-rec (heap-ref (+ obj-ptr 3)))]
      [(procedure? (heap-ref (+ obj-ptr 2)))
       (mark (procedure-roots (heap-ref (+ obj-ptr 2))))])))

;; sweep: void -> void
;; initiates the sweep phase
(define (sweep)
  (printf (string-append "\nSweeping non-" (symbol->string marker)))
  (sweep-rec 0 -1 -1))

;; sweep-rec: number number number -> void
;; recursively frees all blocks that have not been marked
(define (sweep-rec ptr prev prev-free)
  (if (< (+ ptr 4) (heap-size))
      (let ([sym (heap-ref ptr)]
            [ptr-marker (heap-ref (+ ptr 1))])
        (printf (number->string ptr))
        (cond
          [(or (symbol=? sym 'prim)
               (symbol=? sym 'cons))
           (if (not (symbol=? ptr-marker marker))
               (if (and (not (= prev-free -1))
                        (= ptr (+ prev-free (heap-ref (+ prev-free 2)))))
                   (begin
                     (heap-set! (+ prev-free 2) (+ (heap-ref (+ prev-free 2)) 4))
                     (sweep-rec (+ ptr 4) ptr prev-free));coalesce free blocks
                   (begin
                     (make-free ptr 4 prev-free)
                     (sweep-rec (+ ptr 4) ptr ptr)))
               (sweep-rec (+ ptr 4) ptr prev-free))]
          [(symbol=? sym 'free)
           (sweep-rec (+ ptr (heap-ref (+ ptr 2))) ptr ptr)]))
      (if (= prev-free -1)
          (error 'sweep-rec "COMPLETELY out of memory")
          (set! heap-ptr prev-free))))
 
(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))
 
(define (gc:first a)
  (heap-ref (+ 2 a)))
 
(define (gc:rest a)
  (heap-ref (+ 3 a)))
 
(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) f)
      (error 'set-first! "expects address of cons")))
 
(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 3 a) r)
      (error 'set-first! "expects address of cons")))
 
(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))
 
(define (gc:deref a)
  (if (eq? (heap-ref a) 'prim)
      (heap-ref (+ 2 a))
      (error 'gc:deref
             (string-append
              "Expecting 'prim, received '"
              (symbol->string (heap-ref a))))))