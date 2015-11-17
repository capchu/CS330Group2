#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define marker 'no)
 
(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (make-free 0 (heap-size) -1)
    (printf (symbol->string (heap-ref 0)))))

(define (make-free ptr size next)
  (begin
    (heap-set! ptr 'free)
    (heap-set! (+ 1 ptr) marker)
    (heap-set! (+ 2 ptr) size)
    (heap-set! (+ 3 ptr) next)))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 2) (heap-size))
      (mark-sweep (get-root-set))
      (error 'gc:alloc-flat "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ heap-ptr 1) marker)
    (heap-set! (+ heap-ptr 2) p)
    (set! heap-ptr (+ heap-ptr 4))
    (- heap-ptr 4)))

(define (toggle-marker)
  (if (symbol=? marker 'no)
      (set! marker 'yes)
      (set! marker 'no)))

(define (mark-sweep root-set)
  (toggle-marker)
  (mark root-set)
  (sweep))

(define (mark roots)
  (when (not (empty? roots))
         (mark-rec (read-root (first roots)))
         (mark (rest roots))))

(define (mark-rec obj-ptr)
  ;mark obj-ptr
  (printf (number->string obj-ptr))
  (heap-set! (+ obj-ptr 1) marker)
  (when (and (symbol? (heap-ref obj-ptr))
             (symbol=? 'cons (heap-ref obj-ptr)))
    (mark-rec (+ obj-ptr 1))
    (mark-rec (+ obj-ptr 2))))

(define (sweep)
  (printf (string-append "\nSweeping non-" (symbol->string marker)))
  (sweep-rec 0 -1 -1))

(define (sweep-rec ptr prev prev-free)
  (when (<= (+ ptr 4) (heap-size))
    (let ([sym (heap-ref ptr)]
          [ptr-marker (heap-ref (+ ptr 1))])
      (printf (number->string ptr))
      (cond
        [(or (symbol=? sym 'prim)
             (symbol=? sym 'cons))
         (if (not (symbol=? ptr-marker marker))
             (if (and (not (= prev -1))
                      (symbol=? (heap-ref prev) 'free))
                 (begin
                   (heap-set! (+ prev 2) (+ (heap-ref (+ prev 2)) 4))
                   (sweep-rec (+ ptr 4) ptr prev));coalesce free blocks
                 (begin
                   (make-free ptr 4 prev-free)
                   (sweep-rec (+ ptr 4) ptr ptr)));make new free block and link it to the previous free block
             (sweep-rec (+ ptr 4) ptr prev-free))]
        [(symbol=? sym 'free)
         (sweep-rec (+ ptr (heap-ref (+ ptr 2))) ptr ptr)]))))
 
(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (mark-sweep (get-root-set f r))
      #|everything that is reachable in root-set, in 'f' and in 'r' => (get-root-set f r)|#
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) marker)
    (heap-set! (+ 2 heap-ptr) f)
    (heap-set! (+ 3 heap-ptr) r)
    (set! heap-ptr (+ 4 heap-ptr))
    (- heap-ptr 4)))
 
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
  (heap-ref (+ 2 a)))