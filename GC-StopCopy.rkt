#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define off-ptr 'uninitialized-heap-ptr)
(define marker 'no)

(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! off-ptr (/ (heap-size) 2)))
  )

;Taken from your mark sweep
(define (toggle-marker)
  (if (symbol=? marker 'no)
      (set! marker 'yes)
      (set! marker 'no)))

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


;The functions for stop-copy
(define (stop-copy root-set)
  (toggle-marker)
  (mark root-set)
  (copy))

(define (copy)
  (error "made it to copy")
  )

;Starting of the old code (modified to work)
(define (gc:alloc-flat p)
 (begin
   (when (> (+ heap-ptr 2) (/ (heap-size) 2))
     (stop-copy (get-root-set))
     (error 'gc:alloc-flat "out of memory"))
   (heap-set! heap-ptr 'prim)
   (heap-set! (+ 1 heap-ptr) p)
   (set! heap-ptr (+ 2 heap-ptr))
 
   (- heap-ptr 2)))
 
(define (gc:cons f r)
 (begin
   (when (> (+ heap-ptr 3) (/ (heap-size) 2))
     (stop-copy (get-root-set))
     (error 'gc:cons "out of memory"))
   (heap-set! heap-ptr 'cons)
   (heap-set! (+ 1 heap-ptr) f)
   (heap-set! (+ 2 heap-ptr) r)
   (set! heap-ptr (+ 3 heap-ptr))
   (- heap-ptr 3)))
 
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))
 
(define (gc:first a)
 (heap-ref (+ 1 a)))
 
(define (gc:rest a)
 (heap-ref (+ 2 a)))
 
(define (gc:set-first! a f)
 (if (gc:cons? a)
     (heap-set! (+ 1 a) f)
     (error 'set-first! "expects address of cons")))
 
(define (gc:set-rest! a r)
 (heap-set! (+ 2 a) r))
 
(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))
 
(define (gc:deref a)
 (heap-ref (+ 1 a)))

