#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define off-ptr 'uninitialized-heap-ptr)
(define change-ptr 'uninitialized-heap-ptr)
(define marker 'no)

(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! off-ptr (/ (heap-size) 2))
    (set! change-ptr (/ (heap-size) 2))
  )
  )

;Taken from your mark-sweep
(define (move roots)
  (when (not (empty? roots))
    (move-rec (read-root (first roots)))
    (move-root (first roots))
    (move (rest roots))
    )
  )

(define (move-rec obj-ptr)
  (printf (symbol->string (heap-ref obj-ptr)))
  ;(printf " ")
  ;(printf (number->string (heap-ref (+ obj-ptr 1))))
  (printf "\n")
  (cond
    [(procedure? (heap-ref (+ obj-ptr 1)))
     (heap-set! off-ptr (heap-ref obj-ptr))
     (heap-set! (+ off-ptr 1) (heap-ref (+ obj-ptr 1)))
     (set! off-ptr (+ off-ptr 2))
     (move (procedure-roots (heap-ref (+ obj-ptr 1))))
     (- off-ptr 2)
     ]
    [(symbol=? 'prim (heap-ref obj-ptr))
     (heap-set! off-ptr (heap-ref obj-ptr))
     (heap-set! (+ off-ptr 1) (heap-ref (+ obj-ptr 1)))
     (set! off-ptr (+ off-ptr 2))
     (- off-ptr 2)
     ]
    [(symbol=? 'cons (heap-ref obj-ptr))
     (heap-set! off-ptr (heap-ref obj-ptr))
     (heap-set! (+ off-ptr 1) (heap-ref (+ obj-ptr 1)))
     (heap-set! (+ off-ptr 2) (heap-ref (+ obj-ptr 2)))
     ;(move-rec (heap-ref (+ obj-ptr 1)))
     ;(move-rec  (heap-ref (+ obj-ptr 2)))
     (set! off-ptr (+ off-ptr 3))
     (- off-ptr 3)
     ]
    )
  )

(define (move-root obj-root)
  (let ([obj-ptr (read-root obj-root)])
    (cond
      [(symbol=? 'prim (heap-ref obj-ptr))
       (set-root! obj-root (- off-ptr 2))
       ]
      [(symbol=? 'cons (heap-ref obj-ptr))
       (set-root! obj-root (- off-ptr 3))
       ]
      )
    )
  )

(define (scan start-loc)
  (printf (number->string off-ptr))
  (printf "\n")
  (when (not (= start-loc off-ptr))
    (printf (symbol->string (heap-ref start-loc)))
    (printf "\n")
    (if (symbol=? 'cons (heap-ref start-loc))
        [begin
         (heap-set! (+ start-loc 1) (move-rec (heap-ref (+ start-loc 1))))
         (heap-set! (+ start-loc 2) (move-rec (heap-ref (+ start-loc 2))))
         (scan (+ start-loc 3))
         ]
         (scan (+ start-loc 2))  
        )
    )
  )

;The functions for stop-copy
(define (stop-copy root-set)
  (move root-set)
  (printf (string-append "pre-heap-ptr: " (number->string heap-ptr)))
  (printf "\n")
  (printf (string-append "pre-off-ptr: " (number->string off-ptr)))
  (printf "\n")
  (if (<= heap-ptr (/ (heap-size) 2))
      [begin
        (printf "In Less Than")
        (printf "\n")
        (scan (/ (heap-size) 2))
        (set! heap-ptr off-ptr)
        (set! off-ptr 0)
        (set! change-ptr (heap-size))
        ]
      [begin
        (printf "In Greater Than")
        (printf "\n")
        (scan 0)
        (set! heap-ptr off-ptr)
        (set! off-ptr (/ (heap-size) 2))
        (set! change-ptr (/ (heap-size) 2))
        ]
      )
  (printf (string-append "post-heap-ptr: " (number->string heap-ptr)))
  (printf "\n")
  (printf (string-append "post-off-ptr: " (number->string off-ptr)))
  (printf "\n")
  (printf (string-append "post-change-ptr: " (number->string change-ptr)))
  (printf "\n")
  )

;Starting of the old code (modified to work)
(define (gc:alloc-flat p)
 (begin
   (when (> (+ heap-ptr 2) change-ptr)
     (stop-copy (get-root-set))
     (when (> (+ heap-ptr 2) change-ptr)
       (error 'gc:alloc-flat "actually out of memory - too much saved")
       )
     )
   (heap-set! heap-ptr 'prim)
   (heap-set! (+ heap-ptr 1) p)
   (set! heap-ptr (+ 2 heap-ptr))
   (- heap-ptr 2)
   ))
 
(define (gc:cons f r)
 (begin
   (when (> (+ heap-ptr 3) (/ (heap-size) 2))
     (stop-copy (get-root-set f r))
     (when (> (+ heap-ptr 3) change-ptr)
       (error 'gc:cons "actually out of memory - too much saved")
       )
     )
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

