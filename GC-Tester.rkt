#lang plai/mutator
(allocator-setup "GC-MarkSweep.rkt" 20)
;(allocator-setup "GC-SaveCopy.rkt" 20)

;kepp since definition?
;(define x 10)
;All parts of this including answer are temp
;(+ 3 7)
;(define y 20)

;(- y 50)
;This is also only temporary



;(cons 5 (cons 4 (cons 3 empty)))
;Permanant list?
;(define l (cons 1 (cons 0 empty)))



;(define z 30)
;(cons 1 empty)
;(define a 40)
;(define b 35)
;(- 222 a)
;(define c 36)



(define (fact x)
  (if (zero? x)
      1
      (* x (fact (sub1 x)))))