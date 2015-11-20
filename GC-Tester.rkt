#lang plai/mutator
;(allocator-setup "GC-MarkSweep.rkt" 20)
(allocator-setup "GC-StopCopy.rkt" 60)

;kepp since definition?
(define y 20)
(define l (cons 1 (cons 2 (cons 3 empty))))
;All parts of this including answer are temp
(+ 7 7)
(- y 50)
(define g -100)
(+ 1 y)
(+ 1 y)
(define x -1)
(+ 7 y)
(define q 17)
;(define z -2)
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



;(define (fact x)
;  (if (zero? x)
;      1
;      (* x (fact (sub1 x)))))