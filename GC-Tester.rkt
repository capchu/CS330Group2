#lang plai/mutator
;(allocator-setup "GC-MarkSweep.rkt" 30)
(allocator-setup "GC-StopCopy.rkt" 30)

;kepp since definition?
(define x 10)
;All parts of this including answer are temp
(+ 3 7)
(define y 90)
(- y 50)
(+ 1 1)
;This is also only temporary
;(cons 5 (cons 4 (cons 3 empty)))
;Permanant list?
;(define l (cons 1 (cons 0 empty)))