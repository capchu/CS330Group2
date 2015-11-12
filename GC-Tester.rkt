#lang plai/mutator
;(allocator-setup "GC-MarkSweep.rkt" 20)
(allocator-setup "GC-SaveCopy.rkt" 20)

;kepp since definition?
(define x 10)
;All parts of this including answer are temp
(+ 3 4)
;This is also only temporary
(cons 5 (cons 4 (cons 3 empty)))
;Permanant list?
(define l (cons 1 (cons 0 empty)))