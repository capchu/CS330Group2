#lang plai/mutator
(define heap-size 1000)
(allocator-setup "GC-MarkSweep.rkt" heap-size)
;(allocator-setup "GC-SaveCopy.rkt" heap-size)