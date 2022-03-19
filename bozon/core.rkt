#lang slideshow
;; #lang racket/base
;; (require memo)
;; 
;; (define/memoize (fib n)
;;   (if (< n 2)
;;       1
;;       (+ (fib (sub1 n)) (fib (- n 2)))                 
