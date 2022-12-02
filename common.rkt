#lang racket

(define (string-empty? s) (= (string-length s) 0))

(define (replace-head lst replacer) (cons (replacer (car lst)) (cdr lst)))

(define (split-list lst pred?) (foldl (lambda (next st) (if (pred? next) (cons '() st) (replace-head st (lambda (h) (cons next h))))) (list '()) lst))

(define (sum-list lst) (foldl + 0 lst))

(define (max-list lst) (foldl max 0 lst))

(define (zero-list n) (build-list n (lambda (_) 0)))

(define (max-n-list lst n) (foldl 
    (lambda (next st) 
        (sort (cons (max next (car st)) (cdr st)) <)) (zero-list n) lst))

(provide (all-defined-out))