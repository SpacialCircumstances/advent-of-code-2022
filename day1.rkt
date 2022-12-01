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

(let* (
    [lines (file->lines "day1/input.txt" #:mode 'text #:line-mode 'linefeed)]
    [split (split-list lines string-empty?)]
    [numbers (map (lambda (g) (map string->number g)) split)]
    [summed (map sum-list numbers)]
    [max-cals (max-list summed)]
    [max-3-cals (max-n-list summed 3)]
    [total-max-3-cals (sum-list max-3-cals)])
    (displayln max-cals)
    (displayln total-max-3-cals))