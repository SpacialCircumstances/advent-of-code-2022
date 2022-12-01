#lang racket

(define (string-empty? s) (= (string-length s) 0))

(define (replace-head lst replacer) (cons (replacer (car lst)) (cdr lst)))

(define (split-list lst pred?) (foldl (lambda (next st) (if (pred? next) (cons '() st) (replace-head st (lambda (h) (cons next h))))) (list '()) lst))

(define (sum-list lst) (foldl + 0 lst))

(define (max-list lst) (foldl max 0 lst))

(let* (
    [lines (file->lines "day1/input.txt" #:mode 'text #:line-mode 'linefeed)]
    [split (split-list lines string-empty?)]
    [numbers (map (lambda (g) (map string->number g)) split)]
    [summed (map sum-list numbers)]
    [max-cals (max-list summed)])
    (print max-cals))