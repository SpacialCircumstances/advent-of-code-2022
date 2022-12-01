#lang racket

(define (string-empty? s) (= (string-length s) 0))

(define (replace-head lst replacer) (cons (replacer (car lst)) (cdr lst)))

(define (split-list lst pred?) (foldl (lambda (next st) (if (pred? next) (cons '() st) (replace-head st (lambda (h) (cons next h))))) (list '()) lst))

(let* (
    [lines (file->lines "day1/input.txt" #:mode 'text #:line-mode 'linefeed)]
    [split (split-list lines string-empty?)])
    (print split))