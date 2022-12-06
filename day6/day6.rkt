#lang racket

(require "../common.rkt")

(define (find-foldl pred init lst) (call/cc (lambda (return) 
    (foldl (lambda (el st) (let (
        [res (pred el st)])
        (if (car res) (return (cdr res)) (cdr res)))) init lst))))

(define (find-marker marker-len) 
    (lambda (next st) (let* (
        [pos (car st)]
        [last4 (cdr st)]
        [is-marker (= marker-len (set-count (list->set last4)))])
        (if is-marker (cons #t (cons pos last4)) (cons #f (cons (+ pos 1) (append (drop last4 1) (list next))))))))

(define (find-marker-in-text text marker-len) (let* (
    [chars (string->list text)]
    [first (take chars marker-len)]
    [rest (drop chars marker-len)]
    [finder (find-marker marker-len)]
    [marker (find-foldl finder (cons marker-len first) rest)])
    (car marker)))

(let* (
    [input (file->string "day6/input.txt")]
    [sol1 (find-marker-in-text input 4)]
    [sol2 (find-marker-in-text input 14)])
    (displayln sol1)
    (displayln sol2))
    
