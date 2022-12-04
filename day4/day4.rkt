#lang racket

(require srfi/13)

(require "../common.rkt")

(define (parse-range r) (let* (
    [split-pos (string-contains r "-")]
    [first-part (substring r 0 split-pos)]
    [second-part (substring r (+ split-pos 1))]
    [range-start (string->number first-part)]
    [range-end (string->number second-part)]
    [rang (inclusive-range range-start range-end)])
    (list->set rang)))

(define (parse-ranges line) (let* (
    [t (string-trim line)]
    [pairs (string-split t ",")]
    [ranges (map parse-range pairs)])
    ranges))

(define (overlap-complete? lst) (let* (
    [fst (first lst)]
    [scnd (second lst)])
    (or (subset? fst scnd) (subset? scnd fst))))

(define (overlap? lst) (not (set-empty? (apply set-intersect lst))))

(define (solve-puzzle overlap) (let* (
    [lines (file->lines "day4/input.txt")]
    [pairs-of-ranges (map parse-ranges lines)]
    [overlapping (filter overlap pairs-of-ranges)])
    (displayln (length overlapping))))

(solve-puzzle overlap-complete?)

(solve-puzzle overlap?)