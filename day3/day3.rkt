#lang racket

(require "../common.rkt")

(define (parse-compartments line) (let* (
    [tr (string-trim line)]
    [len (string-length tr)]
    [half (/ len 2)]
    [first-half (substring tr 0 half)]
    [second-half (substring tr half)]
    [first-compartment (list->set (string->list first-half))]
    [second-compartment (list->set (string->list second-half))])
    (list first-compartment second-compartment)))

(define (find-common-items compartments) (apply set-intersect compartments))

(define (item-priority item) (if (char-upper-case? item) (- (char->integer item) 38) (- (char->integer item) 96)))

(define (calc-items-priority items) (sum-list (map item-priority (set->list items))))

(let* (
    [lines (file->lines "day3/input.txt")]
    [compartments (map parse-compartments lines)]
    [common-items (map find-common-items compartments)]
    [priorities (map calc-items-priority common-items)]
    [total-priority (sum-list priorities)])
    (displayln total-priority))