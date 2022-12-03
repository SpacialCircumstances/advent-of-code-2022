#lang racket

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

(let* (
    [lines (file->lines "day3/input.txt")]
    [compartments (map parse-compartments lines)]
    [common-items (map find-common-items compartments)])
    (displayln common-items))
