#lang racket

(require "../common.rkt")

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