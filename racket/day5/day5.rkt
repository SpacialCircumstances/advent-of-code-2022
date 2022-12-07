#lang racket

(require "../common.rkt")

(define (is-empty-line? line) (string-empty? (string-trim line)))

(define (extract-block bl) (substring bl 1 2))

(define (add-block block stack new-stacks) (if (is-empty-line? block) (cons stack new-stacks) (cons (cons (extract-block block) stack) new-stacks)))

(define (add-blocks blocks-line stacks) (reverse (foldl add-block (list) blocks-line stacks)))

(define (parse-stacks lines) (let* (
  [chunked (map (lambda (l) (map (lambda (s) (string-trim s #:repeat? #t)) (string-chunk l 4))) lines)]
  [stack-numbers (map (lambda (n) (string->number (string-trim n))) (first chunked))]
  [stack-blocks (rest chunked)]
  [stack-count (max-list stack-numbers)]
  [stacks (make-list stack-count (list))])
  (foldl add-blocks stacks stack-blocks)))

(define (parse-commands lines) (let* (
  [r #px"move (\\d+) from (\\d+) to (\\d+)"]
  [matched (map (lambda (l) (regexp-match r l)) lines)])
  (reverse (map (lambda (m) (map string->number (cdr m))) matched))))

(define (exec-command move-order) (lambda (cmd stacks) (let*-values (
  [(count) (first cmd)]
  [(from) (- (second cmd) 1)]
  [(to) (- (third cmd) 1)]
  [(moving new-from-stack) (split-at (list-ref stacks from) count)]
  [(new-to-stack) (append (move-order moving) (list-ref stacks to))])
  (list-set (list-set stacks from new-from-stack) to new-to-stack))))

(let* (
  [lines (file->lines "day5/input.txt")]
  [sections (split-list lines is-empty-line?)]
  [cmds-section (first sections)]
  [stacks-section (second sections)]
  [stacks (parse-stacks stacks-section)]
  [commands (parse-commands cmds-section)]
  [end-stacks1 (foldl (exec-command reverse) stacks commands)]
  [end-stacks2 (foldl (exec-command (lambda (s) s)) stacks commands)])
  (displayln (string-join (map first end-stacks1) ""))
  (displayln (string-join (map first end-stacks2) "")))