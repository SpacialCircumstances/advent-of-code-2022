#lang racket

(require "../common.rkt")

(define (shape-to-score shape) (case shape 
    [("X") 1] 
    [("Y") 2] 
    [("Z") 3]))

(define (outcome-to-score outcome) (case outcome
    [(loss) 0]
    [(draw) 3]
    [(win) 6]))

(define (outcome opponent me) (case (list opponent me)
    [(("A" "X")) 'draw]
    [(("A" "Y")) 'win]
    [(("A" "Z")) 'loss]
    [(("B" "X")) 'loss]
    [(("B" "Y")) 'draw]
    [(("B" "Z")) 'win]
    [(("C" "X")) 'win]
    [(("C" "Y")) 'loss]
    [(("C" "Z")) 'draw]))

(define (score-turn turn) (let* (
    [opponent (car turn)]
    [me (cadr turn)]
    [outc (outcome opponent me)]
    [sh-sc (shape-to-score me)]
    [outc-sc (outcome-to-score outc)])
    (+ sh-sc outc-sc)))

(define (calculate-score turns) (foldl (lambda (turn st) (+ st (score-turn turn))) 0 turns))

(let* (
    [lines (file->lines "day2/input.txt")]
    [turns (map string-split lines)]
    [score (calculate-score turns)])
    (print score))