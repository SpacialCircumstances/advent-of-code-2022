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

(define (find-shape-for-outcome opponent outcome) (case (list opponent outcome)
    [(("A" win)) "Y"]
    [(("A" loss)) "Z"]
    [(("A" draw)) "X"]
    [(("B" win)) "Z"]
    [(("B" loss)) "X"]
    [(("B" draw)) "Y"]
    [(("C" win)) "X"]
    [(("C" loss)) "Y"]
    [(("C" draw)) "Z"]))

(define (des-res-to-outcome des-res) (case des-res 
    [("X") 'loss]
    [("Y") 'draw]
    [("Z") 'win]))

(define (score-turn2 turn) (let* (
    [opponent (car turn)]
    [des-res (cadr turn)]
    [outcome (des-res-to-outcome des-res)]
    [req-shape (find-shape-for-outcome opponent outcome)]
    [outc-sc (outcome-to-score outcome)]
    [sh-sc (shape-to-score req-shape)])
    (+ sh-sc outc-sc)
))

(define (calculate-score turns sc-turn) (foldl (lambda (turn st) (+ st (sc-turn turn))) 0 turns))

(let* (
    [lines (file->lines "day2/input.txt")]
    [turns (map string-split lines)]
    [score1 (calculate-score turns score-turn)]
    [score2 (calculate-score turns score-turn2)])
    (displayln score1)
    (displayln score2))