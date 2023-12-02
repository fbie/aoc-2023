#lang racket

(define (first-digit chars)
  (ormap (λ (c) (and (char-numeric? c) c)) chars))

(define (last-digit chars)
  (first-digit (reverse chars)))

(define (find-calibration-value str)
  (let [(chars (string->list str))]
    (string->number (string (first-digit chars) (last-digit chars)))))

(define (find-all-calibration-values find-calibration-value str)
  (foldl + 0 (map find-calibration-value (string-split str "\n"))))

(define puzzle-input (file->string "input.txt"))
(find-all-calibration-values find-calibration-value puzzle-input)

(define alphanumeric-num-map
   '(("one"   . 1)
     ("two"   . 2)
     ("three" . 3)
     ("four"  . 4)
     ("five"  . 5)
     ("six"   . 6)
     ("seven" . 7)
     ("eight" . 8)
     ("nine"  . 9)))

(define (calibration-string->integer str)
  (let [(n (assoc str alphanumeric-num-map))]
    (or (and n (cdr n)) (string->number str))))

(define (make-replacement-string replacement-pair)
  (string-append (car replacement-pair) (number->string (cdr replacement-pair)) (car replacement-pair)))

(define (find-calibration-value-2 str)
  (let* [(f (λ (replacement str) (string-replace str (car replacement) (make-replacement-string replacement))))
         (replaced (foldl f str alphanumeric-num-map))]
    (find-calibration-value replaced)))

(find-all-calibration-values find-calibration-value-2 puzzle-input)
