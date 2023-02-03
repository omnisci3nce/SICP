#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs_if x)
  (if (< x 0)
      (- x)
      (x)
      ))

; Ex. 1.3
(define (sum-two-larger-squares a b c)
  (cond ((and (>= a b) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (> c b))  (sum-of-squares a c))
        (else (sum-of-squares b c))))

; Newton's method for calculating square roots
