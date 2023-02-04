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
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Ex. 1.6
; infinite recursive loop because a functional call is applicative order so it evals all arguments first
; and operator and then applies the operator to the args. this will call sqrt-iter in the else-clase again
; causing infinite recursion

; Ex. 1.7
; good-enough? when squaring small numbers the squared number becomes smaller, so we very quickly
; hit 0.001 of precision because the difference might be big below that decimal place,
; but because the values are below 0.0001 e.g. 0.00003 vs 0.00008 we exit earlier
; they're both very different but within 0.001 of each other so the algorithm says its good enough
; For large numbers it won't terminate because the limited precision means that when we get to a very big
; large number, we cant represent a next number within 0.001 of the current guess (without using more
; bits to represent it), so our guess does not improve causing an infinite loop

(define (good-enough-delta? old-guess guess)
  (< (abs (- guess old-guess)) 0.001))

(define (sqrt-iter2 prev-guess guess x)
  (if (good-enough-delta? prev-guess guess)
      guess
      (sqrt-iter2 guess (improve guess x) x)))

; sqrt2 kick starts the recursive function with an old and a new guess
(define (sqrt2 x)
  (sqrt-iter2 1.0 (improve 1.0 x) x))

; sqrt  0.0003 -> 0.03438350032699598
; sqrt2 0.0003 -> 0.01732538223327823
; mr google    -> 0.01732050807 