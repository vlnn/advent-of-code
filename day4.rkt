#lang racket
; solution for https://adventofcode.com/2019/day/4

(require rackunit)

(define (rule1? x)
  (and (> x 99999) (< x 1000000)))
(check-eq? (rule1? 20) #f)
(check-eq? (rule1? 20000000) #f)
(check-eq? (rule1? 943456) #t)

(define (rule2? x)
  (and (> x 254031) (< x 789861)))
(check-eq? (rule2? 111111) #f)
(check-eq? (rule2? 999999) #f)
(check-eq? (rule2? 555555) #t)

(require srfi/1 srfi/26)
(define (digits->list num (base 10))
  (unfold-right zero? (cut remainder <> base) (cut quotient <> base) num))
(check-equal? (digits->list 123456) '(1 2 3 4 5 6))

(define (digits x) (digits->list x))

(define (rule3? x)
   (not(equal? (digits x) (remove-duplicates (digits x)))))
(check-equal? (rule3? 12345) #f)
(check-equal? (rule3? 12223) #t)

(define (rule4? x)
  (equal? (sort (digits x) <) (digits x)))
(check-equal? (rule4? 123225) #f)

(define (combine-rules x)
  (and (rule1? x)
       (rule2? x)
       (rule3? x)
       (rule4? x)))

(display "The first answer:")
(length (filter combine-rules (range 100000 999999 1)))

(define (split-by lst n)
  (if (> (length lst) 2)
       (cons (take lst n) (split-by (cdr lst) n))
       '() ))

(check-equal? (split-by '(1 2 3) 2) '((1 2)))
(check-equal? (split-by '(1 2 3) 3) '((1 2 3)))
(check-equal? (split-by '(1 2 3 4) 2) '((1 2) (2 3) (3 4)))

(define (rule5? x)
  (split-by (digits x) 2)
  #t)

(check-equal? (rule5? 112233) #t)
; (check-equal? (rule5? 123444) #f)
(check-equal? (rule5? 111223) #t)

(define (combine-rules-expanded x)
  (and (combine-rules x)
       (rule5? x)))

(display "The second answer:")
(length (filter combine-rules-expanded (range 100000 999999 1)))
