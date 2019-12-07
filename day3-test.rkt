#lang racket

(require rackunit "day3.rkt" megaparsack megaparsack/text)

(check-equal? test-answer 159)

(check-equal? (eval-direction 'R) '(1 0))
(check-equal? (eval-direction 'L) '(-1 0))
(check-equal? (eval-direction 'U) '(0 1))
(check-equal? (eval-direction 'D) '(0 -1))

(check-equal? (add-lists '(1 3) '(-2 3)) '(-1 6))

(check-equal? (route-wire 'R2) (list '(1 0) '(2 0)))

(check-equal? (calc-length '(1 2 3 4)) 10)

(check-equal? (get-direction 'L393) (list 'L 393))
