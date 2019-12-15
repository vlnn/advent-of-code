#lang racket
; solves https://adventofcode.com/2019/day/3

(require megaparsack megaparsack/text
         data/monad
         data/applicative
         control)

(provide test-answer
         current-position
         eval-direction
         get-direction
         add-lists
         mult-lists
         route-wire
         calc-length
         expand-route
         direction/p)

(define test-wiring
  (list
   (list 'R75 'D30 'R83 'U83 'L12 'D49 'R71 'U7 'L72)
   (list 'U62 'R66 'U55 'R34 'D71 'R55 'D58 'R83)))

(define test-answer 159)

(define current-position '(0 0))

(define (add-lists a b)
  (if (empty? a)
      empty
      (cons (+ (car a) (car b))
            (add-lists (cdr a) (cdr b)))))

(define (mult-lists a b)
  (if (empty? a)
      empty
      (cons (* (car a) (car b))
            (mult-lists (cdr a) (cdr b)))))

(define (eval-direction direct)
  (case direct
    ['R '(1 0)]
    ['L '(-1 0)]
    ['U '(0 1)]
    ['D '(0 -1)]
    [else '(0 0)]
    ))

(define direction/p
  (do [x <- letter/p]
      [y <- integer/p]
    (pure (list (string->symbol(string x)) y))
    ))

(define (get-direction x)
   (parse-result! (parse-string direction/p
                 (symbol->string x))))

(define (calc-length x) (apply + x))

(define ( a b (result '()))
  (apply (+) (list (list a b))))

(define (expand-route list count)
  (for*/list ([dirs (make-list count list)]
              [counts (range 1 count 1)])
             (apply add-lists dirs)
            ))

(define (route-wire x)
  (expand-route (car (eval-direction x) (cdr x))))
