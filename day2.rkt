#lang racket
(require rackunit)

(define pc 0)
(define program 0)

(module+ test
  (define test-program1 '(1 0 0 0 99))
  (define test-answer1 '(2 0 0 0 99))
  (define test-program2 '(2 3 0 3 99))
  (define test-answer2 '(2 3 0 6 99))
  (define test-program3 '(2 4 4 5 99 0))
  (define test-answer3 '(2 4 4 5 99 9801))
  (define test-program4 '(1 1 1 4 99 5 6 0 99))
  (define test-answer4 '(30 1 1 4 2 5 6 0 99)))
(define the-program1 '(1 12 2 3 1 1 2 3 1 3 4 3 1 5 0 3 2 6 1 19 1 19 5 23 2 9 23 27 1 5 27 31 1 5 31 35 1 35 13 39 1 39 9 43 1 5 43 47 1 47 6 51 1 51 13 55 1 55 9 59 1 59 13 63 2 63 13 67 1 67 10 71 1 71 6 75 2 10 75 79 2 10 79 83 1 5 83 87 2 6 87 91 1 91 6 95 1 95 13 99 2 99 13 103 1 103 9 107 1 10 107 111 2 111 13 115 1 10 115 119 1 10 119 123 2 13 123 127 2 6 127 131 1 13 131 135 1 135 2 139 1 139 6 0 99 2 0 14 0))

(define (reload-program source)
  (set! program source))

(define (reset source)
  (set! pc 0)
  (reload-program source))

(module+ test
  (check-equal? (begin (reset test-program1) pc) 0))

(define (next)
  (set! pc (+ pc 4)))

(module+ test
  (check-equal? (begin (reset test-program1) (next) pc) 4))

(define (get-current-opcode source)
  (begin
    (list-ref source pc)))
(module+ test
  (check-equal? (begin (reset test-program1) (get-current-opcode test-program1)) 1))

(define (interpret-add source)
  (list-set source (list-ref source (+ pc 3))
            (+ (list-ref source (list-ref source (+ pc 2)))
               (list-ref source (list-ref source (+ pc 1))))))
(module+ test
  (check-equal? (interpret-add test-program1) test-answer1))

(define (interpret-mult source)
  (list-set source (list-ref source (+ pc 3))
            (* (list-ref source (list-ref source (+ pc 2)))
               (list-ref source (list-ref source (+ pc 1))))))

(module+ test
  (check-equal? (interpret-mult test-program2) test-answer2))

(define (interpret source)
  (begin
    (case (get-current-opcode source)
      [(1) (interpret (begin (set! source (interpret-add source)) (next) source))]
      [(2) (interpret (begin (set! source (interpret-mult source)) (next) source))]
      [(99) source]
      )
    )
  )

(module+ test
  (check-equal? (begin
                  (reset test-program3)
                  (interpret test-program3)) test-answer3)
  (check-equal? (begin
                  (reset test-program4)
                  (interpret test-program4)) test-answer4))

(module+ test
  (check-equal? (list-ref (interpret the-program1) 0) 4090689))
