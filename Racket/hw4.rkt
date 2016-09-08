
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))
;2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))
;3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (let ([l (length xs)])
        (if (= l 0)
            (error "list-nth-mod: empty list")
            (car (list-tail xs (remainder n l)))))))
;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))
;5
(define funny-number-stream
  (letrec ([f (lambda (k) (cons (if (= (remainder k 5) 0) (- k) k) (lambda () (f (+ k 1)))))])
    (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec ([even? (lambda (k) (= (remainder k 2) 0))]
           [f (lambda (k) (cons (if (even? k) "dan.jpg" "dog.jpg") (lambda () (f (+ k 1)))))])
    (lambda () (f 0))))
;7
(define (stream-add-zero s)
  (let ([next (s)])
    (lambda () (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))
;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (k) (cons (cons (list-nth-mod xs k) (list-nth-mod ys k)) (lambda () (f (+ k 1)))))])
    (lambda () (f 0))))
;9
(define (vector-assoc v vec)
  (letrec ([assoc-from (lambda (k)
                                (cond [(>= k (vector-length vec)) #f]
                                      [(not (pair? (vector-ref vec k))) (assoc-from (+ k 1))]
                                      [(equal? (car (vector-ref vec k)) v) (vector-ref vec k)]
                                      [#t (assoc-from (+ k 1))]))])
    (assoc-from 0)))
;10
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pos 0])
    (lambda (v)
      (let ([cached-res (vector-assoc v cache)])
        (if cached-res
            cached-res
            (let ([res (assoc v xs)])
              (begin 
                (vector-set! cache pos res)
                (set! pos (remainder (+ pos 1) n))
                res)))))))
;challenge
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([res1 e1])
       (letrec ([loop (lambda ()
                        (let ([res2 e2])
                          (if (< res2 res1)
                              (loop)
                              #t)))])
         (loop)))]))

                                
      