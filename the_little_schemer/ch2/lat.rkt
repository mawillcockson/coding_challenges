#lang racket
(define (atom? maybe-atom)
  (and
   (not (pair? maybe-atom))
   (not (null? maybe-atom))))
(define (lat? lat)
  (or (null? lat)
      (and
       (atom? (car lat))
       (lat? (cdr lat)))))

(println (eq? (lat? '(atom)) #t))
(println (eq? (lat? '()) #t))
(println (eq? (lat? '(a b c)) #t))
(println (eq? (lat? '((a b) c d)) #f))
(println (eq? (lat? '(() b c)) #f))
(println (eq? (lat? '(a b ())) #f))
(println (eq? (lat? '(a b (c d))) #f))