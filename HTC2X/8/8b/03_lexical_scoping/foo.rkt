;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname foo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (foo x)
  (local [(define (bar y) (+x y))]
    (+ x (bar (* 2 x)))))

(list (foo 2) (foo 3))

;; --> Copy the whole function and assign values to variables coming before the local expression
(list (define (foo 2)
  (local [(define (bar y) (+x y))]
    (+ x (bar (* 2 x)))))
      (define (foo 3)
  (local [(define (bar y) (+x y))]
    (+ x (bar (* 2 x))))))
;; --> Rename local: rename definitions and all references to definitions to something globally unique and move it out of subordinate domain
(define (bar_0 y) (+ x y))

(list (+ 2 (bar_0 (* 2 2)))
      (+ 3 (bar_0 (* 2 3))))

(list (+ 2 (+ 2 (* 2 2)))
      (+ 3 (+ 3 (* 2 3))))

(list (+ 2 (+ 2 4))
      (+ 3 (+ 3 6)))

(list (+ 2 6)
      (+ 3 9))

(list 8
      12)
