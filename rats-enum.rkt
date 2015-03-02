#lang racket

(require data/enumerate/lib
         math/number-theory
         "set-enumeration.rkt")

(define (which-prime-am-i p)
  (let loop ([p p]
             [count 0])
    (cond [(= 2 p) count]
          [else (loop (prev-prime p) (add1 count))])))

(define prime/e
  (map/e nth-prime which-prime-am-i natural/e #:contract prime?))

;; Enum [a], Enum b -> Enum [(a, b)]

(define (uniq-list/e e)
  (map/e (λ (x) (reverse (set->list x)))
         list->set
         (setof/e e)
         #:contract (listof (enum-contract e))))

(define factor-choices/e
  (cons/de [lft
            (uniq-list/e prime/e)]
           [rght (lft)
                 (if (empty? lft)
                     (except/e (uniq-list/e prime/e) '())
                     (uniq-list/e
                      (apply except/e
                             prime/e
                             lft)))]))

(define rat-expr/e
  (map/e 
   cdr
   (λ (p)
     (define lfacts (map car (car p)))
     (define rfacts (map car (cdr p)))
     (cons (cons lfacts rfacts)
           p))
  (cons/de 
   [factors factor-choices/e]
   [expr (factors)
    ;; firsts : nonempty-listof nat
    ;; rest   : listof nat
    (begin
      (define firsts (car factors))
      (define rest   (cdr factors))
      (define (with-nat xs)
        (for/list ([x (in-list xs)])
          (list/e (fin/e x) (nat+/e 1))))
      (cons/e
       (apply list/e (with-nat firsts))
       (apply list/e (with-nat rest))))])
  #:contract (λ (_) #t)))

(define (rat-expr->rat expr)
  (/ (defactorize (cdr expr))
     (defactorize (car expr))))

(define (rat->rat-expr r)
  (define n (numerator r))
  (define d (denominator r))
  (cons (factorize d)
        (factorize n)))

(define pos-non-1-rat/e
  (map/e rat-expr->rat
         rat->rat-expr
         rat-expr/e
         #:contract (and/c (λ (x) (not (= x 1))) positive? rational?)))

(define pos-rat/e
  (or/e (fin/e 1)
        pos-non-1-rat/e))
(define rat/e
  (or/e (fin/e 0)
        pos-rat/e
        (map/e - 
               -
               pos-rat/e
               #:contract (and/c rational?
                                 negative?))))