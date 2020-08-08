#lang racket

( define multiply
   (lambda (x mult)
     (* x mult)))
( define multiply-three-times
   (lambda (x  y multiply)
     (* x ( multiply y y))))

(define  is-pair
  (lambda (pair)
    (pair? pair)))


(define is-equal
  (lambda (string1 string2)
    (equal? string1 string2)))

(define multiply-with-if
  (lambda (x mult_func)
    (if (> x 5)
        (mult_func x 5)
        (mult_func x 2))))

(define double-multiply-with-if
  (lambda (x y if_mult mult)
    (if (< x y)
        (* x (if_mult y mult))
        (* y (if_mult x mult)))))

(define factorial 
  (lambda (n)
    (if ( = n 0)
        1
      (* n (factorial( - n 1))))))

(define  universal-apply
  (lambda (list func)
    (map func list)))

(define  without-first
  (lambda (list)
    (cdr list)))

(define middle-of-list
  (lambda (list)
    (car (cdr list))))

(define middle-of-any-list
  (lambda (list)
    (if (= 3 (length list))
        (car (cdr list))
        ( middle-of-any-list (cdr list)))))

(define (any-list-any-doing list fn)
    (fn list))


(define (class name)
  (define value name)
  (lambda (method)
    (cond
      [ (equal? method "get")
        ( lambda ()
           value)]
      [ (equal? method "set")
        (lambda (v)
          (set! value  v))]
      [ else  ( error  "This  is not a allowed method")])))

(define (counter n)
  (define number n)
  (lambda (method)
    (define (add-number n)
      (set! number ( + number n)))
    (define (desc-number n)
      (set! number ( - number n)))
    (define (mult-number n)
      (set! number (* number n)))
    (define (div-number n)
      (set! number (/ number n)))
    (define (get-method)
       number)
    (cond [ (equal?  method "add")
            add-number
            ]
          [(equal? method "decr")
           desc-number ]
          [(equal? method "mult")
           mult-number]
          [( equal? method "div")
           div-number]
          [ (equal? method "get")
            get-method ]
          [ else (error "Wrong operation")])))

(define (create-counter n counting)
  (define count (counting n))
  (lambda ()
    (define (get-counter)
      (count "get"))
    (define (add)
      (count "add"))
    (define (sub)
      (count "decr"))
    (define (mult)
      (count "mult"))
    (define (div)
      (count "div"))
    (list get-counter add mult div)))

(define (execute-method n list-method index)
  (define method (list-ref list-method index))
    ((method) n))

(define count (create-counter 5 counter))
(define methods (count))
(execute-method 5 methods 3)


 
      

 
             