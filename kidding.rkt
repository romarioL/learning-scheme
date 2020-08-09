#lang racket

(define or-or
  (lambda (bool1 bool2 bool3)
    (and (not bool1) (or #t #t))))

(define (person name)
  (define Name name)
  (lambda(method)
    (cond [(eq? method "get")
        (lambda()
          Name)]
        [(eq? method "set")
         (lambda(name)
           (set! Name name))]
        [else (error "Wrong method name")])))

(define (execute-class param)
  (define Class (person param))
  (define get (Class "get"))
  (define set (Class "set"))
  (lambda([name null])
    (if (eq? name null)
        (get)
         (set name))))


(define (person-concrete)
  (define person1 (execute-class "Paula"))
  (lambda([name null])
     (if (equal? name null)
         (person1)
         (person1 name))))
  
  
  

        
        