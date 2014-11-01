(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))))

(define-syntax set!
  (syntax-rules ()
    ((_ sym def)
     (intern-set! 'sym def))
    ((_ sym)
     (intern-set! 'sym 0))))

; this will do until proper elipsis expansion is implemented...
(define-syntax begin
  (syntax-rules ()
	((_ expr moar)
	 ((lambda () expr (begin moar))))
	((_ expr)
	 expr)
	((_) #f)))

(define make-func
  (lambda (x)
    (lambda () x)))

(define help
  (lambda ()
    (send channel "Basically, I'm a lisp interpreter. (proper documentation coming soon.)")
    (send channel "Expressions are prefixed with ':', and automatically wrapped in parenthesis,")
    (send channel "so '(display (+ 1 (* 2 3)))' is expressed as ':display (+ 1 (* 2 3))'.")
    (send channel "To display output, there's (display msg) and (send channel msg ...)")
    ))

(define not
  (lambda (x)
    (if x
      #f
      #t)))

; TODO: Fix symbol clashes between procedures and macro expansion
(define-syntax or
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       #t
       (if _op2_
         #t
         #f)))))

(define-syntax and
  (syntax-rules ()
    ((_ _op1_ _op2_)
     (if _op1_
       (if _op2_
         #t
         #f)
       #f))))

(define = eq?)

(define <=
  (lambda (a b)
	(or
	  (< a b)
	  (eq? a b))))

(define >=
  (lambda (a b)
	(or
	  (> a b)
	  (eq? a b))))

(define caar
  (lambda (x)
	(car (car x))))

(define caaar
  (lambda (x)
	(car (caar x))))

(define print
  (lambda (x)
    (if (list? x)
      (pprint-list x)
      (display x))
	(newline)))

(define map
  (lambda (func set)
    (if (null? set)
      '()
      (cons
        (func (car set))
        (map func (cdr set))))))

(define member?
  (lambda (obj xs)
    (if (not (null? xs))
      (if (eq? obj (car xs))
        #t
        (member? obj (cdr xs)))
      #f)))
(define ∈ member?)

(define append
  (lambda (xs obj)
    (if (null? xs)
      obj
    (if (null? (cdr xs))
      (cons (car xs) obj)
      (cons (car xs) (append (cdr xs) obj))))))

(define seq
  (lambda (x)
    (+ x 1)))

(define length
  (lambda (ls)
    (if (null? ls)
      0
      (seq (length (cdr ls))))))

(define display
  (lambda (x)
    x))
; see http://srfi.schemers.org/srfi-1/srfi-1.html#TheProcedures

(define-syntax iota
  (syntax-rules ()
    ((_ count)
     (gen_range count 0 1))
    ((_ count start)
     (gen_range count start 1))
    ((_ count start step)
     (gen_range count start step))))

(define gen_range
  (lambda (count start step)

    (define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (cons sum
            (iter
              (+ i 1)
              (+ sum step)
              '()))
          '())))

    (iter 0 start '())))

(define old_gen_range
  (lambda (count start step)

    (define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (iter
            (+ i 1)
            (+ sum step)
            (append xs (cons sum '())))
          xs)))

    (iter 0 start '())))

(define any
  (lambda (func xs)
    (member? #t (map func xs))))
(define ∃ any)

(define every
  (lambda (func xs)
    (not (member? #f (map func xs)))))
(define ∀ every)

(define assq
  (lambda (key xs)
	(if (or (null? xs)
			(not (list? (car xs))))
	  #f
	  (if (eq? key (caar xs))
		(car (cdr (car xs)))
		(assq key (cdr xs))))))

(define random::seed 859)
(define random::nextint
  (lambda ()
    (define ret random::seed)
    (set! random::seed
      (modulo (+
              (* ret 19739)
               89)
            499979))
    ret))

(define random::int
  (lambda (x)
    (modulo (random::nextint) x)))

(define random::choice
  (lambda (xs)
    (list-ref xs (random::int (length xs)))))

(define rand   random::int)
(define random
  '((seed    random::seed)
    (nextint random::nextint)
    (choice  random::choice)))

(define list-ref (lambda (xs n) (if (not (null? xs)) (if (eq? n 0) (car xs) (list-ref (cdr xs) (- n 1))) #f)))
