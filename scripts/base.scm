;; Base library
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

(define-syntax ::
  (syntax-rules ()
    ((_ ns var)
     (extern 'ns 'var))))

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

(define cadr
  (lambda (x)
    (car (cdr x))))

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

(define reply
  (lambda (x)
    (send channel x)))

(define display reply)
(define print   reply)
(define return  reply)

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

(define list-ref
  (lambda (xs n)
    (if (not (null? xs))
      (if (eq? n 0) (car xs)
        (list-ref (cdr xs) (- n 1)))
      #f)))

;; Math stuff
; Recursive factorial function
(define fact
  (lambda (x)
	(if (> x 0)
	  (* x (fact (- x 1)))
	  1)))

; Sequence function
(define seq
  (lambda (x)
	(+ x 1)))

; Calculate the sum of a function with inputs from 1 to n.
(define sum
  (lambda (n f)
    (if (>= n 1)
      (+ (f n)
         (sum (- n 1) f))
      0)))

; Calculate the sum of a function with inputs from k to n.
(define sigma
  (lambda (n k f)
    (if (>= n k)
      (+ (f n)
         (sigma (- n 1) k f))
      0)))
(define ∑ sigma)

(define for-loop
  (lambda (xs sym body)

    (define iter
      (lambda (cur_xs)
        (if (not (null? cur_xs))
          ((lambda ()
            (intern-set sym (car cur_xs))
            (body)
            (iter (cdr cur_xs))))
          '())))

    (iter xs)))

; repeatedly perform a function for "times", using recursion
(define-syntax for
  (syntax-rules (in)
    ((_ var in xs body)
     (for-loop xs 'var body))))

; repeatedly perform a function for "times", using iteration
(define for-iter
  (lambda (times f)
	(define iter
	  (lambda (count)
		(if (<= count times)
		  ;(begin
          ((lambda ()
			 (f count)
			 (iter (seq count))
             ))
		  count)))
	(iter 1)))

; Square a number
(define square
  (lambda (x)
    (* x x)))

(define even?
  (lambda (n)
    (eq? (modulo n 2) 0)))

(define odd?
  (lambda (n)
    (not (even? n))))

(define-syntax range
  (syntax-rules (to by :)
    ((_ begin to end by step)
     (iota end begin step))
    ((_ begin to end)
     (iota end begin))
    ((_ end) (iota end))))

(define eightball::answers
  '[ "No. Pleb."
     "Absolutely not."
     "No. Hmm..."
     "there is a possibility."
     "The future is cloudy; I am unsure."
     "Yes, if you give it your all."
     "Absolutely, yes."
   ])

(define eightball
  (function( question ){
    return (random::choice eightball::answers)
  }))

(define json-obj?
  (lambda (obj)
    (eq? (car obj) 'object)))

(define json-array?
  (lambda (obj)
    (eq? (car obj) 'array)))

; JSON manipulation stuff
(define json-object-get
  (lambda (field obj)
    (if (eq? (car obj) 'object)
      (assq field (cadr obj))
      #f)))

(define json-array-get
  (lambda (index obj)
    (if (eq? (car obj) 'array)
      (list-ref (cadr obj) index)
      #f)))

(define make-search
  (lambda (x)
    (lambda (y)
      (lambda (keyword)
        (x (string-append y keyword))))))

(define init-ddg
  (lambda ()
    (set! ddg-json
      ((make-search (:: dev json-url)) "http://api.duckduckgo.com/?format=json&no_html=1&no_redirect=1&skip_disambig=1&q="))))
      ;((make-search (:: dev json-url)) "http://api.duckduckgo.com/?format=json&no_html=1&no_redirect=1&q="))))

(define ddg-search
  (lambda (x)
    (define res (ddg-json x))

    (reply (ddg::get-link res))
    (reply (ddg::get-abstract res))))

(define ddg::get-link
  (lambda (obj)
    (define results (ddg::get-results obj))
    (if results
      (json-object-get 'FirstURL results)
      (json-object-get 'AbstractURL obj))))

(define ddg::get-abstract
  (lambda (obj)
    (json-object-get 'Abstract obj)))

(define ddg::get-results
  (lambda(obj)
    (json-array-get 0 (json-object-get 'Results obj))))

(define ddg ddg-search)
