(define-syntax define
  (syntax-rules ()
    ((_ sym def)
     (intern-set 'sym def))
    ((_ sym)
     (intern-set 'sym 0))
    ((_)
     (print "Define what?"))
    ))

(define intern-define define)
(define set define)

(define-syntax set!
  (syntax-rules ()
    ((_ sym def)
     (intern-set! 'sym def))
    ((_ sym)
     (intern-set! 'sym 0))))

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

(intern-define make-func
  (lambda (x)
    (lambda () x)))

(define help-msg
  "Bot info is at http://lpaste.net/8511113850400014336, and debug/error info is sent to #sexpbot-debug." )

(define help
  (lambda ()
    (print help-msg)))

(intern-define not
  (lambda (x)
    (if x
      #f
      #t)))

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

(intern-define = eq?)

(intern-define <=
  (lambda (a b)
	(or
	  (< a b)
	  (eq? a b))))

(intern-define >=
  (lambda (a b)
	(or
	  (> a b)
	  (eq? a b))))

(intern-define caar
  (lambda (x)
	(car (car x))))

(intern-define cadr
  (lambda (x)
    (car (cdr x))))

(intern-define caaar
  (lambda (x)
	(car (caar x))))

(intern-define map
  (lambda (fn set)
    (if (null? set)
      '()
      (cons
        (fn (car set))
        (map fn (cdr set))))))

(intern-define member?
  (lambda (obj xs)
    (if (not (null? xs))
      (if (eq? obj (car xs))
        #t
        (member? obj (cdr xs)))
      #f)))
(intern-define ∈ member?)

(intern-define append
  (lambda (xs obj)
    (if (null? xs)
      obj
    (if (null? (cdr xs))
      (cons (car xs) obj)
      (cons (car xs) (append (cdr xs) obj))))))

(intern-define seq
  (lambda (x)
    (+ x 1)))

(intern-define length
  (lambda (ls)
    (if (null? ls)
      0
      (seq (length (cdr ls))))))

(intern-define reply
  (lambda (x)
    (send channel x)))

(intern-define display reply)
(intern-define print   reply)
(intern-define return  
  (lambda (x) x))

(intern-define else
  (lambda (x) x))

(define-syntax iota
  (syntax-rules ()
    ((_ count)
     (gen_range count 0 1))
    ((_ count start)
     (gen_range count start 1))
    ((_ count start step)
     (gen_range count start step))))

(intern-define gen_range
  (lambda (count start step)

    (intern-define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (cons sum
            (iter
              (+ i 1)
              (+ sum step)
              '()))
          '())))

    (iter 0 start '())))

(intern-define old_gen_range
  (lambda (count start step)

    (intern-define iter
      (lambda (i sum xs)
        (if (not (eq? i count))
          (iter
            (+ i 1)
            (+ sum step)
            (append xs (cons sum '())))
          xs)))

    (iter 0 start '())))

(intern-define any
  (lambda (func xs)
    (member? #t (map func xs))))
(intern-define ∃ any)

(intern-define every
  (lambda (func xs)
    (not (member? #f (map func xs)))))
(intern-define ∀ every)

(intern-define assq
  (lambda (key xs)
	(if (or (null? xs)
			(not (list? (car xs))))
	  #f
	  (if (eq? key (caar xs))
		(car (cdr (car xs)))
		(assq key (cdr xs))))))

(intern-define random::seed 859)
(intern-define random::nextint
  (lambda ()
    (intern-define ret random::seed)
    (set! random::seed
      (modulo (+
              (* ret 19739)
               89)
            499979))
    ret))

(intern-define random::int
  (lambda (x)
    (modulo (random::nextint) x)))

(intern-define random::choice
  (lambda (xs)
    (list-ref xs (random::int (length xs)))))

(intern-define rand   random::int)
(intern-define random
  '((seed    random::seed)
    (nextint random::nextint)
    (choice  random::choice)))

(intern-define list-ref
  (lambda (xs n)
    (if (not (null? xs))
      (if (eq? n 0) (car xs)
        (list-ref (cdr xs) (- n 1)))
      #f)))

(intern-define fact
  (lambda (x)
	(if (> x 0)
	  (* x (fact (- x 1)))
	  1)))

(intern-define seq
  (lambda (x)
	(+ x 1)))

(intern-define sum
  (lambda (n f)
    (if (>= n 1)
      (+ (f n)
         (sum (- n 1) f))
      0)))

(intern-define sigma
  (lambda (n k f)
    (if (>= n k)
      (+ (f n)
         (sigma (- n 1) k f))
      0)))
(intern-define ∑ sigma)

(intern-define for-loop
  (lambda (xs sym body)

    (intern-define iter
      (lambda (cur_xs)
        (if (not (null? cur_xs))
          ((lambda ()
            (intern-set sym (car cur_xs))
            (body)
            (iter (cdr cur_xs))))
          '())))

    (iter xs)))

(define-syntax for
  (syntax-rules (in)
    ((_ var in xs body)
     (for-loop xs 'var body))))

(intern-define for-iter
  (lambda (times f)
	(intern-define iter
	  (lambda (count)
		(if (<= count times)
          ((lambda ()
			 (f count)
			 (iter (seq count))
             ))
		  count)))
	(iter 1)))

(intern-define square
  (lambda (x)
    (* x x)))

(intern-define even?
  (lambda (n)
    (eq? (modulo n 2) 0)))

(intern-define odd?
  (lambda (n)
    (not (even? n))))

(define-syntax range
  (syntax-rules (to by :)
    ((_ begin to end by step)
     (iota end begin step))
    ((_ begin to end)
     (iota end begin))
    ((_ end) (iota end))))

(intern-define eightball::answers
  '[ "No. Pleb."
     "Absolutely not."
     "No. Hmm..."
     "there is a possibility."
     "The future is cloudy; I am unsure."
     "Yes, if you give it your all."
     "Absolutely, yes."
   ])

(intern-define eightball
  (function( question ){
    return (random::choice eightball::answers)
  }))

(intern-define json-obj?
  (lambda (obj)
    (eq? (car obj) 'object)))

(intern-define json-array?
  (lambda (obj)
    (eq? (car obj) 'array)))

(intern-define json-object-get
  (lambda (field obj)
    (if (eq? (car obj) 'object)
      (assq field (cadr obj))
      #f)))

(intern-define json-array-get
  (lambda (index obj)
    (if (eq? (car obj) 'array)
      (list-ref (cadr obj) index)
      #f)))

(intern-define make-search
  (lambda (x)
    (lambda (y)
      (lambda (keyword)
        (x (string-append y keyword))))))

(intern-define init-ddg
  (lambda ()
    (set! ddg-json
      ((make-search (:: dev json-url)) "http://api.duckduckgo.com/?format=json&no_html=1&no_redirect=1&skip_disambig=1&q="))))

(intern-define ddg-search
  (lambda (x)
    (intern-define res (ddg-json x))
    (send "#sexpbot-debug" "weather debug for " x ": " res)

    (reply (ddg::get-link res))
    (reply (ddg::get-abstract res))))

(intern-define ddg-link
  (lambda (x)
    (intern-define res (ddg-json x))

    (reply (ddg::get-link res))))

(intern-define ddg::get-link
  (lambda (obj)
    (intern-define results (ddg::get-results obj))
    (if results
      (json-object-get 'FirstURL results)
      (json-object-get 'AbstractURL obj))))

(intern-define ddg::get-abstract
  (lambda (obj)
    (json-object-get 'Abstract obj)))

(intern-define ddg::get-results
  (lambda(obj)
    (json-array-get 0 (json-object-get 'Results obj))))

(intern-define ddg ddg-search)
 
(intern-define list-slice
  (lambda (xs start end)

    (intern-define iter (lambda (ls i)
        (if (null? ls) '()
        (if (< i start)
          (iter (cdr ls) (+ i 1))
        (if (< i end)
          (cons (car ls) (iter (cdr ls) (+ i 1)))
        (else
          '()))))))

  (iter xs 0)))

(define-syntax slice
 (syntax-rules (to :)
  ((_ xs start to end)
   (list-slice xs start end))
  ((_ xs start end)
   (list-slice xs start end))
  ((_ xs start)
   (list-slice xs start 1000000))))

(intern-define hooks::privmsg '())
(intern-define hooks::join '())

(intern-define rules '(
  "No ponies"
  "No fur"
  "Tag all NSFW content as such before posting"
  "Just because you can do something, doesn't mean you should.  Use common sense."
  "No drugs, legal or otherwise."
  "No fun allowed."
  ))

(intern-define rule
  (func(n){
    if (and (> n 0) (< n (+ 1 (length rules))))
    (return (list-ref rules (- n 1)))
    (return "That rule doesn't exist...")}))

(intern-define init-weather
  (lambda()
    (set! weather-json
      ((make-search (:: dev json-url)) "http://api.openweathermap.org/data/2.5/weather?q="))))

(intern-define weather
  (func(loc)
    (intern-define res (json-object-get 'main (weather-json loc)))
    (send "#sexpbot-debug" "weather debug for " loc ": " res)
    (send channel "temperature: " (- (json-object-get 'temp res) 273 ) "°C")
    (send channel "pressure:    " (json-object-get 'pressure res) " bars")
  ))

(intern-define w weather)

(intern-define hooks::privmsg (cons (func(msg){ if (string-contains msg "synack") (print "ACK") #f }) hooks::privmsg))
(intern-define tcp-connect (func(){ print "SYN" }))
(intern-define hooks::privmsg (cons (func(msg){ if (string-contains msg "WHY") (print "but seriously, why") #f }) hooks::privmsg))
(intern-define hooks::privmsg (cons (func(msg){ if (and (string-contains msg "'lelelel I'm a faggot'") (random::choice '[#f #t])) (send channel "fuck you " nick) #f }) hooks::privmsg))
(intern-define hooks::privmsg (cons (func(msg){ if (and (string-contains msg "fuck you") (random::choice '[#f #t])) (send channel "no u") #f }) hooks::privmsg))

(define request
  (lambda (x)
    (send "#sexpbot" nick " from " channel " requested feature: " x )))

(define spam
  (lambda (x)
    (print ">>>#/g/spam #/g/bots #bots #sexpbot")))
