# The evolution of a Scheme programmer

> https://erkin.party/blog/200715/evolution/

;; Studying Scheme in university or reading The Little Schemer
```scheme
(define factorial
  (lambda (n)
    (cond ((= n 0) 1)
          (else (* (factorial (- n 1)) n)))))
```
;; Newbie programmer, enjoys recursion and simplicity in life
```scheme
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
```
;; Hates functional programming and resents being taught Scheme in university
```scheme
(define (return x) x)
(define (factorial n)
  (define result 0)
  (do ((counter 1 (add1 counter))
       (product 1 (* counter product)))
      ((> counter (add1 n)))
    (set! result product))
  (return result))
```

;; SICP reader, appreciates helper procedures and understands the power of tail calls
```scheme
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (add1 counter)
                 max-count)))
(define (factorial n)
  (fact-iter 1 1 n))
```

;; Astute SICP reader, doesn't want to clutter the namespace
```scheme
(define (factorial n)
  (define (fact-aux product counter)
    (if (> counter n)
        product
        (fact-aux (* counter product)
                  (add1 counter))))
  (fact-aux 1 1 n))
```

;; Adept programmer, getting a hang of the idioms
```scheme
(define (factorial n)
  (let loop ((product 1) (counter 1))
    (if (> counter n)
        product
        (loop (* counter product)
              (add1 counter)))))
```

;; Came from a different functional language, sees patterns everywhere
```scheme
(define factorial
  (match-lambda
   (0 1)
   (n (* n (factorial (sub1 n))))))
```

;; Really likes lists, carries around their own 300 line file of helper procedures
```scheme
(define (factorial n)
  (apply * (build-list n add1)))
```

;; Discovering different functional approaches and came across SRFI-1
```scheme
(define (factorial n)
  (fold * 1 (iota 1 (add1 n))))
```

;; Eagerly believes that the ideal solution can always be found in an SRFI
```scheme
(define (factorial n)
  (product-ec (: i 1 (add1 n)) i))
```

;; Spoilt by Racket's macros
```scheme
(define (factorial n)
  (for/product ((i (in-range 1 (add1 n)))) i))
```

;; Heard about the Y-combinator and went down a rabbithole
```scheme
(define factorial
  ((?? (f)
     ((?? (g)
        (f (?? (x)
             ((g g) x))))
      (?? (g)
        (f (?? (x)
             ((g g) x))))))
   (?? (f)
     (?? (n)
       (if (zero? n)
           1
           (* n (f (sub1 n))))))))
```

;; Spent an idle weekend learning about Peano numerals and combinatory logic
;; Will forget about it all in a week and all this will become unreadable in two days
```scheme
(define ?? (?? (f) (f f)))
(define S (?? (f) (?? (g) (?? (x) ((f x) (g x))))))
(define K (?? (x) (?? (y) x)))
(define ?? (?? (x) (x (S K))))
(define I (?? ??))
(define Z (?? (f) (?? (?? (x) (f (v) ((?? x) v))))))
(define true (?? (a) (?? (b) a)))
(define false (?? (a) (?? (b) b)))
(define if (?? (p) (?? (a) (?? (b) ((p a) b)))))
(define and (?? (p) (?? (q) ((p q) p))))
(define zero (?? (f) (?? (x) x)))
(define add1 (?? (n) (?? (f) (?? (x) (f ((n f) x))))))
(define sub1 (?? (n) (?? (f) (?? (x) (((n (?? (g) (?? (h) (h (g f))))) (?? (u) x)) I)))))
(define + (?? (m) (?? (n) (n (add1 m)))))
(define * (?? (m) (?? (n) (?? (f) (m (n f))))))
(define zero? (?? (n) ((n (?? (x) false)) true)))
(define <= (?? (m) (?? (n) (zero? ((- m) n)))))
(define = (?? (m) (?? (n) ((and ((<= m) n)) ((<= n) m)))))
(define factorial
  (?? (x)
    ((Z (?? (f)
          (?? (n)
            (((if (zero? n)) (add1 zero)) ((* n) (f (sub1 n))))))) x))) 
```

;; Gone mad with power after discovering macros, likes -funroll-loops
```scheme
(define-syntax factorial
  (lambda (stx)
    (syntax-case stx ()
      ((_ 0) #'1)
      ((_ n) #`(* n (factorial
                     #,(sub1 (syntax->datum #'n))))))))
```

;; Feels enlightened after learning about CPS but doesn't know what to do with this information
```scheme
(define (fact-cont n k)
  (if (= n 1)
      (k 1)
      (fact-cont (sub1 n)
                 (lambda (x)
                   (k (* n x))))))
(define (factorial n)
  (fact-cont n values))
```

;; Overjoyed to see first-class continuations in Scheme, still doesn't know what to do with them
```scheme
(define (factorial n)
  (letrec ((f (lambda (n k)
                (if (= n 1)
                    (k 1)
                    (f (sub1 n)
                       (lambda (ret)
                         (k (* n ret))))))))
    (call-with-current-continuation
     (lambda (k)
       (f n k)))))
```

;; Attempted to make their CPS procedures simpler, failed
```scheme
(define (factorial n)
  (define retry #f)
  (if (= n 1)
      (call-with-current-continuation
       (lambda (k)
         (set! retry k) 1))
      (* n (factorial (sub1 n)))))
```

;; Concerned about wasted performance, really likes the word 'amortised'
```scheme
(define (memoise proc)
  (let ((table (make-hashtable equal-hash equal?)))
    (lambda args
      (hashtable-ref! table args (lambda () (apply proc args))))))
(define (factorial n)
  (define fact-iter
    (memoise
     (lambda (product counter)
       (if (> counter n)
           product
           (fact-iter (* counter product)
                      (add1 counter))))))
  (fact-iter 1 1))
```

;; Lazy programmer
```scheme
(define-coroutine-generator (factorials)
  (let loop ((product 1) (counter 1))
    (yield product)
    (loop (* product counter) (add1 counter))))
(define (factorial n)
  (car (generator->list (gindex factorials (generator n)))))
```

;; Lazier programmer
```scheme
(define factorials (stream-scan * 1 (stream-from 1)))
(define (factorial n)
  (stream-ref factorials n))
```

;; Laziest programmer
```scheme
(define factorial
  (dynamic-require
   'math/number-theory
   'factorial
   (lambda ()
     (lambda (n)
       (error 'factorial "Cannot import library: ~a" 'math/number-theory)))))
```