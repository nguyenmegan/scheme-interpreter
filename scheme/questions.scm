(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (cons-all first rests)
  (if (null? rests) nil
  (cons (cons first (car rests)) (cons-all first (cdr rests))))
)

(define (zip pairs)
  (define firsts (map (lambda (pair) (car pair)) pairs))
  (define seconds (map (lambda (pair) (cadr pair)) pairs))
  (list firsts seconds)
)

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define a 0)
  (define (enumerate-helper a s)
          (if (null? s) nil
          (cons (list a (car s)) (enumerate-helper (+ a 1) (cdr s)))))
  (enumerate-helper a s))
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond ((or (< total 0) (equal? total 0)) nil)
        ((null? denoms) nil)
        ((> (car denoms) total) (list-change total (cdr denoms)))
        ((equal? (car denoms) total) (cons (list (car denoms)) (list-change total (cdr denoms))))
        (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                    (list-change total (cdr denoms))))))
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
          (append (list form params) (analyze body))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
          (define zipped (zip values))
          (define parameters (car zipped))
          (define pvalues (map analyze (cadr zipped)))
          (define fn (append (list 'lambda parameters) (analyze body)))
          (cons fn pvalues)
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr)
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21
