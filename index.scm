(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (lookup-variable-value exp env) 1)

(define (variable? exp) (symbol? exp))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (no-operands? ops) (null? ops))

(define (list-of-values exp env) 
  (if (no-operands? exps) 
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env) 
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp) ))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ;formal parameters
                 (cddr exp)))) ;body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body) 
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (caddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure 
                         (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) 
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else 
          (error "Unknown expression type: EVAL" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

; primitive-procedure: ('primitive car)

(define apply-in-underlying-scheme apply)

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args) 
  (apply-in-underlying-scheme 
   (primitive-implementation proc) args))

(define (apply procedure arguments) 
  (cond 
    ((primitive-procedure? procedure) 
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-enviroment 
         (procedure-parameters procedure)
         arguments
         procedure-environment procedure)))
    (else 
      (error 
        "Unknown procedure type: APPLY" exp))))

(eval 'l 0)

(eval (5 0) 0)

(x 2)
(+ x 5)

(eval 5 0)


(factorial 5)
