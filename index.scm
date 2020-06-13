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

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
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



