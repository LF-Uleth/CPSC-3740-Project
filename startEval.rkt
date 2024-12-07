#lang racket

(define (startEval expr env)
  (cond
    ;; Handle constants and variables
    [(number? expr) expr]
    [(symbol? expr) (lookup expr env)]
    [(and (list? expr) (eq? (first expr) 'quote)) (second expr)]
    
    ;; Handle arithmetic operators
    [(and (list? expr) (eq? (first expr) '+)) 
     (+ (startEval (second expr) env) 
        (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '-)) 
     (- (startEval (second expr) env) 
        (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '*)) 
     (* (startEval (second expr) env) 
        (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '/)) 
     (/ (startEval (second expr) env) 
        (startEval (third expr) env))]
    
    ;; Handle relational operators
    [(and (list? expr) (eq? (first expr) 'equal?)) 
     (equal? (startEval (second expr) env) 
             (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '=)) 
     (= (startEval (second expr) env) 
        (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '<=)) 
     (<= (startEval (second expr) env) 
         (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '<)) 
     (< (startEval (second expr) env) 
        (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '>=)) 
     (>= (startEval (second expr) env) 
         (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) '>)) 
     (> (startEval (second expr) env) 
        (startEval (third expr) env))]
    
    ;; Handle lists
    [(and (list? expr) (eq? (first expr) 'car)) 
     (car (startEval (second expr) env))]
    [(and (list? expr) (eq? (first expr) 'cdr)) 
     (cdr (startEval (second expr) env))]
    [(and (list? expr) (eq? (first expr) 'cons)) 
     (cons (startEval (second expr) env) 
           (startEval (third expr) env))]
    [(and (list? expr) (eq? (first expr) 'pair?)) 
     (pair? (startEval (second expr) env))]
    
    ;; Handle conditional
    [(and (list? expr) (eq? (first expr) 'if)) 
     (if (startEval (second expr) env) 
         (startEval (third expr) env) 
         (startEval (fourth expr) env))]
    
    ;; Handle lambda
    [(and (list? expr) (eq? (first expr) 'lambda)) 
     (make-closure (second expr) (third expr) env)]
    
    ;; Handle function application
    [(list? expr) 
     (let* ([func (startEval (first expr) env)]
            [args (map (λ (e) (startEval e env)) (rest expr))])
       (apply-closure func args))]
    
    ;; Handle let and letrec
    [(and (list? expr) (eq? (first expr) 'let)) 
     (let* ([bindings (second expr)]
            [new-env (extend-env env 
                                 (map first bindings) 
                                 (map (λ (b) (startEval (second b) env)) bindings))])
       (startEval (third expr) new-env))]
    [(and (list? expr) (eq? (first expr) 'letrec)) 
     (let* ([bindings (second expr)]
            [new-env (extend-env-rec env bindings)])
       (startEval (third expr) new-env))]))

;; Helper functions

;; Look up a variable in the environment
(define (lookup var env)
  (if (null? env)
      (error "Variable not found: " var)
      (let ([binding (assoc var env)])
        (if binding (cdr binding) (lookup var (cdr env))))))

;; Extend the environment with new bindings
(define (extend-env env vars vals)
  (append (map cons vars vals) env))

;; Extend the environment with recursive bindings
(define (extend-env-rec env bindings)
  (let ([rec-env (cons '() env)])
    (for-each (λ (b) (set-cdr! rec-env 
                               (cons (cons (first b) 
                                           (startEval (second b) rec-env)) 
                                     (cdr rec-env)))) 
              bindings)
    rec-env))

;; Create a closure
(define (make-closure params body env)
  (list 'closure params body env))

;; Apply a closure to arguments
(define (apply-closure closure args)
  (if (not (eq? (first closure) 'closure))
      (error "Not a closure: " closure)
      (let ([params (second closure)]
            [body (third closure)]
            [env (fourth closure)])
        (startEval body (extend-env env params args)))))
