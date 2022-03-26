;; Macroexpansion reflective tower

(load "evaluate.scm")

(define (make-level env next)
  (vector 'level env next))

(define (level-env l)
  (vector-ref l 1))

(define (set-level-env! l e)
  (vector-set! l 1 e))

(define (level-next l)
  (vector-ref l 2))

(define (do-pure-eval expr level)
  (evaluate (level-env level)
            expr
            (lambda (env result)
              (set-level-env! level env)
              result)))

(define (do-eval expr level)
  (do-pure-eval (do-expand expr level) level))

(define (do-expand expr level)
  (do-pure-eval `(expand ',expr)
                (force (level-next level))))

(define (create-level)
  (make-level
   (create-standard-env)
   (delay
     (let ((next-level (create-level)))
       (set-level-env! next-level
                       (extend-env (level-env next-level)
                                   'eval
                                   (lambda (value)
                                     (do-eval value next-level))))
       ;; NOTE The expander itself doesn't need expansion.
       (do-pure-eval expand-definition
                     next-level)
       next-level))))

;; Bare bones expander.
(define minimal-expander
  '(define expand
     (lambda (e)
       (if (pair? e)
           (if (eq? (car e) 'eval-in-expansion-world)
               (eval (car (cdr e)))
               e)
           e))))

;; A better expander.
(define better-expander
  '(begin
     (define global-macro-env
       (list (cons 'quote
                   (lambda (exp env) exp))
             (cons 'eval-in-expansion-world
                   (lambda (exp env) (eval (car (cdr exp)))))))
     (define env-get
       (lambda (env key)
         ((lambda (def)
            (if def
                (cdr def)
                #f))
          (assoc key env))))
     (define really-expand
       (lambda (exp env)
         (if (pair? exp)
             ((lambda (expander)
                (if expander
                    (expander exp env)
                    ((lambda (a)
                       ;; NOTE Not ideal, as it can expand the cdr of '(foo eval-in-expansion-world bar).
                       (cons a (really-expand (cdr exp) env)))
                     (really-expand (car exp) env))))
              (env-get env (car exp)))
             exp)))
     (define expand
       (lambda (exp)
         (really-expand exp global-macro-env)))
     (define again-izer
       (lambda (expander)
         (lambda (exp env)
           (really-expand (expander exp env) env))))
     (define form-extend
       (lambda (env key fn)
         (cons (cons key fn)
               env)))
     (define install-macro-form!
       (lambda (name expander)
         (set! global-macro-env
               (form-extend global-macro-env name expander))
         #f))))

(define expand-definition better-expander)

;; Repl
(define level-0 (create-level))

(define (repl)
  (let loop ()
    (display ">>> ")
    (display (do-eval (read) level-0))
    (newline)
    (loop)))
