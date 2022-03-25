;; Macroexpansion reflective tower

(define (make-meaning expr free-vars)
  (vector 'meaning expr free-vars))

(define (meaning-expr m)
  (vector-ref m 1))

(define (meaning-free-vars m)
  (vector-ref m 2))

(define (pure-meaning expr)
  ;; FIXME Do this properly.
  (make-meaning expr
                (filter (lambda (s)
                          (not (member s '(quote unquote quasiquote define if lambda begin set!))))
                        (uniq (collect-symbols expr)))))

(define (collect-symbols expr)
  (cond ((symbol? expr)
         (list expr))
        ((and (list? expr)
              (> (length expr) 1))
         (case (car expr)
           ((lambda)
            ;; NOTE Filter out bould variables.
            (let ((formals (collect-symbols (cadr expr))))
              (filter (lambda (s)
                        (not (member s formals)))
                      (collect-symbols (cddr expr)))))
           ((define)
            ;; NOTE Filter out bould name.
            (filter (lambda (s)
                      (not (eq? s (cadr expr))))
                    (collect-symbols (cddr expr))))
           (else
            (append (collect-symbols (car expr))
                    (collect-symbols (cdr expr))))))
        ((pair? expr)
         (append (collect-symbols (car expr))
                 (collect-symbols (cdr expr))))
        (else
         '())))

(define (uniq lst)
  (let loop ((acc '())
             (lst lst))
    (cond ((empty? lst)
           (reverse acc))
          ((member (car lst) acc)
           (loop acc (cdr lst)))
          (else
           (loop (cons (car lst)
                       acc)
                 (cdr lst))))))

(load "evaluate.scm")

(define (env-set! env var value)
  (let ((v (env-get env var)))
    (if v
        (set-box! (cdr v) value)
        (error var (format "~a not found in env" var)))))

(define (run expr level)
  (evaluate (level-env level)
            expr
            resulting-value))

(define (make-level env next)
  (vector 'level env next))

(define (level-env l)
  (vector-ref l 1))

(define (set-level-env! l e)
  (vector-set! l 1 e))

(define (level-next l)
  (vector-ref l 2))

(define (do-pure-eval expr level)
  (let ((mn (pure-meaning expr)))
    (set-level-env! level
                    (extend-env (level-env level)
                                (meaning-free-vars mn)))
    (run (meaning-expr mn) level)))

(define (do-eval expr level)
  (do-pure-eval (do-expand expr level) level))

(define (do-expand expr level)
  (do-pure-eval `(expand ',expr)
                (force (level-next level))))

(define (expand-definition-meaning)
  (pure-meaning expand-definition))

(define (create-level)
  (let ((expander (expand-definition-meaning)))
    (make-level
     (create-standard-env)
     (delay
       (let ((next-level (create-level)))
         (set-level-env! next-level
                         (extend-env (level-env next-level)
                                     (append '(eval expand)
                                             (meaning-free-vars expander))))
         (env-set! (level-env next-level)
                   'eval
                   (lambda (value)
                     (do-eval value next-level)))
         (run (meaning-expr expander)
              next-level)
         next-level)))))

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
     (define expand
       (lambda (exp)
         ;; NOTE Car expansion might have modified the env, so for the cdr we reload it from global-macro-env.

         (really-expand exp global-macro-env)))
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
         #f))
     ;; FIXME Needs to be here because otherwise it's excluded from free vars.
     install-macro-form!
     global-expand-env))

(define expand-definition better-expander)

;; Repl
(define level-0 (create-level))

(define (repl)
  (let loop ()
    (display ">>> ")
    (display (do-eval (read) level-0))
    (newline)
    (loop)))
