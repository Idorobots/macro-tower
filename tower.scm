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
        ((and (pair? expr)
              (> (length expr) 1))
         (case (car expr)
           ((lambda)
            ;; NOTE Filter out bould variables.
            (filter (lambda (s)
                      (not (member s (cadr expr))))
                    (collect-symbols (cddr expr))))
           ((define)
            ;; NOTE Filter out bould variables.
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
            (lambda (env result)
              result)))

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
    (run (meaning-expr mn)
         level)))

(define (do-eval expr level)
  (do-pure-eval (do-expand expr level) level))

(define (do-expand expr level)
  (do-pure-eval `(expand ',expr)
                (force (level-next level))))

(define expand-definition-meaning (pure-meaning expand-definition))

(define (create-level)
  (make-level
   (create-standard-env)
   (delay
     (let ((next-level (create-level)))
       (set-level-env! next-level
                       (extend-env (level-env next-level)
                                   (append '(eval expand)
                                           (meaning-free-vars expand-definition-meaning))))
       (env-set! (level-env next-level)
                 'eval
                 (lambda (value)
                   (do-eval value next-level)))
       (run (meaning-expr expand-definition-meaning)
            next-level)
       next-level))))

;; Bare bones expander.
(define expand-definition
  '(define expand
     (lambda (e)
       (if (pair? e)
           (if (eq? (car e) 'eval-in-expansion-world)
               (eval (car (cdr e)))
               e)
           e))))

;; Repl
(define level-0 (create-level))

(define (repl)
  (let loop ()
    (display ">>> ")
    (display (do-eval (read) level-0))
    (newline)
    (loop)))

;; A better expander.
(define better-expander
  '(eval-in-expansion-world
    (begin
      (define expand
        (lambda (e)
          (really-expand e global-expand-env)))
      (define macro-env
        (lambda (e)
          (if (pair? e)
              (if (eq? (car e) 'eval-in-expansion-world)
                  (lambda (e m) (eval (car (cdr e))))
                  (if (eq? (car e) 'quote)
                      (lambda (e m) e)
                      #f))
              #f)))
      (define global-expand-env
        (lambda (e)
          (macro-env e)))
      (define really-expand
        (lambda (e m)
          ((lambda (expander)
             (if expander
                 (expander e m)
                 (default-expand e m)))
           (m e))))
      (define default-expand
        (lambda (e m)
          (if (pair? e)
              ((lambda (a)
                 (cons a (really-expand (cdr e) m)))
               (really-expand (car e) m))
              e)))
      (define again-izer
        (lambda (expander)
          (lambda (e m)
            (really-expand (expander e m) m))))
      (define form-extend
        (lambda (m key fn)
          (lambda (ee)
            (if (pair? ee)
                (if (eq? (car ee) key)
                    fn
                    (m ee))
                (m ee)))))
      (define install-macro-form!
        (lambda (name expander)
          (set! macro-env
                (form-extend macro-env name expander))
          #f))
      ;; FIXME Needs to be here because otherwise it's excluded from free vars.
      install-macro-form!)))

(define local-macros
  '(eval-in-expansion-world
    (install-macro-form!
     'let-abbreviation
     (lambda (e m)
       (really-expand
        (cons 'begin (cdr (cdr e)))
        (form-extend m
                     (car (car (car (cdr e))))
                     (again-izer
                      ((lambda (expander)
                         (lambda (ee mm)
                           (apply expander (cdr ee))))
                       (eval (append (list 'lambda (cdr (car (car (cdr e)))))
                                     (cdr (car (cdr e)))))))))))))
