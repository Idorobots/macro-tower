;; Some tests for the macro tower.

(do-eval '(begin (define foo 23) foo) top-level)

(do-eval '(eval-in-expansion-world (begin (define foo 5) foo)) top-level)

(do-eval 'foo top-level)

(do-eval '(eval-in-expansion-world foo) top-level)

(do-eval '(eval-in-expansion-world
           (begin (install-macro-form!
                   'foo
                   (lambda (e m)
                     (list 'equal? (car e) (cadr e))))))
         top-level)

(do-eval '(foo 23) top-level)

(define local-macros
  '(install-macro-form!
    'let-abbreviation ;; (let-abbreviation ((key params ...) expansion ...) scope ...)
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
                                    (cdr (car (cdr e))))))))))))

(define global-macros
  '(install-macro-form!
    'define-abbreviation ;; (define-abbreviation (key params ...) expansion ...)
    (lambda (e m)
      (install-macro-form!
       (car (car (cdr e)))
       (again-izer
        ((lambda (expander)
           (lambda (ee mm)
             (apply expander (cdr ee))))
         (eval (append (list 'lambda (cdr (car (cdr e))))
                       (cdr (cdr e))))))))))

(do-eval `(eval-in-expansion-world
           (begin ,local-macros
                  ,global-macros))
         top-level)

(do-eval `(eval-in-expansion-world
           (eval-in-expansion-world
            (begin ,local-macros
                   ,global-macros)))
         top-level)

(do-eval '(let-abbreviation
           ((progn . body)
            (let-abbreviation
             ((sequence . body)
              (cons 'begin body))
             (sequence (display "Use begin instead of progn!")
                       (cons 'begin body))))
           (progn 5 23))
         top-level)

(do-eval '(define-abbreviation (trace-lambda args . body)
            (list 'lambda args
                  '(display "Args: ")
                  (list 'display (cons 'list args))
                  '(newline)
                  (list '(lambda (result)
                           (display "Result: ")
                           (display result)
                           (newline)
                           result)
                        (cons 'begin body))))
         top-level)

(do-eval '(begin
            (define bar (trace-lambda (x)
                                      (if (eq? x 23)
                                          x
                                          (+ (bar (+ 1 x)) 1))))
            (bar 5))
         top-level)

;; FIXME Requires three loads to work correctly due to how really-expand maintains the same macro-env throughout the expansion:
;; - first one installs the progn macro and fails on the invocation,
;; - second reinstalls the macro and invokes it, installs sequence and fails on its invocation,
;; - third reinstalls both macros and invokes them successfully.
;; This can be fixed by always using the global-macro-env, but then the let-abbreviation version won't work,
;; as it'll "forget" the local definition on the recursive call to really-expand.
;; A proper fix would be to _properly_ run expansion of scopes that introduce new bindings.
(do-eval '(begin (define-abbreviation (progn . body)
                   (define-abbreviation (sequence . body)
                     (cons 'begin body))
                   (sequence (display "Use begin instead of progn!")
                             (cons 'begin body)))
                 (progn 23 5))
         top-level)
