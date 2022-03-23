;; A simple meta-circular evaluator for scheme without macros.

(define (create-standard-env)
  (list (cons 'not not)
        (cons 'null? null?)
        (cons 'pair? pair?)
        (cons 'list? list?)
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'list list)
        (cons 'length length)
        (cons 'append append)
        (cons 'caar caar)
        (cons 'cdar cdar)
        (cons 'cadr cadr)
        (cons 'caddr caddr)
        (cons 'cddr cddr)
        (cons 'cdddr cdddr)
        (cons 'eqv? eqv?)
        (cons 'eq? eq?)
        (cons 'equal? equal?)
        (cons 'memv memv)
        (cons 'memq memq)
        (cons 'member member)
        (cons 'assv assv)
        (cons 'assq assq)
        (cons 'assoc assoc)
        (cons 'map map)
        (cons 'filter filter)
        (cons 'foldl foldl)
        (cons 'vector vector)
        (cons 'vector? vector?)
        (cons 'list->vector list->vector)
        (cons 'vector->list vector->list)
        (cons 'string->symbol string->symbol)
        (cons 'symbol->string symbol->string)
        (cons 'string-append string-append)
        (cons 'number->string number->string)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= =)
        (cons '< <)
        (cons '> >)
        (cons '<= <=)
        (cons '>= >=)
        (cons 'error error)))

(define (extend-env env var value)
  (let ((v (env-get env var)))
    (if (and v (box? (cdr v)))
        ;; FIXME Please no mutation.
        (begin (set-box! (cdr v) value)
               env)
        (cons (cons var (box value))
              env))))

(define (env-get env var)
  (assoc var env))

(define (resulting-value env val)
  val)

(define (evaluate env exp cont)
  (cond ((symbol? exp)
         (let ((def (env-get env exp)))
           (cond ((not def)
                  (error "Undefined variable" exp))
                 ;; NOTE These are introduced by extend-env.
                 ((box? (cdr def))
                  (cont env (unbox (cdr def))))
                 (else
                  (cont env (cdr def))))))

        ((not (pair? exp))
         (cont env exp))

        (else (case (car exp)
                ((quote)
                 (cont env (cadr exp)))

                ((if)
                 (evaluate env (cadr exp)
                           (lambda (_ condition)
                             (evaluate env
                                       (cond (condition
                                              (caddr exp))
                                             ((= (length exp) 4)
                                              (cadddr exp))
                                             (else
                                              (when #f #f)))
                                       cont))))

                ((begin)
                 (evaluate-list env (cdr exp) cont))

                ((set!)
                 (let ((def (env-get env (cadr exp))))
                   (cond ((not def)
                          (error "Undefined variable" (cadr exp)))
                         ((not (box? (cdr def)))
                          (error "Unassignable variable" (cadr exp)))
                         (else
                          (evaluate env (caddr exp)
                                    (lambda (env value)
                                      (set-box! (cdr def) value)
                                      (cont env value)))))))

                ((define)
                 (let* ((name (cadr exp))
                        (value (caddr exp))
                        (extended-env (extend-env env name (when #f #f))))
                   (evaluate extended-env
                             value
                             (lambda (_ executed)
                               (set-box! (cdr (env-get extended-env name))
                                         executed)
                               (cont extended-env
                                     (when #f #f))))))

                ((lambda)
                 (let ((formals (cadr exp))
                       (body (cddr exp)))
                   (cont env
                         (lambda args
                           (if (equal? (length args)
                                       (length formals))
                               (let ((extended-env (foldl (lambda (binding env)
                                                            (extend-env env
                                                                        (car binding)
                                                                        (cdr binding)))
                                                          env
                                                          (map cons
                                                               formals
                                                               args))))
                                 (evaluate-list extended-env
                                                body
                                                resulting-value))
                               (error "Arity mismatch" (length args)))))))

                (else
                 (begin
                   (define (evaluate-args env-acc acc args cont)
                     (if (null? args)
                         (cont env-acc (reverse acc))
                         (evaluate env (car args)
                                   (lambda (env-acc arg)
                                     (evaluate-args env-acc (cons arg acc) (cdr args) cont)))))
                   (evaluate env (car exp)
                             (lambda (_ op)
                               (evaluate-args env '() (cdr exp)
                                              (lambda (_ args)
                                                (cont env (apply op args))))))))))))

(define (evaluate-list env exps cont)
  (define (evaluate-list-aux env-acc acc exps cont)
    (if (null? exps)
        (cont env-acc acc)
        (evaluate env-acc (car exps)
                  (lambda (env-acc value)
                    (evaluate-list-aux env-acc value (cdr exps) cont)))))
  (evaluate-list-aux env (when #f #f) exps cont))
