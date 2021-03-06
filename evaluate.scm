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
        (cons 'error error)
        (cons 'apply apply)
        (cons 'display display)
        (cons 'newline newline)))

(define (extend-env env var val)
  (cons (cons var val)
        env))

(define (env-get env var)
  (assoc var env))

(define (env-set! env var value)
  (let ((v (env-get env var)))
    (if v
        (set-box! (cdr v) value)
        (error "Unassignable variable" var))))

(define (resulting-value env val)
  val)

(define (evaluate env exp cont)
  (cond ((symbol? exp)
         (let ((def (env-get env exp)))
           (cond ((not def)
                  (error "Undefined variable" exp))
                 ;; NOTE These are introduced by define & lambda.
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
                 (evaluate env
                           (caddr exp)
                           (lambda (env value)
                             (env-set! env (cadr exp) value)
                             (cont env value))))

                ((define)
                 (let* ((name (cadr exp))
                        (value (caddr exp))
                        (extended-env (extend-env env name (box (when #f #f)))))
                   (evaluate extended-env
                             value
                             (lambda (_ executed)
                               ;; NOTE This is using set! so that recursive functions work correctly.
                               (env-set! extended-env name executed)
                               (cont extended-env
                                     (when #f #f))))))

                ((lambda)
                 (let ((formals (cadr exp))
                       (body (cddr exp)))
                   (cont env
                         (lambda args
                           (let loop ((f formals)
                                      (a args)
                                      (e env))
                             (cond ((symbol? f)
                                    (evaluate-list (extend-env e f (box a))
                                                   body
                                                   resulting-value))
                                   ((and (empty? f) (empty? a))
                                    (evaluate-list e body resulting-value))
                                   ((or (empty? f) (empty? a))
                                    (error "Arity mismatch" (length args)))
                                   (else
                                    (loop (cdr f)
                                          (cdr a)
                                          (extend-env e (car f) (box (car a)))))))))))

                (else
                 (begin
                   (define (evaluate-args env-acc acc args cont)
                     (if (null? args)
                         (cont env-acc (reverse acc))
                         (evaluate env (car args)
                                   (lambda (env-acc arg)
                                     (evaluate-args env-acc (cons arg acc) (cdr args) cont)))))
                   (evaluate env
                             (car exp)
                             (lambda (_ op)
                               (if (procedure? op)
                                   (evaluate-args env '() (cdr exp)
                                                  (lambda (_ args)
                                                    (cont env (apply op args))))
                                   (error "Invalid function specified at" (car exp) env))))))))))

(define (evaluate-list env exps cont)
  (define (evaluate-list-aux env-acc acc exps cont)
    (if (null? exps)
        (cont env-acc acc)
        (evaluate env-acc (car exps)
                  (lambda (env-acc value)
                    (evaluate-list-aux env-acc value (cdr exps) cont)))))
  (evaluate-list-aux env (when #f #f) exps cont))
