(define-struct meaning expr free-vars)

(define (pure-meaning expr)
  (meaning expr '()))


