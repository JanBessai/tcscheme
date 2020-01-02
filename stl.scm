;; Types
(define (atomic? A) (symbol? A))

(define (source A) (car (cdr A)))
(define (target A) (car (cdr (cdr A))))
(define (arrow? A)
  (and (list? A)
       (equal? 3 (length A))
       (equal? '-> (car A))))

(define (type? A) (or (arrow? A) (atomic? A)))

;; Terms
(define (variable? M) (symbol? M))

(define (bound-var b) (car b))
(define (ascribed-type b) (car (cdr b)))
(define (binder-pair? b)
  (and (list? b)
       (equal? 2 (length b))
       (and (variable? (bound-var b))
            (type? (ascribed-type b)))))

(define (functional M) (car M))
(define (argument M) (car (cdr M)))
(define (application? M)
  (and (list? M)
       (equal? 2 (length M))))

(define (binder M) (car (cdr M)))
(define (body M) (car (cdr (cdr M))))
(define (lambda? M)
  (and (list? M)
       (equal? 3 (length M))
       (equal? 'lambda (car M))
       (binder-pair? (binder M))))

(define (type-in report-error todo Gamma x)
  (let ((res (assoc x Gamma)))
    (if res 
        (todo (car (cdr res)))
        (report-error (list 'unbound-variable x)))))

(define (synthesize report-error todo Gamma M)
  (cond 
    ((variable? M)
     (type-in report-error todo Gamma M))
    ((lambda? M)
     (synthesize report-error 
                 (lambda (T) (todo (list '-> (ascribed-type (binder M)) T)))
                 (cons (binder M) Gamma) 
                 (body M)))
    ((application? M)
     (synthesize 
       report-error
       (lambda (ST) 
          (if (arrow? ST)
              (check report-error 
                     (lambda (ok) (todo (target ST))) 
                     Gamma 
                     (argument M)
                     (source ST))
              (synthesize report-error
                          (lambda (S)
                            (report-error 
                              (list 'expected (list '-> S 'SomeType) 'got ST)))
                          Gamma
                          (argument M))))
       Gamma
       (functional M)))
    (report-error (list 'unknown-statement M))))

(define (check report-error todo Gamma M A)
  (cond 
    ((variable? M)
     (type-in 
       report-error
       (lambda (T)
          (if (equal? A T) 
              (todo M)
              (report-error (list 'expected A 'got T))))
       Gamma
       M))
    ((lambda? M)
     (let ((S (ascribed-type (binder M))))
       (if (and (arrow? A) (equal? (source A) S))
         (check report-error  
                (lambda (body) (todo (list 'lambda (binder M) body)))
                (cons (binder M) Gamma)
                (body M)
                (target A))
         (report-error (list 'expected A 'got (list '-> S 'SomeType))))))
    ((application? M)
     (let ((functional (functional M)))
       (synthesize
         report-error
         (lambda (ST)
            (if (arrow? ST)
                (let ((T (target ST)))
                  (check report-error
                         (lambda (argument)
                           (if (equal? A T)
                             (todo (list functional argument))
                             (report-error (list 'expected A 'got T))))
                         Gamma
                         (argument M)
                         (source ST)))
                (synthesize report-error
                            (lambda (S) 
                              (report-error 
                                (list 'expected (list '-> S A) 'got ST)))
                            Gamma
                  (argument M))))
         Gamma
         functional)))
    (report-error (list 'unknown-statement M))))

(define (erase M)
  (cond ((variable? M) M)
        ((application? M) 
         (list (erase (functional M))
               (erase (argument M))))
        ((lambda? M)
         (list 'lambda
               (bound-var (binder M))
               (erase (body M))))
        (list 'unknown-statement M)))

(define (id x) x)

(display 
  (list 
    (check id erase '((x A)) `x 'A)
    (check id erase '((x B)) `x 'A)
    (check id erase '((x A)) `y 'A)
    (check id erase '() `(lambda (x A) x) '(-> A A))
    (check id erase '() `(lambda (x (-> A A)) x) '(-> (-> A A) (-> A A)))
    (check id erase '() `(lambda (x A) x) '(-> A (-> A A)))
    (check id erase '() `(lambda (x A) (lambda (y B) x)) '(-> A (-> B A)))
    (check id erase '() `(lambda (x A) (lambda (y B) y)) '(-> A (-> B B)))
    (check id erase '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> A B))
    (check id erase '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> B B))
    (check id erase '((f X)) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> A B))
    (check id erase '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (x f))) f) '(-> A B))
  ))

