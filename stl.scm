(require-extension srfi-1)

(define (type? A)
  (cond 
    ((list? A)
     (and (equal? 3 (length A))
          (equal? '-> (car A))
          (every type? (cdr A))))
    (symbol? A)))

(define (binder-pair? kv)
  (and (equal? 2 (length kv))
       (let ((bound-var (car kv))
             (ascribed-type (car (cdr kv))))
         (and (symbol? bound-var)
              (type? ascribed-type)))))

(define (with-target-for-source report-error todo S A)
  (if (and (list? A)
           (equal? 3 (length A))
           (equal? '-> (car A))
           (equal? S (car (cdr A))))
      (todo (car (cdr (cdr A))))
      (report-error (list 'expected (list '-> S 'Y) 'got A))))

(define (synthesize report-error todo Gamma M)
  (cond 
    ((symbol? M)
     (let 
       ((binding (find (lambda (kv) (equal? (car kv) M)) Gamma)))
       (if (not binding) 
           (report-error (list 'unbound-variable M))
           (todo (car (cdr binding))))))
    ((and (equal? 3 (length M))
          (equal? 'lambda (car M))
          (binder-pair? (car (cdr M))))
     (let* ((binder (car (cdr M)))
            (bound-type (car (cdr binder)))
            (body (car (cdr (cdr M)))))
           (synthesize
             report-error
             (lambda (T) (todo (list '-> bound-type T)))
             (cons binder Gamma)
             body)))
    ((equal? 2 (length M))
     (synthesize 
       report-error
       (lambda (ST) 
          (if (and (list? ST)
                   (equal? 3 (length ST))
                   (equal? '-> (car ST)))
              (check 
                report-error 
                (lambda (ok) (todo (car (cdr (cdr ST)))))
                Gamma
                (car (cdr M))
                (car (cdr ST)))
              (synthesize
                report-error
                (lambda (S) (report-error (list 'expected (list '-> S 'Y) 'got ST)))
                Gamma
                (car (cdr M)))))
       Gamma
       (car M)))
    (report-error (list 'unknown-statement M))))

(define (check report-error todo Gamma M A)
  (cond 
    ((symbol? M)
     (let 
       ((binding (find (lambda (kv) (equal? (car kv) M)) Gamma)))
       (cond ((not binding) (report-error (list 'unbound-variable M)))
             ((equal? A (car (cdr binding))) (todo #t))
             (report-error (list 'expected A 'got (car (cdr binding)))))))
    ((and (equal? 3 (length M))
          (equal? 'lambda (car M))
          (binder-pair? (car (cdr M))))
     (let* ((binder (car (cdr M)))
            (bound-type (car (cdr binder)))
            (body (car (cdr (cdr M)))))
           (with-target-for-source 
             report-error
             (lambda (T) (check report-error todo (cons binder Gamma) body T))
             bound-type
             A)))
    ((equal? 2 (length M))
     (synthesize 
       report-error
       (lambda (ST) 
          (if (and (list? ST)
                   (equal? 3 (length ST))
                   (equal? '-> (car ST)))
              (check 
                report-error 
                (lambda (ok)
                  (let ((T (car (cdr (cdr ST)))))
                       (if (equal? A T)
                           (todo #t)
                           (report-error (list 'expected A 'got T)))))
                Gamma
                (car (cdr M))
                (car (cdr ST)))
              (synthesize 
                report-error
                (lambda (S) (report-error (list 'expected (list '-> S A) 'got ST)))
                Gamma
                (car (cdr M)))))
       Gamma
       (car M)))
    (report-error (list 'unknown-statement M))))

(define (id x) x)

(display 
  (list 
    (check id id '((x A)) `x 'A)
    (check id id '((x B)) `x 'A)
    (check id id '((x A)) `y 'A)
    (check id id '() `(lambda (x A) x) '(-> A A))
    (check id id '() `(lambda (x (-> A A)) x) '(-> (-> A A) (-> A A)))
    (check id id '() `(lambda (x A) x) '(-> A (-> A A)))
    (check id id '() `(lambda (x A) (lambda (y B) x)) '(-> A (-> B A)))
    (check id id '() `(lambda (x A) (lambda (y B) y)) '(-> A (-> B B)))
    (check id id '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> A B))
    (check id id '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> B B))
    (check id id '((f X)) `((lambda (f (-> A B)) (lambda (x A) (f x))) f) '(-> A B))
    (check id id '((f (-> A B))) `((lambda (f (-> A B)) (lambda (x A) (x f))) f) '(-> A B))
  ))


