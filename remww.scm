(define isInInput
    (lambda (row el)
        (member el (cadr row))))
        
(define isInOutput
    (lambda (row el)
        (member el (caddr row))))

(define isElementNeeded
    (lambda (exps el)
        (cond ((null? exps) #t)
              ((isInInput (car exps) el) #t)
              ((isInOutput (car exps) el) #f)
              (else (isElementNeeded (cdr exps) el)))))
              
; returns true if even one is needed
(define isElementsNeeded
    (lambda (exps lst)
        (ormap (lambda (el) (isElementNeeded exps el)) lst)))
  
(define isRowNeeded
    (lambda (exps row)
        (isElementsNeeded (cdr exps) (caddr row))))
            
(define remww_loop
    (lambda (exps)
        (cond ((null? exps) exps)
              ((isRowNeeded exps (car exps)) (append (list (car exps)) (remww_loop (cdr exps))))
              (else (remww_loop (cdr exps))))))
            
(define remww
    (lambda (exps)
        (let ((new_exps (remww_loop exps)))
            (if (equal? exps new_exps)
                exps
                (remww new_exps)))))