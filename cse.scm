(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))

(define sum 
    (lambda (elemList)
        (if (null? elemList)
            0
            (+ (car elemList) (sum (cdr elemList))))))
      
(define getOccurences
    (lambda (tree node)
        (cond ((equal? tree node) 1)
              ((not (list? tree)) 0)
              (else (sum (map (lambda (innerTree) (getOccurences innerTree node)) tree))
                        ))))
            
(define findLet
    (lambda (tree node)
        (cond ((or (not (list? node)) (null? node) (quote? node)) #f)
              ((ormap (lambda (x) x)
                             (map (lambda (innerNode) (findLet tree innerNode)) node))
                             (ormap (lambda (x) x)
                             (map (lambda (innerNode) (findLet tree innerNode)) node)))
              ((> (getOccurences tree node) 1) node)
              (else #f)
              )))
              
            
(define findMultiple
    (lambda (exp)
        (findLet exp exp)))
        
(define replaceWithExp
    (lambda (tree old new)
        (cond ((null? tree) tree)
              ((equal? tree old) new)
              ((not (list? tree)) tree)
              (else (map (lambda (node) (replaceWithExp node old new)) tree)))))

; getNewTreeAndReplacements
; 1. new tree with replaced variables
; 2. pairs of a. generated and b. replaced expressions
(define newTreeAndRep
    (lambda (tree pairs)
        (let ((symb (symbol->string (gensym)))
              (multExpr (findMultiple tree)))
            (if multExpr
                (newTreeAndRep (replaceWithExp tree multExpr symb)
                               (cons (list symb multExpr) pairs))
                (cons tree pairs)))))

; ptl - pairsTreeList
(define removePair
    (lambda (ptl pair)
        (let ((ans (replaceWithExp ptl (car pair) (cadr pair))))
            (list (filter (lambda (pair) (not (equal? (car pair) (cadr pair)))) (car ans)) (cadr ans)))))
        
(define minimizePairs
    (lambda (pairs tree)
        (let ((ptl (list pairs tree)))
            (cond ((null? pairs) (list pairs tree))
                  ((= (getOccurences ptl (car (car pairs))) 2)
                    (let ((newPtl (removePair ptl (car pairs))))
                        (minimizePairs (car newPtl) (cadr newPtl))))
                  (else 
                    (let ((newPtl (minimizePairs (cdr pairs) tree)))
                        (list (append (list (car pairs)) (car newPtl)) (cadr newPtl))))))))
                    
                  
        
(define cse
    (lambda (tree)
        (let* ((ntar (newTreeAndRep tree '()))
                 (newTree (car ntar))
                 (pairs (reverse (cdr ntar)))
                 (minPtl (minimizePairs pairs newTree)))
            (cond ((equal? newTree tree) tree)
                  ((= (length (car minPtl)) 1) `(let ,(car minPtl) ,(cadr minPtl)))
                  (else `(let* ,(car minPtl) ,(cadr minPtl)))))))
                
                
                
(define cse-2 cse)