(load "pc.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*delayed (lambda () <sexpr>))
       (*disj 2)
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))
     

(define <skip>
  (disj <comment>
	<whitespace>))


(define <Boolean>
    (new (*parser (word-ci "#f"))
         (*pack (lambda (_) #f))
         (*parser (word-ci "#t"))
         (*pack (lambda (_) #t))
         (*disj 2)
         done))
         
         
(define <CharPrefix>
    (new (*parser (word "#\\"))
         (*pack (lambda (_) "#\\"))
         done))


(define <VisibleSimpleChar>
    (new (*parser <any-char>)
         (*parser (range (integer->char 0) (integer->char 32)))
         *diff
         done))
         
         
(define ^<meta-char>
    (lambda (str ch)
        (new (*parser (word-ci str))
        (*pack (lambda (_) ch))
        done)))
        

(define <NamedChar>
    (new (*parser (^<meta-char> "lambda" (integer->char 955)))
         (*parser (^<meta-char> "newline" (integer->char 10)))
         (*parser (^<meta-char> "nul" (integer->char 0)))
         (*parser (^<meta-char> "page" (integer->char 12)))
         (*parser (^<meta-char> "return" (integer->char 13)))
         (*parser (^<meta-char> "space" (integer->char 32)))
         (*parser (^<meta-char> "tab" (integer->char 9)))
         (*disj 7)
         done))
         
         
(define <HexChar>
    (new (*parser (range #\0 #\9))
         (*parser (range #\a #\f))
         (*parser (range #\A #\F))
         (*disj 3)
        done))
        
        
         
(define <HexUnicodeChar>
    (new (*parser (char #\x))
         (*parser <HexChar>) *plus
         (*caten 2)
         (*pack-with (lambda (x ch) (integer->char (string->number (list->string ch) 16))))
         done))
         
                     
         
        
(define <Char>
    (new (*parser <CharPrefix>)
         (*parser <NamedChar>)
         (*parser <HexUnicodeChar>)
         (*parser <VisibleSimpleChar>)
         (*disj 3)
         (*caten 2)
         (*pack-with (lambda (cp vnh)
                        vnh))
         done))
        
        
(define <Natural> 
    (new (*parser (range #\0 #\9)) *plus
         (*pack (lambda (nums)  
                    (string->number (list->string nums))))
        done)) 
        
         
(define <Integer>
    (new (*parser (char #\+))
         (*parser (char #\-))
         (*disj 2)
         (*parser <Natural>)
         (*caten 2)
         
         (*pack-with (lambda (s num)
                        (if (eq? s #\-)
                            (- num)
                            num)))
         
         (*parser <Natural>)
         (*disj 2)
         done))
         

(define <Fraction>
    (new (*parser <Integer>)
         (*parser (char #\/))
         (*parser <Natural>)
         (*caten 3)
         (*pack-with (lambda (i div n)
                        (/ i n)))
         done))
                        
         
(define <Number>
    (new (*parser <Fraction>)
         (*parser <Integer>)
         (*disj 2)
         done))
         

(define <StringLiteralChar>
    (new (*parser <any-char>)
         (*parser (char #\"))
         (*parser (char #\\))
         (*disj 2)
         *diff
         done))
    
(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f"  (integer->char 12)))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" (integer->char 13)))
       (*disj 6)
       done))
         
(define <StringHexChar>
    (new (*parser (char #\\))
         (*parser (char-ci #\x))
         (*parser <HexChar> ) *star
         (*parser (char #\;))
         (*caten 4)
         (*pack-with (lambda (_ x c np)  (string->number (list->string c) 16)) )
         (*guard (lambda (num)  (< num  1114111) ))
         (*pack (lambda (x) (integer->char x) ))
         done))
         
         
(define <StringChar>
    (new (*parser <StringLiteralChar>)
         (*parser <StringMetaChar>)
         (*parser <StringHexChar>)
         (*disj 3)
         done))
         
         
(define <String>
    (new (*parser (char #\"))
         (*parser <StringChar>) *star
         (*parser (char #\"))
         (*caten 3)
         (*pack-with (lambda (m1 sc m2)
                        (list->string `(,@sc))))
         done))
         
         
(define <SymbolChar>
    (new (*parser (range #\0 #\9))         
         (*parser (range #\a #\z))
         (*parser (range #\A #\Z))
         (*pack (lambda (c)
                    (integer->char (+ (char->integer c) 32))))
         (*parser (char #\!))
         (*parser (char #\$))
         (*parser (char #\^))
         (*parser (char #\*))
         (*parser (char #\-))
         (*parser (char #\_))
         (*parser (char #\=))
         (*parser (char #\+))
         (*parser (char #\<))
         (*parser (char #\>))
         (*parser (char #\?))
         (*parser (char #\/))
         (*parser (char #\:))
         (*disj 16)
         done))
         
(define <Symbol> 
    (new (*parser <SymbolChar>) *plus
         (*guard (lambda (n) (not (number? (string->number (list->string n))))))
         (*pack (lambda (s)
                   (string->symbol (list->string s))))
        done))
        
        
(define <ProperList> 
    (new (*parser (char #\())
         (*parser <skip>) *star
         
         (*delayed (lambda() <sexpr>))
         (*parser <skip>) *star
         (*caten 2)  
         (*pack-with (lambda (sexp ws)
                        sexp))
          *star
          
         (*caten 2)
         (*pack-with (lambda (ws sexpList)
                        sexpList))
          
         (*parser (char #\)))
         (*caten 3)
         (*pack-with (lambda (sp sexp ss)
                        `(,@sexp)))
        done))


(define <ImproperList> 
    (new (*parser (char #\())
         (*parser <skip>) *star
         
         (*delayed (lambda() <sexpr>))
         (*parser <skip>) *star
         (*caten 2)  
         (*pack-with (lambda (sexp ws)
                        sexp))
         *plus
         
         (*parser <skip>) *star
         (*parser (char #\.))
         (*parser <skip>) *star
         
         (*delayed (lambda() <sexpr>))
         (*parser <skip>) *star
         (*caten 2)  
         (*pack-with (lambda (sexp ws)
                        sexp))
         (*parser (char #\)))
         
         (*caten 8)
         (*pack-with (lambda (sp ws0 sexpp ws1 dot ws2 sexp ss)
                        `(,@sexpp ,@sexp)))
        done))
         
                  
         

    
(define <Vector> 
        (new (*parser (char #\#))
             (*parser <ProperList>)
             (*caten 2)
             (*pack-with (lambda (l s)
                            (list->vector s)))
             done))
            
    
(define <Quoted>
    (new (*parser (char #\'))
         (*delayed (lambda() <sexpr>))
         (*caten 2)
         (*pack-with (lambda(c s)
                    `',s))
            
         done))
         
             
(define <QuasiQuoted>
    (new (*parser (char #\`))
         (*delayed (lambda() <sexpr>))
         (*caten 2)
         (*pack-with (lambda(c s)
                    (list 'quasiquote s)))
            
         done))
            
            
(define <Unquoted>
    (new (*parser (char #\,))
         (*delayed (lambda() <sexpr>))
         (*caten 2)
         (*pack-with (lambda(c s)
                    (list 'unquote s)))
            
         done))    
    
    
(define <UnquoteAndSplice>
    (new (*parser (char #\,))
         (*parser (char #\@))
         (*delayed (lambda() <sexpr>))
         (*caten 3)
         (*pack-with (lambda(c sh s)
                    (list 'unquote-splicing s)))
            
         done))    
    
    
(define <InfixPrefixExtensionPrefix>
    (new (*parser (word "##"))
         (*parser (word "#%"))
         (*disj 2)  
         done))
    

(define <InfixSymbolAllowed>
    (new (*parser (range #\0 #\9))
         (*parser (range #\a #\z))
         (*parser (range #\A #\Z))
         (*pack (lambda (c)
                    (integer->char (+ (char->integer c) 32))))
         (*parser (char #\!))
         (*parser (char #\$))
         (*parser (char #\_))
         (*parser (char #\=))
         (*parser (char #\<))
         (*parser (char #\>))
         (*parser (char #\?))
         (*disj 10)
         done))       
    

(define <InfixSymbol>
    (new (*parser <InfixSymbolAllowed>) *plus
         (*guard (lambda (n) (not (number? (string->number (list->string n))))))
         (*pack (lambda (s)
                   (string->symbol (list->string s))))
        done))
        
        
        
(define <CircleParenthesis>
    (new (*parser (char #\())
         (*delayed (lambda () <InfixExpression>))
         (*parser (char #\)))
         (*caten 3)
         (*pack-with (lambda (sp inf ss)
                        inf))
        done))
        
    
(define <FunctionArgList>
    (new (*parser (char #\())
         
         (*delayed (lambda () <InfixExpression>))
         (*parser (char #\,))
         (*delayed (lambda () <InfixExpression>))
         (*caten 2)
         (*pack-with (lambda (psik inf) inf))
         *star
         (*parser (char #\)))
         (*caten 4)
         (*pack-with (lambda (sp inf infList ss)
                        (lambda (element)
                            `(,element ,inf ,@infList))))
                            
         (*parser (char #\())
         (*parser <skip>) *star
         (*parser (char #\)))
         (*caten 3)
         (*pack-with (lambda (sp _ ss)
                        (lambda (element)
                            `(,element))))
         (*disj 2)
        done))

(define <SquareParenthesisLambda>
    (new (*parser (char #\[))
         (*delayed (lambda () <InfixExpression>))
         (*parser (char #\]))
         (*caten 3)
         (*pack-with (lambda (sp inf ss)
                        (lambda (element)
                            `(vector-ref ,element ,inf))))
        done))
        
        
(define <Atom> 
    (new (*parser <skip>) *star
         (*parser <InfixSymbol>)
         (*parser <Number>)         
         (*parser <CircleParenthesis>)
         (*disj 3)
         (*parser <skip>) *star
         (*caten 3)
         (*pack-with (lambda (s1 a s2) a))
         done))
        
(define <Array>
    (new (*parser <Atom>)
         (*parser <SquareParenthesisLambda>) *plus
         (*caten 2)
         (*pack-with (lambda (atom1 lstLamb)
                        (fold-left (lambda (element lmbda)
                                    (lmbda element))
                                        atom1 lstLamb)))     
    done))
    
    
    
    
(define <Function>
    (new (*parser <Atom>)
         (*parser <FunctionArgList>) *plus
         (*caten 2)
         (*pack-with (lambda (atom1 lstLamb)
                        (fold-left (lambda (element lmbda)
                                    (lmbda element))
                                        atom1 lstLamb)))     
    done))
    
    
    
(define <InfixEscape>
    (new (*parser <InfixPrefixExtensionPrefix>)
         (*delayed (lambda() <sexpr>))
         (*caten 2)
         (*pack-with (lambda (ipep sexpr)
                        sexpr))
         done))
 
 
(define <Neg>
    (new (*parser (char #\-))
         (*delayed (lambda () <Leaf>))
         (*caten 2)
         (*pack-with (lambda (c inf)
                               `(,(string->symbol (string c)) ,inf)))
         done))
         
(define <Leaf>
    (new (*parser <skip>) *star
         (*parser <Function>)
         (*parser <Array>)
         (*parser <Atom>)
         (*parser <Neg>)
         (*parser <InfixEscape>)
         (*disj 5)
         (*parser <skip>) *star
         (*caten 3)
         (*pack-with (lambda (ws1 leaf ws2) leaf))
         done))

(define <Pow>
    (new (*parser <Leaf>)
         
         (*parser (char #\*))
         (*parser (char #\*))
         (*caten 2)
         (*pack-with (lambda (s1 s2)
                        "**"))
         (*parser (char #\^))
         (*pack (lambda (s1)
                        "^"))
         (*disj 2)
         (*caten 2)
         (*pack-with (lambda (atom sign)
                        (lambda (element)
                            (list 'expt atom element))))
         *star
         
         (*parser <Leaf>)
         (*caten 2)
         (*pack-with (lambda (lstLamb atom1)
                        (fold-left (lambda (element lmbda)
                                        (lmbda element))
                                            atom1 (reverse lstLamb))))
                                    
         (*parser <Leaf>)
         (*disj 2)
         done))
  
    
    
(define <Mul>
    (new (*parser <Pow>)
         (*parser (char #\*))
         (*parser (char #\/))
         (*disj 2)
         (*parser <Pow>)
         (*caten 2)
         (*pack-with (lambda (sign pow)
                        (lambda (element)
                            (list (string->symbol (string sign)) element pow))))
         *star
         (*caten 2)
         (*pack-with (lambda (pow1 lstLamb)
                        (fold-left (lambda (element lmbda)
                                (lmbda element))
                                    pow1 lstLamb)))
         done))
    

(define <InfixExpression>
    (new (*parser <Mul>)
         (*parser (char #\+))
         (*parser (char #\-))
         (*disj 2)
         (*parser <Mul>)
         (*caten 2)
         (*pack-with (lambda (sign mult)
                        (lambda (element)
                            (list (string->symbol (string sign)) element mult))))
         *star
         (*caten 2)
         (*pack-with (lambda (mult1 lstLamb)
                        (fold-left (lambda (element lmbda)
                                (lmbda element))
                                    mult1 lstLamb)))
         done))
         
    
    

    
    
    
    
(define <InfixExtension> 
    (new (*parser <InfixPrefixExtensionPrefix>)
         (*parser <InfixExpression>)
         (*caten 2)
         (*pack-with (lambda (ipep inf) inf))
         done))
    
    
    
    
(define <sexpr>
    (new (*parser <skip>) *star
         (*parser <Boolean>)
         (*parser <Char>)
         (*parser <Symbol>)
         (*parser <Number>)
         (*parser <String>)
         (*parser <ProperList>)
         (*parser <ImproperList>)
         (*parser <Vector>)
         (*parser <Quoted>)
         (*parser <QuasiQuoted>)
         (*parser <Unquoted>)
         (*parser <UnquoteAndSplice>)
         (*parser <InfixExtension>)
         (*disj 13)
         (*parser <skip>) *star
         (*caten 3)
         (*pack-with (lambda (sk1 sexpr sk2) sexpr))
         done))
    








(load "pattern-matcher.scm")
            

(define *reserved-words*
    '(and begin cond define do else if lambda
      let let* letrec or quasiquote unquote
      unquote-splicing quote set!))
    
(define var?
    (lambda (str)
        (and (symbol? str) (not (member str *reserved-words*)) (not (number? str)))))

(define notlist?
    (lambda (lst)
        (or (not (list? lst))
            (eq? (car lst) 'quote)
            )))
        
(define listOfVars?
    (lambda (lst)
        (and (list? lst)
             (andmap (lambda (el) (var? el)) lst))))
             
(define strip-seqs
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) (list exp))
              ((equal? (car exp ) 'seq) (car (fold-left append '() (map strip-seqs (cdr exp)))))
              (else (list exp)))))
        
(define clear-seqs
    (lambda (exp)
        (list 'seq (fold-left append '() (map strip-seqs (cadr exp))))))

(define identify-lambda
        (lambda (argl ret-simple ret-opt ret-var)
                (cond 
                        ((null? argl) (ret-simple '()))
                        ((var? argl) (ret-var argl))
                        (else (identify-lambda (cdr argl)
                                        (lambda (s) (ret-simple `(,(car argl) ,@s))) 
                                        (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
                                        (lambda (var) (ret-opt `(,(car argl)) var)))))))
                                        
(define *void-object*
    (if #f #f))
                                        
(define beginify
        (lambda (s)
                (cond
                        ((null? s) *void-object*)
                        ((null? (cdr s)) (car s))
                        (else `(begin ,@s)))))
                        
                        
(define notHaveDup? 
    (lambda (lst)
        (andmap (lambda (y) ( = (length (filter (lambda(z) (eq? z y)) lst)) 1)) lst)))

(define prameters-good?
    (lambda (args)
        (identify-lambda
            args
            (lambda (s) (and (listOfVars? s) (notHaveDup? s)))
            (lambda (s opt) (and (listOfVars? (cons opt s)) (notHaveDup? (cons opt s))))
            (lambda (s) (var? s)))))
    
    
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))  
        
    
(define parse
       (let ((run 
                (compose-patterns
                        
                        ;CONST
                        (pattern-rule
                            (? 'nil (lambda(v) (equal? v (void))))
                            (lambda (c) `(const ,c)))
                        
                        (pattern-rule
                            (? 'vec vector?)
                            (lambda (c) `(const ,c)))
                            
                        (pattern-rule
                            (? 'bool boolean?)
                            (lambda (c) `(const ,c)))
                            
                        (pattern-rule
                            (? 'c char?)
                            (lambda (c) `(const ,c)))
                        
                        (pattern-rule
                            (? 'num number?)
                            (lambda (c) `(const ,c)))
                        
                        (pattern-rule
                            (? 'str string?)
                            (lambda (c) `(const ,c)))
                            
                        (pattern-rule
                            `(quote ,(? 'c))
                            (lambda (c) `(const ,c)))
                        
                        ;VAR
                        (pattern-rule
                            (? 'var var?)
                            (lambda (var) `(var ,var)))

                        ;CONDITIONALS
                        (pattern-rule
                            `(if ,(? '<test>) ,(? '<dit>) ,(? '<dif>))
                            (lambda (<test> <dit> <dif>) `(if3 ,(parse <test>) ,(parse <dit>) ,(parse <dif>))))

                        (pattern-rule
                            `(if ,(? '<test>) ,(? '<dit>))
                            (lambda (<test> <dit>) `(if3 ,(parse <test>) ,(parse <dit>) ,(parse (void)))))
                            
                       
                        
                        ;DISJUNCTION
                        (pattern-rule
                           `(or)
                           (lambda () (parse #f)))
                           
                        (pattern-rule
                            `(or ,(? 'exp notlist?))
                            (lambda (exp) (parse exp)))
                           
                        (pattern-rule
                            `(or . ,(? 'procs))
                            (lambda (procs) `(or ,(map parse procs))))
                            
                        
                        ;LAMBDA
                        (pattern-rule
                            `(lambda ,(? 'args prameters-good?) ,(? 'body) . ,(? 'rest list?))
                            (lambda (args body rest) 
                                    `(,@(identify-lambda
                                            args
                                            (lambda (s) `(lambda-simple ,s ))   ;ret-simple
                                            (lambda (s opt) `(lambda-opt ,s ,opt))  ;ret-opt
                                            (lambda (var) `(lambda-var ,var))) ;ret-var
                                            ,(parse `(begin ,body ,@rest)))
                                    ))
                        
                        ;DEFINE
                        (pattern-rule
                            `(define ,(? 'var var?) . ,(? 'any))
                            (lambda (var any)
                                `(def ,(parse var) ,(parse (beginify any)))))
                                
                        (pattern-rule
                            `(define ,(? 'list list? (lambda (lst) (var? (car lst)))) . ,(? 'rest))
                            (lambda (lst rest)
                                `(def ,(parse (car lst)) ,(parse `(lambda (,@(cdr lst)) ,(beginify rest))))))
                                
                        (pattern-rule
                            `(define ,(? 'list pair? (lambda (lst) (var? (car lst)))) . ,(? 'rest))
                            (lambda (lst rest)
                                `(def ,(parse (car lst)) ,(parse `(lambda ,(cdr lst) ,(beginify rest))))))
                        
                        
                        ;APPLIC
                        (pattern-rule
                            `(,(? 'proc var?) . ,(? 'args))
                            (lambda (proc argsList) 
                                (if (null? argsList)
                                `(applic ,(parse proc) ,argsList)
                                `(applic ,(parse proc) ,(map parse argsList)))))
                                
                        (pattern-rule
                            `(,(? 'proc const?) . ,(? 'args))
                            (lambda (proc argsList) 
                                (if (null? argsList)
                                `(applic ,(parse proc) ,argsList)
                                `(applic ,(parse proc) ,(map parse argsList)))))
                        
                        (pattern-rule
                            `(,(? 'apps list?) . ,(? 'subs))
                            (lambda (apps subs)
                            (if (null? subs)
                            `(applic ,(parse apps) ())
                            `(applic ,(parse apps) ,(map parse subs)))))
                        
                           
                        ;SEQ
                        (pattern-rule
                            `(begin)
                            (lambda () `(const ,*void-object*)))
                        
                        (pattern-rule
                            `(begin ,(? 'expr) . ,(? 'exprs))
                            (lambda (expr exprs) 
                                (if (null? exprs)
                                    (parse  expr)
                                    (let ((all (append (list expr) exprs)))
                                    (clear-seqs `(seq ,(map parse all)))))))
                           
                        (pattern-rule
                            `(,(? 'exprs) var?)
                            (lambda (exprs) (clear-seqs `(seq ,(parse (beginify exprs))))))
                        
                        
                        
                        ;ASSIGMENT
                        (pattern-rule
                            `(set! ,(? 'var var?) ,(? 'value))
                            (lambda (var val) `(set ,(parse var) ,(parse val))))
                    
                           
                        ;LET
                        (pattern-rule
                            `(let ,(? 'pairs list?) . ,(? 'body))
                            (lambda (pairs body)
                            `,(parse `((lambda ,(map car pairs) ,@body) ,@(map cadr pairs)))))
                           
                        
                        ;LET*   
                        (pattern-rule
                            `(let* () ,(? 'expr) . ,(? 'exprs list?))
                            (lambda (expr exprs) (parse `(let () ,expr ,@exprs))))
                            
                        (pattern-rule
                            `(let* ((,(? 'var var?) ,(? 'val?)) . ,(? 'rest)) . ,(? 'exprs))
                            (lambda (var val rest exprs) 
                                (if (null? rest)
                                    (parse `(let ((,var ,val)) ,@exprs))
                                    (parse `(let ((,var ,val)) (let* ,rest . ,exprs))))))
                           
                           
                        ;LETREC
                        (pattern-rule
                            `(letrec ,(? 'pairs list?) . ,(? 'body))
                            (lambda (pairs body)
                             `,(parse `(let ,(map (lambda (pair) (list (car pair) #f)) pairs)
                                                                    (begin ,@(map 
                                                                                (lambda (pair) `(set! ,(car pair) ,(cadr pair))) 
                                                                                pairs) 
                                                                            ((lambda () ,@body)))))))
                           
                        ;AND
                        (pattern-rule
                            `(and)
                            (lambda () (parse #t)))
                        
                        (pattern-rule
                            `(and ,(? 'app) . ,(? 'apps))
                            (lambda (app apps)
                                (if (null? apps)
                                    (parse app)
                                    (parse `(if ,app (and ,@apps) #f)))))
                           
                           
                        ;COND
                        (pattern-rule
                            `(cond ,(? 'cond) . ,(? 'restConds))
                            (lambda (conda rest)
                                (cond ((eq? (car conda) 'else) (parse (beginify (cdr conda))))
                                      ((null? rest) (parse `(if ,(car conda) ,(beginify (cdr conda)) ,*void-object*)))
                                      
                                      (else (parse `(if ,(car conda) ,(beginify (cdr conda)) (cond ,@rest)))))))                              
                        
                        
                        ;QQ
                        (pattern-rule
                            `(quasiquote . ,(? 'rest))
                            (lambda (rest) (parse (expand-qq (car rest)))))

                            
                            )))
               
            (lambda (e) (run e (lambda() (error 'parse (format "I can't recognize this: ~s" e)))))))
            
            
            
       
(define remove-applic-lambda-nil
        (let ((run 
                (compose-patterns
                        (pattern-rule 
                            `(applic (lambda-simple () ,(? 'body)) ())
                            (lambda (body)
                                (remove-applic-lambda-nil body))))))
            (lambda (pes) (run pes 
                             (lambda()
                                (if (and (list? pes) (not (null? pes)))
                                    (map remove-applic-lambda-nil pes)
                          pes))))))
              
              
              
              

              
(define lambda?
    (lambda (exp)
        (and (list? exp) (not (null? exp)) (or (eq? (car exp) 'lambda-simple) (eq? (car exp) 'lambda-opt) (eq? (car exp) 'lambda-var)))))

(define getLambdaParameters
    (lambda (exp)
        (cond ((eq? (car exp) 'lambda-simple) (cadr exp))
              ((eq? (car exp) 'lambda-opt) (append (cadr exp) (list (caddr exp))))
              ((eq? (car exp) 'lambda-var) (list (cadr exp)))
              (else (cadr exp)))))

(define getLambdaBody
    (lambda (exp)
        (cond ((eq? (car exp) 'lambda-simple) (caddr exp))
              ((eq? (car exp) 'lambda-opt) (cadddr exp))
              ((eq? (car exp) 'lambda-var) (caddr exp))
              (else (caddr exp)))))
              
(define setLambdaBody
    (lambda (orgLambda newBody)
        (cond ((eq? (car orgLambda) 'lambda-simple) `(lambda-simple ,(cadr orgLambda) ,newBody))
              ((eq? (car orgLambda) 'lambda-opt) `(lambda-opt ,(cadr orgLambda) ,(caddr orgLambda) ,newBody))
              ((eq? (car orgLambda) 'lambda-var) `(lambda-var ,(cadr orgLambda) ,newBody))
              (else #f))))
              
(define end '())

;eliminate-nested-defines-seq-pairs
; returns a list of defs, values and the rest of the body
(define endSeqPairs
    (lambda (exp defs values)
        (cond ((or (null? exp) (not (list? exp))) (list defs values exp))
              ((eq? (caar exp) 'def)
                (endSeqPairs (cdr exp)
                             (append defs (list (cadar exp)))
                             (append values (list (end (caddar exp))))))
              (else (list defs values exp)))))


;called only with lambda body
(define end-lambda-body
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp) ; (= (length exp) 1)
              ;     its a sequence       there aren't any nested defines
              ((and (eq? (car exp) 'seq) (not (= (length (car (endSeqPairs (cadr exp) '() '()))) 0)))
               (let ((esp (endSeqPairs (cadr exp) '() '()))) ; esp[0]=defs, esp[1]=values, esp[2]=rest
                    `(applic (lambda-simple 
                                ,(map cadr (car esp))
                                (seq ,(append (map (lambda (def val) `(set ,def ,val)) (car esp) (end (cadr esp)))
                                            (end (caddr esp))))
                            )
                            ,(map (lambda (_) '(const #f)) (car esp))
                    )
                ))
              ((eq? (car exp) 'def)
                (let ((esp (endSeqPairs (list exp) '() '()))) ; esp[0]=defs, esp[1]=values, esp[2]=rest
                    `(applic (lambda-simple 
                                ,(map cadr (car esp))
                                (seq ,(list (map (lambda (def val) `(set ,def ,val)) (car esp) (end (cadr esp)))
                                            (end (caddr esp))))
                             )
                             ,(map (lambda (_) '(const #f)) (car esp))
                     )
                ))
              (else (end exp)))))
        
              
;eliminate-nested-defines body
(define end
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp) ; (= (length exp) 1)
              ((lambda? exp) (setLambdaBody exp (end-lambda-body (getLambdaBody exp))))
              (else (map end exp)))))
              
(define eliminate-nested-defines 
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp)
              ((eq? (car exp) 'def)
                `(def ,(cadr exp) ,(end (caddr exp))))
              (else (end exp)))))
              
              
              
              
              
              
              
              
              
              
              
              
              
                      
(define taggedSet?
    (lambda (exp)
        (and (list? exp)
             (not (null? exp)) 
             (eq? (car exp) 'set) 
             (list? (cadr exp)) 
             (= (length (cadr exp)) 2) 
             (eq? (caadr exp) 'var))))
             
(define isTheVar?
    (lambda (e v)
        (and (list? e) (= (length e) 2) (eq? (car e) 'var) (eq? (cadr e) v))))
        
(define pushExpToStartOfExp
    (lambda (exp expToPush)
        (cond ((or (null? exp) (not (list? exp))) expToPush)
              ((eq? (car exp) 'seq) `(seq ,(append (list expToPush) (cadr exp))))
              (else `(seq (,expToPush ,exp))))))
        
; var is 'a or 'b or so..
(define isLambdaAndVarIsRedeclared?
    (lambda (exp var)                                                         ;the list of parameters
        (and (lambda? exp) (not (= (length (filter (lambda (el) (eq? el var)) (getLambdaParameters exp))) 0)))))
            
; var is 'a or 'b or so..
(define isLambdaAndVarIs-NOT-Redeclared?
    (lambda (exp var)                                                    ;the list of parameters
        (and (lambda? exp) (= (length (filter (lambda (el) (eq? el var)) (getLambdaParameters exp))) 0))))

; var is 'a or 'b or so..
(define isLambdaAndVarIsRedeclaredOrIsSet?
    (lambda (exp var)
        (or (isLambdaAndVarIsRedeclared? exp var)
            (taggedSet? exp))))

; is exp fulfilling f untill there is lambda with this var inside of it which it ignores it and continue
; v   (var)  is 'a or 'b or so..
; f   (func) must receive an exp as a list and a value and checks if the list fullfills it needs
; cut (func) must receive an exp as a list and a value and cut the search in that branch if it fullfills it needs
(define fulfillFunc?
    (lambda (f v cut)
        (lambda (exp)
            (cond ((or (null? exp) (not (list? exp)) (cut exp v)) #f)
                  ((f exp v) #t)
                  (else (ormap (fulfillFunc? f v cut) exp))))))
               
; find if under an exp tree there is a var with that name (untill it get to a leaf or a lambda with redeclaration of that var)
(define isVarInsideExp?
    (lambda (exp var)
        ((fulfillFunc? isTheVar?
                       var
                       isLambdaAndVarIsRedeclared?)
         exp)))
                
; checks if the variable is bounded
; if there is a lambda in the exp (flatly - not as tree search), it search this lambda fully to find if the variable is called there
(define isVarBounded?
    (lambda (exp var)
        (let ((lambdaHandler (lambda (innerExp)
                                (and (isLambdaAndVarIs-NOT-Redeclared? innerExp var) 
                                     (isVarInsideExp? (getLambdaBody innerExp) var)))))
             (or (lambdaHandler exp)
                 (ormap lambdaHandler exp)))))
            
            
; checks flatly whether there is a set in the exp
(define isVarSet?
    (lambda (exp var)
        (and (taggedSet? exp)
             (eq? (cadadr exp) var))))

             
; find if under an exp tree there is a var with that name (untill it get to a leaf or a lambda with redeclaration of that var)
(define isVarInsideExp?
    (lambda (exp var)
        ((fulfillFunc? isTheVar?
                       var
                       isLambdaAndVarIsRedeclared?)
         exp)))             
             
             
; checks if the variable is called (excluding (set var to something))
; if there is a set in the exp (flatly - not as tree search), it search this set fully by himself to find if the variable is called there
(define isVarGet?
    (lambda (exp var)
        (let ((setHandler (lambda (innerExp)
                                (and (taggedSet? innerExp) 
                                     ((fulfillFunc? isVarGet? var isLambdaAndVarIsRedeclaredOrIsSet?) (caddr innerExp))))))
            (or (isTheVar? exp var)
                (ormap setHandler exp)))))
                
; search if all three of the requirments are fulfilled in the exp
(define isBoxNeeded?
    (lambda (exp var)
        (and ((fulfillFunc? isVarBounded? var isLambdaAndVarIsRedeclared?) exp)
             ((fulfillFunc? isVarSet? var isLambdaAndVarIsRedeclared?) exp)
             ((fulfillFunc? isVarGet? var isLambdaAndVarIsRedeclaredOrIsSet?) exp))))
     
(define replaceWithBox
    (lambda (var)
        (lambda (exp)
            (cond ((or (null? exp) (not (list? exp)) (isLambdaAndVarIsRedeclared? exp var)) exp)
                ((isTheVar? exp var) `(box-get (var ,var)))
                ((isVarSet? exp var) `(box-set (var ,var) ,((replaceWithBox var) (caddr exp))))
                (else (map (replaceWithBox var) exp))))))
                
; checks if a variable is need to be replaced and if so, replaces it.
(define lambdaCheckAndBox
    (lambda (exp var)
        (if (isBoxNeeded? exp var)
            (pushExpToStartOfExp ((replaceWithBox var) exp) `(set (var ,var) (box (var ,var))))
            exp)))
                
; replaces lst variables if needed inside expression
(define lambdaCheckAndBoxList
    (lambda (exp lst)
        (if (null? lst)
            exp
            (lambdaCheckAndBoxList (lambdaCheckAndBox exp (car lst)) (cdr lst)))))

(define box-set
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp)
              ((lambda? exp) (setLambdaBody exp 
                                            (box-set (lambdaCheckAndBoxList (getLambdaBody exp)
                                                                            (reverse (getLambdaParameters exp))))))
              (else (map box-set exp)))))
              
              
              
              

(define lex-add-replace '())


(define lex-add
    (lambda (exp lst)
        (let ((run
                (compose-patterns
                    (pattern-rule
                    `(lambda-simple ,(? 'args) . ,(? 'body))
                    (lambda (args body)
                        `(lambda-simple ,args ,(car (lex-add-replace body (cons args lst))))))
                        
                    (pattern-rule
                    `(lambda-var ,(? 'args) . ,(? 'body))
                    (lambda (arg body)
                    `(lambda-var ,arg ,(car (lex-add-replace body (cons (list arg) lst))))))
                        
                    (pattern-rule
                    `(lambda-opt ,(? 'args) ,(? 'arg (lambda (el) (not (list? el)))) . ,(? 'body))
                    (lambda (args arg body)
                    `(lambda-opt ,args ,arg ,(car (lex-add-replace body (cons (append args (list arg)) lst)))))))))
                                                  ;else
                (run exp (lambda () (lex-add-replace exp lst))))))
        
;goal: a => (bvar a 2 3)
;lst: ((a b c) (c d e) (e f g)...)
(define varsIndex
    (lambda (var lst)
        (letrec ((finder (lambda (partLst major minor)
                                (cond ((null? (car partLst)) (finder (cdr partLst) (+ major 1) 0))
                                      ((equal? (caar partLst) var) (cons major minor))
                                      (else (finder (cons (cdar partLst) (cdr partLst)) major (+ minor 1)))))))
            (if (member var (fold-right append '() lst))
                (let ((majorMinor (finder lst -1 0)))
                    (if (= (car majorMinor) -1)
                        `(pvar ,var ,(cdr majorMinor))
                        `(bvar ,var ,(car majorMinor) ,(cdr majorMinor))))
                `(fvar ,var)))))
    
;; exp and list of current depth params ((pvars) (bvars0) (bvars1)...)
(define lex-add-replace
    (lambda (exp lst)
        (cond ((or (null? exp) (not (list? exp))) exp)
              ((eq? (car exp) 'var) (varsIndex (cadr exp) lst))
              (else (map (lambda (innerExp) (lex-add innerExp lst)) exp)))))
        
        
(define pe->lex-pe 
    (lambda(pes)
        (lex-add pes '())))
        

        

(define annotate
    (lambda (pes tp?)
            ;const var
        (if (or (equal? (car pes) 'const) (equal? (car pes) 'var) 
                (equal? (car pes) 'pvar) (equal? (car pes) 'bvar) (equal? (car pes) 'fvar)
                (equal? (car pes) 'box-get))
            pes
            ;applic
            (if (equal? (car pes) 'applic)
                (if tp?
                    `(tc-applic ,(annotate (cadr pes) #f) ,(map (lambda (pe)
                                            (annotate pe #f)) (caddr pes)))
                    `(applic ,(annotate (cadr pes) #f) ,(map (lambda (pe)
                                            (annotate pe #f)) (caddr pes))))
                      ;or
                (cond ((equal? (car pes) 'or)
                            `(or ,(map (lambda (pe)
                                            (if (eq? (car (list-tail (cadr pes) (- (length (cadr pes)) 1))) pe)
                                                (annotate pe tp?)
                                                (annotate pe #f))) 
                                        (cadr pes))))
                     ;box-set
                     ((equal? (car pes) 'box-set)
                        `(box-set ,(cadr pes) ,(annotate (caddr pes) #f)))
                     ;set then box
                     ((and (equal? (car pes) 'set) (equal? (caaddr pes) 'box))
                        `(set ,(cadr pes) ,(annotate (caddr pes) #f)))
                    ;box
                     ((equal? (car pes) 'box)
                      `(box ,(annotate (cadr pes) tp?)))
                    ;set
                     ((and (equal? (car pes) 'set))
                        `(set ,(cadr pes) ,(annotate (caddr pes) #f)))
                     ;if
                     ((equal? (car pes) 'if3)
                            `(if3 ,(annotate (cadr pes) #f)
                                   ,(annotate (caddr pes) tp?)
                                   ,(annotate (cadddr pes) tp?)))
                     ;define
                     ((equal? (car pes) 'def)
                            `(def ,(cadr pes) ,(annotate (caddr pes) #f)))
                     ;lambda-simple
                     ((equal? (car pes) 'lambda-simple)
                            `(lambda-simple ,(cadr pes) ,(annotate (caddr pes) #t)))
                     ;lambda-opt
                     ((equal? (car pes) 'lambda-opt)
                            `(lambda-opt ,(cadr pes) ,(caddr pes) ,(annotate (cadddr pes) #t)))
                    ;lambda-var 
                    ((equal? (car pes) 'lambda-var)
                            `(lambda-var ,(cadr pes) ,(annotate (caddr pes) #t)))
                    ;seq
                    (else (equal? (car pes) 'seq)
                            `(seq ,(map (lambda(pe)
                                            (if (eq? (car (list-tail (cadr pes) (- (length (cadr pes)) 1))) pe)
                                                (annotate pe tp?)
                                                (annotate pe #f)))
                                            (cadr pes)))))))))
                     
                     
        
        
(define annotate-tc
    (lambda (pes)
        (annotate pes #f)))
        
        
        
        
        

        
        
        
        
        
        
        
        
        
        
        
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FINAL PROJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define prologue
(string-append
"
#include <stdio.h>
#include <stdlib.h>
#include \"cisc.h\"

int main()
{
START_MACHINE;

JUMP(CONTINUE);

#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"
#include \"scheme.lib\"

CONTINUE:\n"
))


(define epilogue
(string-append
    "JUMP(L_exit);\n"
    "L_error_cannot_apply_non_clos:\n"
    "SHOW(\"FATAL ERROR CANNOT APPLY NON CLOSURE\",R0);\n"
    "JUMP(L_exit);\n"
    
    "L_error_args_count:\n"
    "SHOW(\"FATAL ERROR NUMBER OF ARGUMENTS MISMATCH\",R0);\n"
    "JUMP(L_exit);\n"
    
    "L_error_type_mismatch:\n"
    "SHOW(\"FATAL ERROR TYPE MISMATCH\",R0);\n"
    "JUMP(L_exit);\n"
    
    "L_error_non_positive_number:\n"
    "SHOW(\"FATAL ERROR NON POSITIVE NUMBER ENTERED\",R0);\n"
    "JUMP(L_exit);\n"
        
    "L_error_invalid_index:\n"
    "SHOW(\"FATAL ERROR INVALID INDEX ENTERED\",R0);\n"
    "JUMP(L_exit);\n"
    
    "L_second_arg_cannot_be_zero:\n"
    "SHOW(\"FATAL ERROR SECOND ARG CANNOT BE ZERO\",R0);\n"
    "JUMP(L_exit);\n"
    
	"L_error_must_be_proper_list:\n"
    "SHOW(\"FATAL ERROR ARG MUST BE PROPER LIST\",R0);\n"
    "JUMP(L_exit);\n"
    
    "L_exit:\n"

    "\nSTOP_MACHINE;

    return 0;
    }"))


(define fullOpt
    (lambda(pe)
        (annotate-tc
            (pe->lex-pe
                (box-set
                    (remove-applic-lambda-nil
                        (eliminate-nested-defines pe)))))))

                           
(define listOfExps
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s)
	      (append (list e)
		(listOfExps <sexpr> (list->string s))))
	    (lambda (w) `()))))

(define START_MEMORY 1)
	    
(define major -1)

(define memCount START_MEMORY)

(define memCountIncAndGet
    (lambda (n)
        (let ((old memCount))
            (set! memCount (+ memCount n))
                old)))
                        
(define constantTable `(,*void-object* () ,#t ,#f 0))

(define resetConstantTable
    (lambda ()
        (set! constantTable `(,*void-object* () ,#t ,#f 0))
        (set! memCount START_MEMORY)))

(define addConstants
    (lambda (pe)
        (cond ((or (not (list? pe)) (null? pe)) #f)
              ((equal? (car pe) 'const)
                (cond ((vector? (cadr pe))
                        (begin (vector-map (lambda (el) (addConstants `(const ,el))) (cadr pe))
                               (set! constantTable (append constantTable (cdr pe)))))
                      ((and (list? (cadr pe)) (null? (cadr pe))) #f) 
                      ((pair? (cadr pe))
                        (begin (addConstants `(const ,(caadr pe)))
                               (addConstants `(const ,(cdadr pe)))
                               (set! constantTable (append constantTable (list (cadr pe))))))
                               
                               
                      ((and (number? (cadr pe)) (not (= (/ (denominator (cadr pe)) (gcd (numerator (cadr pe)) (denominator (cadr pe)))) 1)))
                                (let* ((highest (gcd (numerator (cadr pe)) (denominator (cadr pe))))
                                       (numer (/ (numerator (cadr pe)) highest))
                                       (denom (/ (denominator (cadr pe)) highest)))
                                    (begin (addConstants `(const ,numer))
                                           (addConstants `(const ,denom))
                                           (set! constantTable (append constantTable (list (cadr pe)))))))
                               
                               
                               
                      ((string? (cadr pe)) (set! constantTable (append constantTable (list (cadr pe)))))
                      ((symbol? (cadr pe)) (begin (set! constantTable (append constantTable (list (symbol->string (cadr pe)))))
                                                  (set! constantTable (append constantTable (list (cadr pe))))))
                      (else (set! constantTable (append constantTable (list (cadr pe)))))))
                      
              (else (map addConstants pe)))))
                          
(define removeDuplicates
    (lambda (lst)
        (fold-left (lambda (acc el)
                        (if (member el acc)
                            acc
                            (append acc (list el))))
                    '()
                    lst)))
                    
(define removeDuplicatesOfConstTable
    (lambda ()
        (set! constantTable (removeDuplicates constantTable))))
                                      
(define getMemFromTable
    (lambda(c taggedTable)
        (caar (filter 
            (lambda(el)
                (equal? c (cadr el)))
            taggedTable))))
                    
(define addTagAndMem
    (lambda()
        (set! constantTable
            (fold-left
                (lambda (newTable c)
                    (append newTable (list
                        (cond ((equal? c *void-object*) `(,(memCountIncAndGet 1) ,c T_VOID))
                            ((equal? c '()) `(,(memCountIncAndGet 1) ,c T_NIL))
                            ((equal? c #t) `(,(memCountIncAndGet 2) ,#t (T_BOOL 1)))
                            ((equal? c #f) `(,(memCountIncAndGet 2) ,#f (T_BOOL 0)))
                            ((number? c)
                                (let* ((highest (gcd (numerator c) (denominator c)))
                                       (numer (/ (numerator c) highest))
                                       (denom (/ (denominator c) highest)))
                                    (if (= denom 1)
                                        `(,(memCountIncAndGet 2) ,numer (T_INTEGER ,numer))
                                        `(,(memCountIncAndGet 3) ,c (T_FRACTION ,(getMemFromTable numer newTable) ,(getMemFromTable denom newTable))))))
                            ((char? c) `(,(memCountIncAndGet 2) ,c (T_CHAR ,(char->integer c))))
                            ((vector? c)
                                (let ((addresses (map (lambda (el) (getMemFromTable el newTable)) (vector->list c))))
                                    `(,(memCountIncAndGet (+ (vector-length c) 2)) ,c (T_VECTOR ,(vector-length c) ,@addresses))))
                            ((string? c)
                                (let ((ascii (map (lambda (el) (char->integer el)) (string->list c))))
                                    `(,(memCountIncAndGet (+ (string-length c) 2)) ,c (T_STRING ,(string-length c) ,@ascii))))
                            ((symbol? c)
                                `(,(memCountIncAndGet 2) ,c (T_SYMBOL ,(getMemFromTable (symbol->string c) newTable))))
                            ((pair? c)
                                `(,(memCountIncAndGet 3) ,c (T_PAIR ,(getMemFromTable (car c) newTable) ,(getMemFromTable (cdr c) newTable))))
                            (else (error 'c "add tag and mem error"))))))
                '()
                constantTable))))
                                    

(define cMap
    (lambda(proc lst)
        (fold-left
            (lambda(acc el)
                (append acc (list (proc el))))
            '()
            lst)))

(define toString
    (lambda (c)
        (cond ((number? c) (number->string c))
              ((symbol? c) (symbol->string c))
              (else c))))
                                    
(define getConstantTableStr
    (lambda ()
        (let ((i (- START_MEMORY 1)))
            (fold-left
                string-append
                (string-append 
                    "#define    SOB_VOID    " (number->string (getMemFromTable *void-object* constantTable)) "\n"
                    "#define    SOB_NIL     " (number->string (getMemFromTable '() constantTable)) "\n"
                    "#define    SOB_TRUE    " (number->string (getMemFromTable #t constantTable)) "\n"
                    "#define    SOB_FALSE   " (number->string (getMemFromTable #f constantTable)) "\n")

                (cMap (lambda (row)
                        (if (list? (caddr row))
                            (fold-left
                                string-append
                                ""
                                (cMap 
                                    (lambda (el)
                                        (set! i (+ i 1))
                                        (string-append
                                            "MOV(IND("(number->string i)"),"(toString el)");\n"))
                                    (caddr row)))
                            (begin
                                (set! i (+ i 1))
                                (string-append
                                    "MOV(IND("(number->string i)"),"(toString (caddr row))");\n"))))
                    constantTable)))))
                    
(define fvarLibraryFunctions 
    '(cons car cdr map append + apply null? pair? boolean? char? integer? procedure? 
        string? symbol? vector? make-string make-vector not string-length vector-length zero? vector char->integer
        integer->char set-car! set-cdr! string-ref vector-ref symbol->string string->symbol eq? - * / = > < expt
        + list remainder denominator numerator number? rational? string-set! vector-set! ))
                    
(define fvarTable fvarLibraryFunctions)

(define resetFvarTable
    (lambda ()
        (set! fvarTable fvarLibraryFunctions)))

(define addFvars
    (lambda (pe)
        (cond ((or (not (list? pe)) (null? pe)) #f)
              ((equal? (car pe) 'fvar) 
                (set! fvarTable (append fvarTable (list (cadr pe)))))
              (else (cMap addFvars pe)))))
            
(define removeDuplicatesOfFvarTable
    (lambda ()
        (set! fvarTable (removeDuplicates fvarTable))))
            
(define addMemFvar
    (lambda()
        (set! fvarTable
            (fold-left
                (lambda (newTable c)
                        (append newTable (list `(,(memCountIncAndGet 1) ,c 0xDEF))))
                '()
                fvarTable))))
            
(define getFvarTableStr
    (lambda ()
        (fold-left
            string-append
            ""
            (cMap (lambda (row)
                    (string-append
                        "MOV(IND("(number->string (car row))"),"(toString (caddr row))");\n"))
                  fvarTable))))
                  
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; library functions

(define genCons
    (lambda()
        (string-append
            
            "CONS:\n"                       ; fvar cons
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(CONS_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'cons fvarTable))"), R0);\n"
            "JUMP(CONS_EXIT);\n"
            
            "CONS_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "PUSH(FPARG(3));\n"
            "PUSH(FPARG(2));\n"
            "CALL(MAKE_SOB_PAIR);\n"
            "DROP(2);\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "CONS_EXIT:\n")))
            
            
(define genCar
    (lambda()
        (string-append
            "CAR:\n"                       ; fvar cons
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(CAR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'car fvarTable))"), R0);\n"
            "JUMP(CAR_EXIT);\n"
            
            "CAR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0,0), T_PAIR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R0, INDD(R0, 1));\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "CAR_EXIT:\n")))
            
(define genCdr
    (lambda()
        (string-append
            
            "CDR:\n"                       ; fvar cons
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(CDR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'cdr fvarTable))"), R0);\n"
            "JUMP(CDR_EXIT);\n"
            
            "CDR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0,0), T_PAIR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R0, INDD(R0, 2));\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "CDR_EXIT:\n")))
            
(define genNull?
    (lambda()
        (string-append
            "IS_NULL:\n"                       ; fvar cons
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(IS_NULL_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'null? fvarTable))"), R0);\n"
            "JUMP(IS_NULL_EXIT);\n"
            
            "IS_NULL_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(R0, SOB_NIL);\n"
            "JUMP_NE(L_IS_NULL_NOT);\n"
            "MOV(R0, SOB_TRUE);\n" 
            "JUMP(L_IS_NULL_FINISH);\n"
            "L_IS_NULL_NOT:\n"
            "MOV(R0, SOB_FALSE);\n"
            "L_IS_NULL_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "IS_NULL_EXIT:\n")))
            
(define genMap
    (lambda()
        (code-gen (fullOpt (parse
                    '(define map (lambda(proc lst)
                        (if (null? lst)
                            '()
                            (cons (proc (car lst)) (map proc (cdr lst)))))))))))
              
(define genAppend
    (lambda ()
        (code-gen
            (fullOpt
                (parse
                    '(define append
						(letrec ((helper (lambda (lst)
												(cond ((null? lst) lst)
													  ((null? (car lst)) (helper (cdr lst)))
													  ((null? (cdr lst)) (car lst))
													  ((not (pair? (car lst))) (car lst))
													  (else (cons (car (car lst)) (helper (cons (cdr (car lst))
																								(cdr lst)))))))))
							(lambda lst (helper lst))))
							)))))
    
(define genApply
    (lambda()
        (string-append
            "APPLY:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(APPLY_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'apply fvarTable))"), R0);\n"
            "JUMP(APPLY_EXIT);\n"
            
            "APPLY_BODY:\n"
			"PUSH(FP);\n"
            "MOV(FP,SP);\n"
			
            "CMP(FPARG(1), 4);\n"
            "JUMP_GE(L_error_args_count);\n"
            
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0,0), IMM(T_CLOSURE));\n"
            "JUMP_NE(L_error_cannot_apply_non_clos);\n"
            "MOV(R11, FPARG(2));\n"                         ;R11 - f
			"MOV(R3, FPARG(0));\n"
            "MOV(R3, FPARG(0));\n"                 			;R3 - f's env 		
            "MOV(R2, FPARG(-1));\n"                         ;R2 - ret
            "MOV(R1, FPARG(-2));\n"                         ;R1 - FP
            
            "MOV(R4, FPARG(3));\n"                          ;R4 - holds the list
            "MOV(R5, 0);\n"                                 ;R5 - holds the counter
            
            ;checks the list's length
            "APPLY_LENGTH_LOOP:\n"                               
            "CMP(IND(R4), T_PAIR);\n"
            "JUMP_NE(APPLY_LENGTH_LOOP_END);\n"
            "INCR(R5);\n"
            "MOV(R4, INDD(R4,2));\n"
            "JUMP(APPLY_LENGTH_LOOP);\n"
            "APPLY_LENGTH_LOOP_END:\n"
			
			"CMP(IND(R4), T_NIL);\n"
			"JUMP_NE(L_error_must_be_proper_list);\n"
			
            ;"INCR(R5);\n"                                   ;R5 - holds the length
            
			; well.. its a must
			"PUSH(SOB_NIL);\n"
			
            ;push all the list
            "MOV(R4, FPARG(3));\n"                          ;R4 - holds the list
            "APPLY_PUSH_LOOP:\n"                               
            "CMP(IND(R4), T_PAIR);\n"
            "JUMP_NE(APPLY_LENGTH_PUSH_END);\n"
            "PUSH(INDD(R4, 1));\n"
            "MOV(R4, INDD(R4,2));\n"
            "JUMP(APPLY_PUSH_LOOP);\n"
            "APPLY_LENGTH_PUSH_END:\n"
            ;"PUSH(R4);\n"
			
            "MOV(R6,-3);\n"
            "SUB(R6, R5);\n"                                ;R6 - pointer to up
            "MOV(R7, -4);\n"                                ;R7 - pointer to down
            "MOV(R8, R5);\n"
            "DIV(R8, 2);\n"                                 ;R8 - holds number of loops
            
            
            "APPLY_STACK_LOOP_IN_PLACE:\n"
            "CMP(R8, 0);\n"
            "JUMP_EQ(APPLY_STACK_LOOP_IN_PLACE_END);\n"
            "MOV(R4, FPARG(R6));\n"
            "MOV(FPARG(R6), FPARG(R7));\n"
            "MOV(FPARG(R7), R4);\n"
            "INCR(R6);\n"
            "DECR(R7);\n"
            "DECR(R8);\n"
            "JUMP(APPLY_STACK_LOOP_IN_PLACE);\n"

            "APPLY_STACK_LOOP_IN_PLACE_END:\n"
			
            "INCR(R5);\n"
            "PUSH(R5);\n"
            "PUSH(R3);\n"
            "PUSH(R2);\n"

			
            "MOV(R2, -3);\n"                            ;R2 - pointer to FPARG
            "MOV(R3, R5);\n"                            ;R3 - number of loops
            "ADD(R3, 3);\n"
            "DROP(R3);\n"
            "DROP(FPARG(1));\n"
            "DROP(4);\n"
            
            "APPLY_STACK_LOOP_MOVE_BOTTOM_STACK:\n"
            "CMP(R3, 0);\n"
            "JUMP_EQ(APPLY_STACK_LOOP_MOVE_BOTTOM_STACK_END)"
            "PUSH(FPARG(R2));\n"
            "DECR(R2);\n"
            "DECR(R3);\n"
            "JUMP(APPLY_STACK_LOOP_MOVE_BOTTOM_STACK);\n"
            
            "APPLY_STACK_LOOP_MOVE_BOTTOM_STACK_END:\n"
			"MOV(FP, R1);\n"
			
			"JUMPA(INDD(R11, 2));\n"
            
            "APPLY_EXIT:\n")))
            
(define genMinus
    (lambda ()
        (string-append
            "MINUS:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(MINUS_BODY));\n"
            "MOV(IND("(toString (getMemFromTable '- fvarTable))"), R0);\n"
            "JUMP(MINUS_EXIT);\n"
            
            "MINUS_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(1, FPARG(1));\n"
            "JUMP_GE(L_error_args_count);\n"
            
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			
			;transform each integer to fraction
            "MINUS_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(MINUS_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_EQ(MINUS_frac_loop);\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP(MINUS_frac_loop);\n"
            ;end transform
            "MINUS_frac_loop_end:\n"
			
            ; Minus                                                 ; a/b - c/d
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "SUB(R1, 2);\n"
            "MOV(R2, 3);\n"          ;R2 - pointer to FPARG

            "MOV(R5, INDD(FPARG(2), 1));\n" 
            "MOV(R3, INDD(R5,1));\n"        ;R3 - numerator
            "MOV(R5, INDD(FPARG(2), 2));\n"
            "MOV(R4, INDD(R5,1));\n"        ;R4 - denominator
            
            "CMP(R1, 0);\n"
            "JUMP_NE(MINUS_minus_loop);\n"
            
            "MOV(R6, R3);\n"
            "SUB(R3, R6);\n"
            "SUB(R3, R6);\n"
            "JUMP(MINUS_minus_loop_end);\n"
            
            "MINUS_minus_loop:"
            
            "MOV(R6, R4);\n"                ; R6 - b
            
            "MOV(R5, INDD(FPARG(R2), 2));\n"; R5 - (int,d)
			
            "MUL(R3, INDD(R5,1));\n"        ; R3 - a*=d
            "MUL(R4, INDD(R5,1));\n"        ; R4 - b*=d
            
            "MOV(R5, INDD(FPARG(R2), 1));\n"; R5 - (int,c)
			
			"MOV(R7, INDD(R5,1));\n"
            "MUL(R7, R6);\n"        ; c*=b
			"SUB(R3, R7);\n"        ; a-=c
            
            "DECR(R1);\n"
            "INCR(R2);\n"
            
            "CMP(R1, 0);\n"
            "JUMP_NE(MINUS_minus_loop);\n"
            
            "MINUS_minus_loop_end:\n"
            "PUSH(R3);\n"
            "PUSH(R4);\n"
            
            "CALL(MAKE_SOB_FRACTION);\n"
            "DROP(2);\n"
            "POP(FP);\n"
            "RETURN;\n"
            "MINUS_EXIT:"
            )))
            
(define genPlus
    (lambda ()
        (code-gen
            (fullOpt
                (parse
                    '(define +
                        (lambda exps
                            (cond ((null? exps) 0)
                                  ((null? (cdr exps)) (car exps))
                                  (else (apply - (append (list (car exps)) (map (lambda (x) (- 0 x)) (cdr exps)))))
                                )))
                )))))
    
(define genMul
    (lambda ()
        (string-append
            "MUL:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(MUL_BODY));\n"
            "MOV(IND("(toString (getMemFromTable '* fvarTable))"), R0);\n"
            "JUMP(MUL_EXIT);\n"
            
            "MUL_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			
			;transform each integer to fraction
            "MUL_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(MUL_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_EQ(MUL_frac_loop);\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP(MUL_frac_loop);\n"
            ;end transform
            "MUL_frac_loop_end:\n"
			
            ;Mul each frac
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "DECR(R1);\n"
            "MOV(R2, 2);\n"          ;R2 - pointer to FPARG
            "MOV(R3, 1);\n"
            "MOV(R4, 1);\n"
            "MUL_mul_loop:"
            "CMP(R1, 0);\n"
            "JUMP_EQ(MUL_mul_loop_end);\n"
            "MOV(R5, INDD(FPARG(R2), 1));\n"
            "MUL(R3, INDD(R5,1));\n"        ;R3 - mone
            "MOV(R5, INDD(FPARG(R2), 2));\n"
            "MUL(R4, INDD(R5,1));\n"        ;R4 - mechane
            
            "DECR(R1);\n"
            "INCR(R2);\n"
            "JUMP(MUL_mul_loop);\n"
            
            "MUL_mul_loop_end:\n"
            "PUSH(R3);\n"
            "PUSH(R4);\n"
            
            "CALL(MAKE_SOB_FRACTION);\n"
            "DROP(2);\n"
            "POP(FP);\n"
            "RETURN;\n"
            "MUL_EXIT:\n"
            )))
            
(define genExpt
    (lambda ()
        (string-append
            "EXPT:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(EXPT_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'expt fvarTable))"), R0);\n"
            "JUMP(EXPT_EXIT);\n"
            
            "EXPT_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
			"PUSH(INDD(FPARG(3),1));\n"
			"PUSH(INDD(FPARG(2),1));\n"
			"CALL(POWER);\n"
			"DROP(2);\n"
			"PUSH(R0);\n"
			"CALL(MAKE_SOB_INTEGER);\n"
			"DROP(1);\n"
			
            "POP(FP);\n"
            "RETURN;\n"
            "EXPT_EXIT:\n"
            )))
			
(define Comparisons
    (lambda (label tag jumpCondition)
        (string-append
            label":\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL("label"_BODY));\n"
            "MOV(IND("(toString (getMemFromTable tag fvarTable))"), R0);\n"
            "JUMP("label"_EXIT);\n"
            
            label"_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 1);\n"
            "JUMP_EQ(L_error_args_count);\n"
            
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			
			;transform each integer to fraction
            label"_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ("label"_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_EQ("label"_frac_loop);\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP("label"_frac_loop);\n"
            ;end transform
            label"_frac_loop_end:\n"
            
            ; Minus the two fractions                               ; a/b - c/d
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "SUB(R1, 2);\n"
            "MOV(R2, 3);\n"          ;R2 - pointer to FPARG
            
            "MOV(R5, INDD(FPARG(2), 1));\n" 
            "MOV(R3, INDD(R5,1));\n"        ; R3 - a
            "MOV(R5, INDD(FPARG(2), 2));\n"
            "MOV(R4, INDD(R5,1));\n"        ; R4 - b
            
            "CMP(R1, 0);\n"
            "JUMP_EQ("label"_RETURN_TRUE);\n"

            label"_minus_loop:"
            
            "MOV(R6, R4);\n"                ; R6 - b
            
            "MOV(R5, INDD(FPARG(R2), 2));\n"; R5 - (int,d)
            
            "MUL(R3, INDD(R5,1));\n"        ; R3 - a*=d
            "MUL(R4, INDD(R5,1));\n"        ; R4 - b*=d
            
            "MOV(R5, INDD(FPARG(R2), 1));\n"; R5 - (int,c)
            "MOV(R7, INDD(R5,1));\n"        ; R7 - c
            "MUL(R7, R6);\n"        ; c*=b
            
            "SUB(R3, R7);\n"        ; a-=c
            
            "DECR(R1);\n"
            
            "CMP(R3, 0);\n"
            "JUMP_"jumpCondition"("label"_RETURN_FALSE);\n"
            
            "CMP(R1, 0);\n"
            "JUMP_EQ("label"_RETURN_TRUE);\n"
            
            "MOV(R3, INDD(FPARG(R2), 1));\n"; R3 - (int,c)
            "MOV(R3, INDD(R3,1));\n"        ; R3 - c
            "MOV(R4, INDD(FPARG(R2), 2));\n"; R4 - (int,d)
            "MOV(R4, INDD(R4,1));\n"        ; R4 - c
            
            "INCR(R2);\n"
            
            "JUMP("label"_minus_loop);\n"
     
            label"_RETURN_FALSE:\n"
            "MOV(R0,SOB_FALSE);\n"
            "JUMP("label"_FINISH);\n"
            
            label"_RETURN_TRUE:\n"
            "MOV(R0,SOB_TRUE);\n"
            
            label"_FINISH:\n"
            
            "POP(FP);\n"
            "RETURN;\n"
            label"_EXIT:\n"
            )))
            
(define genEqual
    (lambda()
        (Comparisons "EQUAL" '= "NE")))
        
(define genSmallerThan
    (lambda()
        (Comparisons "SMALLER_THAN" '< "GE")))
        
(define genBiggerThan
    (lambda()
        (Comparisons "BIGGER_THAN" '> "LE")))
		
; returns a fraction after gcd even if the denom is 1
(define genMakeFraction
	(lambda()
		(string-append
  "JUMP(MAKE_FRACTION_EXIT);\n"
  "MAKE_FRACTION:\n"
  "PUSH(FP);\n"
  "MOV(FP, SP);\n"
  "PUSH(R1);\n"
  "PUSH(R2);\n"
  "PUSH(R3);\n"
  
  "MOV(R1, FPARG(0));\n"                        ;R1 - denominator
  "MOV(R2, FPARG(1));\n"                        ;R2 - nominator
  "CMP(R1, 0);\n"
  "JUMP_EQ(MAKE_FRACTION_ERROR);\n"
  
  
  "PUSH(R1);\n"
  "PUSH(R2);\n"
  
  "PUSH(R1);\n"
  "CALL(MAKE_SOB_INTEGER);\n"
  "DROP(1);\n"
  "PUSH(R0);\n"
  
  "PUSH(R2);\n"
  "CALL(MAKE_SOB_INTEGER);\n"
  "DROP(1);\n"
  "PUSH(R0);\n"
  
  "CALL(GCD);\n"
  "DROP(2);\n"
  
  "POP(R2);\n"
  "POP(R1);\n"
  "DIV(R2, INDD(R0,1));\n"
  "DIV(R1, INDD(R0,1));\n"
  
  "PUSH(R2);\n"
  "CALL(MAKE_SOB_INTEGER);\n"
  "DROP(1);\n"
  "MOV(R2, R0);\n"
  
  "PUSH(R1);\n"
  "CALL(MAKE_SOB_INTEGER);\n"
  "DROP(1);\n"
  "MOV(R1, R0);\n"
  
  "CMP(INDD(R1, 1), 0);\n"
  "JUMP_GE(MAKE_FRACTION_NO_NEG);\n"
  
  "PUSH(INDD(R1, 1));\n"
  "CALL(ABS);\n"
  "DROP(1);\n"
  "MOV(INDD(R1,1), R0);\n"
  "MOV(R3, 1);\n"
  "NEG(R3);\n"
  "INCR(R3);"
  "MUL(INDD(R2, 1), R3);\n"
  
  
  "MAKE_FRACTION_NO_NEG:\n"
  "PUSH(IMM(3));\n"
  "CALL(MALLOC);\n"
  "DROP(1);\n"
  
  "MOV(IND(R0), T_FRACTION);\n"
  "MOV(INDD(R0, 1), R2);\n"
  "MOV(INDD(R0, 2), R1);\n"
"MAKE_FRACTION_END:\n"
  "POP(R3);\n"
  "POP(R2);\n"
  "POP(R1);\n"
  "POP(FP);\n"
  "RETURN;\n"

  
"MAKE_FRACTION_ERROR:\n"
    "SHOW(\"FATAL ERROR CANNOT DIVIDE BY ZERO\", R0);\n"
    
"MAKE_FRACTION_EXIT:\n")))

  
  
  
	
	
(define genDiv
    (lambda ()
        (string-append
            "DIV:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(DIV_BODY));\n"
            "MOV(IND("(toString (getMemFromTable '/ fvarTable))"), R0);\n"
            "JUMP(DIV_EXIT);\n"
            
            "DIV_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 1);\n"
            "JUMP_EQ(L_error_args_count);\n"
            
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			

			;transform each integer to fraction
            "DIV_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(DIV_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_EQ(DIV_frac_loop);\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP(DIV_frac_loop);\n"
            ;end transform
            
            
            "DIV_frac_loop_end:\n"
            
  
            ;if there is only one element
            "CMP(FPARG(1), 2);\n"
            "JUMP_EQ(DIV_ONE_ELEMENT);\n"

            
            ; replace numerator and denominator
            "MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "SUB(R1, 2);\n"
            "MOV(R2, 3);\n"          ;R2 - pointer to FPARG
            
			"DIV_div_loop:\n"
            "CMP(0, R1);\n"
            "JUMP_GE(DIV_div_loop_end);\n"
            
            "MOV(R5, INDD(FPARG(R2), 1));\n"
            "MOV(R6, INDD(FPARG(R2), 2));\n"
			
			"PUSH(INDD(R6,1));\n"
            "PUSH(INDD(R5,1));\n"
			"CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
			
            "DECR(R1);\n"
            "INCR(R2);\n"
            "JUMP(DIV_div_loop);\n"
            
            "DIV_div_loop_end:\n"
            "POP(FP);\n"
            "JUMP(MUL_BODY);\n"
            
            "DIV_ONE_ELEMENT:\n"

            "MOV(R5, INDD(FPARG(2), 2));\n"
            "PUSH(INDD(R5, 1));\n"
			"MOV(R5, INDD(FPARG(2), 1));\n"
            "PUSH(INDD(R5, 1));\n"
			
            "CALL(MAKE_SOB_FRACTION);\n"
            "DROP(2);\n"
            "POP(FP);\n"
            "RETURN;\n"
            "DIV_EXIT:\n")))
            
(define genList
    (lambda()
        (code-gen (fullOpt (parse
                    '(define list (lambda x x)))))))
            

(define typeValidatorGen
    (lambda (label tag)
        (string-append
            label":\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL("label"_BODY));\n"
            "MOV(IND("(toString (getMemFromTable tag fvarTable))"), R0);\n"
            "JUMP("label"_EXIT);\n"
            
            label"_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(IND(R0), T_"label");\n"
            "JUMP_NE(L_"label"_NOT);\n"
            "MOV(R0, SOB_TRUE);\n" 
            "JUMP(L_"label"_FINISH);\n"
            "L_"label"_NOT:\n"
            "MOV(R0, SOB_FALSE);\n"
            "L_"label"_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            label"_EXIT:\n")))
        
(define genPair?
    (lambda()
        (typeValidatorGen "PAIR" 'pair?)))
        
(define genBoolean?
    (lambda()
        (typeValidatorGen "BOOL" 'boolean?)))
        
(define genChar?
    (lambda()
        (typeValidatorGen "CHAR" 'char?)))
        
(define genInteger?
    (lambda()
        (typeValidatorGen "INTEGER" 'integer?)))
        
(define genProcedure?
    (lambda()
        (typeValidatorGen "CLOSURE" 'procedure?)))
        
(define genString?
    (lambda()
        (typeValidatorGen "STRING" 'string?)))
        
(define genSymbol?
    (lambda()
        (typeValidatorGen "SYMBOL" 'symbol?)))
        
(define genVector?
    (lambda()
        (typeValidatorGen "VECTOR" 'vector?)))
        
(define genNot
    (lambda ()
        (string-append
            "NOT:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(NOT_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'not fvarTable))"), R0);\n"
            "JUMP(NOT_EXIT);\n"
            
            "NOT_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(R0, SOB_FALSE);\n"
            "JUMP_EQ(L_IS_FALSE);\n"
            "MOV(R0, SOB_FALSE);\n" 
            "JUMP(L_NOT_FINISH);\n"
            "L_IS_FALSE:\n"
            "MOV(R0, SOB_TRUE);\n"
            "L_NOT_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "NOT_EXIT:\n")))

(define genMakeString
    (lambda()
        (string-append
            "MAKE_STRING:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(MAKE_STRING_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'make-string fvarTable))"), R0);\n"
            "JUMP(MAKE_STRING_EXIT);\n"
            
            "MAKE_STRING_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            
            
            "CMP(FPARG(1), 4);\n"
            "JUMP_GE(L_error_args_count);\n"
            "CMP(1, FPARG(1));\n"
            "JUMP_GE(L_error_args_count);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_EQ(MAKE_STRING_3_PARAMETERS);\n"           
            
            ; need a fix because there is only a number which is entered
            "POP(R1);\n"                ; FP
            "POP(R2);\n"                ; ret
            "POP(R3);\n"                ; env
            "POP(R4);\n"                ; n
            "POP(R5);\n"                ; arg0
            
            "PUSH(0);\n"
            "CALL(MAKE_SOB_CHAR);\n"
            "DROP(1);\n"
            
            "PUSH(R0);\n"
            "PUSH(R5);\n"
            "INCR(R4);\n"
            "PUSH(R4);\n"
            "PUSH(R3);\n"
            "PUSH(R2);\n"
            "PUSH(R1);\n"
            "INCR(FP);\n"
            
            "MAKE_STRING_3_PARAMETERS:\n" ; with 3 parameters
            
            
            "MOV(R1, FPARG(3));\n"
            "CMP(IND(R1), T_CHAR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "CMP(-1, INDD(R1,1));\n"
            "JUMP_GE(L_error_non_positive_number);\n"
            
            "MOV(R3, FPARG(3));\n"
            
            "MOV(R1, INDD(R1,1));\n"            ; R1 - holds the number of chars we should have
            "MOV(R2, R1);\n"
            "MAKE_STRING_LOOP:\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(MAKE_STRING_FINISH);\n"
            "PUSH(INDD(R3,1));\n"
            "DECR(R1);\n"
            "JUMP(MAKE_STRING_LOOP);\n"
            
            "MAKE_STRING_FINISH:\n"
            "PUSH(R2);\n"
            "CALL(MAKE_SOB_STRING);\n"
            "DROP(1);\n"
            "DROP(R2);\n"
            
            "POP(FP);\n"
            "RETURN;\n"
            
            "MAKE_STRING_EXIT:\n")))
            
(define genMakeVector
    (lambda()
        (string-append
            "MAKE_VECTOR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(MAKE_VECTOR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'make-vector fvarTable))"), R0);\n"
            "JUMP(MAKE_VECTOR_EXIT);\n"
            
            "MAKE_VECTOR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
                        
            "CMP(FPARG(1), 4);\n"
            "JUMP_GE(L_error_args_count);\n"
            "CMP(1, FPARG(1));\n"
            "JUMP_GE(L_error_args_count);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_EQ(MAKE_VECTOR_3_PARAMETERS);\n"           
            
            ; need a fix because there is only a number which is enterned
            "POP(R1);\n"                ; FP
            "POP(R2);\n"                ; ret
            "POP(R3);\n"                ; env
            "POP(R4);\n"                ; n
            "POP(R5);\n"                ; arg0
            
            "PUSH(0);\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP(1);\n"
            
            "PUSH(R0);\n"
            "PUSH(R5);\n"
            "INCR(R4);\n"
            "PUSH(R4);\n"
            "PUSH(R3);\n"
            "PUSH(R2);\n"
            "PUSH(R1);\n"
            "INCR(FP);\n"
            
            "MAKE_VECTOR_3_PARAMETERS:\n" ; with 3 parameters
            
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "CMP(-1, INDD(R1,1));\n"
            "JUMP_GE(L_error_non_positive_number);\n"
            
            "MOV(R3, FPARG(3));\n"
            
            "MOV(R1, INDD(R1,1));\n"            ; R1 - holds the number of chars we should have
            "MOV(R2, R1);\n"
            "MAKE_VECTOR_LOOP:\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(MAKE_VECTOR_FINISH);\n"
            "PUSH(R3);\n"
            "DECR(R1);\n"
            "JUMP(MAKE_VECTOR_LOOP);\n"
            
            "MAKE_VECTOR_FINISH:\n"
            "PUSH(R2);\n"
            "CALL(MAKE_SOB_VECTOR);\n"
            "DROP(1);\n"
            "DROP(R2);\n"
            
            
            "POP(FP);\n"
            "RETURN;\n"
            
            "MAKE_VECTOR_EXIT:\n")))
         
(define ComplicatedLength
    (lambda(label tag)
        (string-append
            label"_LENGTH:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL("label"_LENGTH_BODY));\n"
            "MOV(IND("(toString (getMemFromTable tag fvarTable))"), R0);\n"
            "JUMP("label"_LENGTH_EXIT);\n"
            
            label"_LENGTH_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
                        
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_"label");\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "PUSH(INDD(R1,1));\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP(1);\n"
            
            "POP(FP);\n"
            "RETURN;\n"
            
            label"_LENGTH_EXIT:\n")))
            
(define genStringLength
    (lambda ()
        (ComplicatedLength "STRING" 'string-length)))
        
(define genVectorLength
    (lambda ()
        (ComplicatedLength "VECTOR" 'vector-length)))
            
(define genZero?
    (lambda()
        (string-append
            "IS_ZERO_START:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(IS_ZERO_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'zero? fvarTable))"), R0);\n"
            "JUMP(IS_ZERO_EXIT);\n"
            
            "IS_ZERO_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
                        
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
			
            "CMP(IND(R1), T_INTEGER);\n"
            "JUMP_EQ(IS_ZERO_CHECK_INTEGER);\n"
            
            "CMP(IND(R1), T_FRACTION);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
			"MOV(R1, INDD(R1,1))\n"
			
			"IS_ZERO_CHECK_INTEGER:\n"
            "CMP(INDD(R1,1), 0);\n"
            "JUMP_EQ(IS_ZERO_TRUE);\n"
            "MOV(R0, SOB_FALSE);\n"
            "JUMP(IS_ZERO_FINISH);\n"
            
            "IS_ZERO_TRUE:\n"
            "MOV(R0, SOB_TRUE);\n"
            
            "IS_ZERO_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "IS_ZERO_EXIT:\n")))
            
(define genVector
    (lambda()
        (string-append
            "CREATE_VECTOR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(CREATE_VECTOR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'vector fvarTable))"), R0);\n"
            "JUMP(CREATE_VECTOR_EXIT);\n"
            
            "CREATE_VECTOR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
			
			; if there are no elements
			"CMP(FPARG(1), 1);\n"
			"JUMP_EQ(CREATE_VECTOR_END_SWITCHING);\n"
            
            ; switching the elements in place
            "MOV(R1, 2);\n"             ; R1 - pointer to the first element
            "MOV(R2, FPARG(1));\n"      ; R2 - pointer to the last element
            ;"INCR(R2);\n"
            "MOV(R3, FPARG(1));\n"      ; R3 - number of loops we need to do
            "DIV(R3,2);\n"
            
            "CREATE_VECTOR_LOOP:\n"
            "MOV(R4, FPARG(R2));\n"
            "MOV(R5, FPARG(R1));\n"
            "MOV(FPARG(R2), R5);\n"
            "MOV(FPARG(R1), R4);\n"
            
            "INCR(R1);\n"
            "DECR(R2);\n"
            "DECR(R3);\n"
            "CMP(R3, 0);\n"
            "JUMP_NE(CREATE_VECTOR_LOOP);\n"
            ; end switching
			"CREATE_VECTOR_END_SWITCHING:\n"
            
            "POP(R1);\n"                        ; OLD FP
            "POP(R2);\n"                        ; ret
            "POP(R3);\n"                        ; env
            "DECR(FPARG(1));\n"
            
            "CALL(MAKE_SOB_VECTOR);\n"
            
            "INCR(FPARG(1));\n"
            "PUSH(R3);\n"                        ; env
            "PUSH(R2);\n"                        ; ret
            "PUSH(R1);\n"                        ; OLD FP
            
            "POP(FP);\n"
            "RETURN;\n"
            
            "CREATE_VECTOR_EXIT:\n")))

(define ComplicateSet
    (lambda(label tag)
        (string-append
            "SET_"label":\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(SET_"label"_BODY));\n"
            "MOV(IND("(toString (getMemFromTable tag fvarTable))"), R0);\n"
            "JUMP(SET_"label"_EXIT);\n"
            
            "SET_"label"_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 4);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_"label");\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, FPARG(3));\n"
            "CMP(IND(R2), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, INDD(R2,1));\n"
            "CMP(-1, R2);\n"
            "JUMP_GE(L_error_non_positive_number);\n"
            "CMP(R2, INDD(R1,1));\n"
            "JUMP_GE(L_error_invalid_index);\n"
            
            "MOV(R4, FPARG(4));\n"
            
            "ADD(R2, 2);\n"
            "CMP(IND(R1), T_VECTOR);\n"
            "JUMP_EQ(SET_"label"_DISPLACEMENT);\n"
            "MOV(R4, INDD(R4,1));\n"
            
            "SET_"label"_DISPLACEMENT:\n"
            "MOV(INDD(R1, R2), R4);\n"
            
            "MOV(R0, IMM(SOB_VOID));\n"
            
            "POP(FP);\n"
            "RETURN;\n"
            
            "SET_"label"_EXIT:\n")))
            
(define genSetString
    (lambda()
        (ComplicateSet "STRING" 'string-set!)))           
    
(define genSetVector
    (lambda()
        (ComplicateSet "VECTOR" 'vector-set!)))
            
(define genCharToInteger
    (lambda()
        (string-append
            "CHAR_TO_INTEGER:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(CHAR_TO_INTEGER_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'char->integer fvarTable))"), R0);\n"
            "JUMP(CHAR_TO_INTEGER_EXIT);\n"
            
            "CHAR_TO_INTEGER_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_CHAR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "PUSH(INDD(R1,1));\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP(1);\n"
            
            "CHAR_TO_INTEGER_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "CHAR_TO_INTEGER_EXIT:\n")))
            
(define genIntegerToChar
    (lambda()
        (string-append
            "INTEGER_TO_CHAR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(INTEGER_TO_CHAR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'integer->char fvarTable))"), R0);\n"
            "JUMP(INTEGER_TO_CHAR_EXIT);\n"
            
            "INTEGER_TO_CHAR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "PUSH(INDD(R1,1));\n"
            "CALL(MAKE_SOB_CHAR);\n"
            "DROP(1);\n"
            
            "INTEGER_TO_CHAR_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "INTEGER_TO_CHAR_EXIT:\n")))            
            
(define genSetCar
    (lambda()
        (string-append
            "SET_CAR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(SET_CAR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'set-car! fvarTable))"), R0);\n"
            "JUMP(SET_CAR_EXIT);\n"
            
            "SET_CAR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_PAIR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "MOV(INDD(R1,1),FPARG(3));\n"
			"MOV(R0, SOB_VOID);\n"
            
            "SET_CAR_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "SET_CAR_EXIT:\n")))  
            
(define genSetCdr
    (lambda()
        (string-append
            "SET_CDR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(SET_CDR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'set-cdr! fvarTable))"), R0);\n"
            "JUMP(SET_CDR_EXIT);\n"
            
            "SET_CDR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_PAIR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "MOV(INDD(R1,2),FPARG(3));\n"
			"MOV(R0, SOB_VOID);\n"
            
            "SET_CDR_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "SET_CDR_EXIT:\n"))) 
            
(define genStringRef
    (lambda()
        (string-append
            "STRING_REF:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(STRING_REF_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'string-ref fvarTable))"), R0);\n"
            "JUMP(STRING_REF_EXIT);\n"
            
            "STRING_REF_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_STRING);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, FPARG(3));\n"
            "CMP(IND(R2), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, INDD(R2,1));\n"
            "CMP(-1, R2);\n"
            "JUMP_GE(L_error_non_positive_number);\n"
            "CMP(R2, INDD(R1,1));\n"
            "JUMP_GE(L_error_invalid_index);\n"
            
            "ADD(R2,2);\n"
            "PUSH(INDD(R1,R2));\n"
            "CALL(MAKE_SOB_CHAR);\n"
            "DROP(1);\n"
            
            "STRING_REF_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "STRING_REF_EXIT:\n"))) 
            
(define genVectorRef
    (lambda()
        (string-append
            "VECTOR_REF:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(VECTOR_REF_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'vector-ref fvarTable))"), R0);\n"
            "JUMP(VECTOR_REF_EXIT);\n"
            
            "VECTOR_REF_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(IND(R1), T_VECTOR);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, FPARG(3));\n"
            "CMP(IND(R2), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R2, INDD(R2,1));\n"
            "CMP(-1, R2);\n"
            "JUMP_GE(L_error_non_positive_number);\n"
            "CMP(R2, INDD(R1,1));\n"
            "JUMP_GE(L_error_invalid_index);\n"
            
            "ADD(R2,2);\n"
            "MOV(R0,INDD(R1,R2));\n"
            
            "VECTOR_REF_FINISH:\n"
            "POP(FP);\n"
            "RETURN;\n"
            
            "VECTOR_REF_EXIT:\n")))
            
            
(define genSymbol->string
    (lambda ()
        (string-append
            "SYMBOL_STRING:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(SYMBOL_STRING_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'symbol->string fvarTable))"), R0);\n"
            "JUMP(SYMBOL_STRING_EXIT);\n"
            
            "SYMBOL_STRING_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_GE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0, 0), T_SYMBOL);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R0, INDD(R0, 1));\n"
            "POP(FP);\n"
            "RETURN;\n"
            "SYMBOL_STRING_EXIT:\n"
            )))

(define genString->symbol
    (lambda()
        (string-append
            "STRING_SYMBOL:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(STRING_SYMBOL_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'string->symbol fvarTable))"), R0);\n"
            "JUMP(STRING_SYMBOL_EXIT);\n"
            
            "STRING_SYMBOL_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0, 0), T_STRING);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
			"MOV(R3, FPARG(2));\n"                      ;R3 holds the string that we need to compare to
			"MOV(R5, 1);\n"								;boolean
            "MOV(R2, "(toString symTblStartMem)");\n"   ;R2 holds the current link -> start
			"CMP(INDD(R2, 0), 0);\n"
			"JUMP_EQ(L_STRING_SYMBOL_LOOP_endList);\n"
            
            
            "L_STRING_SYMBOL_LOOP:\n"
			"MOV(R5, 1);\n"
            "MOV(R6, INDD(R2, 0));\n"                   ;R6 - link string
			;"MOV(R6, IND(R6));\n"
            "MOV(R7, R3);\n"                            ;R7 - FPARG string
            "MOV(R8, 1);\n"                             ;R8 - pointer to cell in string
            "MOV(R11, INDD(R3, 1));\n"					;R11 - number of loops
            "ADD(R11,1);\n"
            
            "L_STRING_SYMBOL_LOOP_CMP_STRING:\n"
            "CMP(R11, 0);\n"
            "JUMP_EQ(L_STRING_SYMBOL_LOOP_CMP_STRING_CHECK);\n"
            "CMP(INDD(R6, R8), INDD(R7, R8));\n"
            "INCR(R8);\n"
            "DECR(R11);\n"
            "JUMP_EQ(L_STRING_SYMBOL_LOOP_CMP_STRING);\n"
            "MOV(R5, 0);\n"
            "L_STRING_SYMBOL_LOOP_CMP_STRING_CHECK:\n"
            "CMP(R5, 1);\n"
            "JUMP_EQ(L_STRING_SYMBOL_LOOP_match);\n"
            
            
            "CMP(INDD(R2, 1), 0);\n"
			;"SHOW(\"why we jump to end\", INDD(R2, 1));\n"
            "JUMP_EQ(L_STRING_SYMBOL_LOOP_endList);\n"  ;jump if there is no string in the link list that match
            "MOV(R2, INDD(R2, 1));\n"
            "JUMP(L_STRING_SYMBOL_LOOP);\n"
            
            "L_STRING_SYMBOL_LOOP_match:\n"
            "PUSH(2);\n"                                ;create T_SYMBOL and put the string in him
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_SYMBOL);\n"
            "MOV(INDD(R0, 1), R6);\n"
            "JUMP(L_STRING_SYMBOL_end_func);\n"
            
            "L_STRING_SYMBOL_LOOP_endList:\n"
			"SHOW(\"end list\", R1);\n"
			"SHOW(\"boolean\", R5);\n"
            "PUSH(2);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0,0), R3);\n"
            "MOV(INDD(R0,1), 0);\n"
			
			"CMP(INDD(R2,0), 0);\n"
			"JUMP_NE(L_STRING_SYMBOL_NOT_FIRST);\n"
			"MOV(INDD(R2,0), R0);\n"
			"JUMP(L_STRING_SYMBOL_FIRST);\n"
			"L_STRING_SYMBOL_NOT_FIRST:\n"
			"MOV(INDD(R2,1), R0);\n"
			;"SHOW(\"next is not null\", R5);\n"
			"L_STRING_SYMBOL_FIRST:\n"
            "PUSH(2);\n"                                ;create T_SYMBOL and put the new string in him
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_SYMBOL);\n"
            "MOV(R4, INDD(R2, 0));\n"
			;"SHOW(\"R4\", R4);\n"
            "MOV(INDD(R0, 1), IND(R4));\n"
            
            
            
            "JUMP(L_STRING_SYMBOL_end_func);\n"
            "L_STRING_SYMBOL_end_func:\n"
            "POP(FP);\n"
			;"SHOW(\"return of the symTab\", R0);\n"
            "RETURN;\n"
            "STRING_SYMBOL_EXIT:\n"

            )))
            
            
(define genRemainder
    (lambda ()
        (string-append
            "REMAINDER:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(REMAINDER_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'remainder fvarTable))"), R0);\n"
            "JUMP(REMAINDER_EXIT);\n"
            
            "REMAINDER_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, FPARG(2));\n"
            "CMP(INDD(R0, 0), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            "MOV(R1, FPARG(3));\n"
            "CMP(INDD(R1, 0), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
            
            "MOV(R0, INDD(R0, 1));\n"
            "MOV(R1, INDD(R1, 1));\n"
            "REM(R0, R1);\n"
            "PUSH(R0);\n"
            "CALL(MAKE_SOB_INTEGER);\n"
            "DROP(1);\n"
            "POP(FP);\n"
            "RETURN;\n"
            "REMAINDER_EXIT:\n"
            )))
            
            
(define genDenominator
    (lambda ()
        (string-append
            "DENOMINATOR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(DENOMINATOR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'denominator fvarTable))"), R0);\n"
            "JUMP(DENOMINATOR_EXIT);\n"
            
            "DENOMINATOR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            
			"MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			
			
			;transform each integer to fraction
            "DENOMINATOR_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(DENOMINATOR_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_NE(DENOMINATOR_NOT_FRACTION);\n"
			"PUSH(INDD(INDD(FPARG(R2),1),1));\n"
            "PUSH(INDD(INDD(FPARG(R2),2),1));\n"
			"JUMP(DENOMINATOR_CREATE_FRACTION);\n"
			"DENOMINATOR_NOT_FRACTION:\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
			"PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "DENOMINATOR_CREATE_FRACTION:\n"
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP(DENOMINATOR_frac_loop);\n"
            ;end transform
			
            "DENOMINATOR_frac_loop_end:\n"
			
            "MOV(R0, INDD(FPARG(2), 2));\n"
            "POP(FP);\n"
            "RETURN;\n"
            "DENOMINATOR_EXIT:\n"
            )))
            
(define genNumerator
 (lambda ()
        (string-append
            "NUMERATOR:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(NUMERATOR_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'numerator fvarTable))"), R0);\n"
            "JUMP(NUMERATOR_EXIT);\n"
            
            "NUMERATOR_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            
			"MOV(R1, FPARG(1));\n"   ;R1 - num of loops
            "MOV(R2, 1);\n"          ;R2 - pointer to FPARG
			
			;transform each integer to fraction
            "NUMERATOR_frac_loop:\n"
            "DECR(R1);\n"
            "INCR(R2);\n"
            "CMP(R1, 0);\n"
            "JUMP_EQ(NUMERATOR_frac_loop_end);\n"
            "CMP(IND(FPARG(R2)), T_FRACTION);\n"
            "JUMP_NE(NUMERATOR_NOT_FRACTION);\n"
			"PUSH(INDD(INDD(FPARG(R2),1),1));\n"
            "PUSH(INDD(INDD(FPARG(R2),2),1));\n"
			"JUMP(NUMERATOR_CREATE_FRACTION);\n"
			"NUMERATOR_NOT_FRACTION:\n"
            "CMP(IND(FPARG(R2)), T_INTEGER);\n"
            "JUMP_NE(L_error_type_mismatch);\n"
			"PUSH(INDD(FPARG(R2),1));\n"
            "PUSH(1);\n"			
            "NUMERATOR_CREATE_FRACTION:\n"
            "CALL(MAKE_FRACTION);\n"
            "DROP(2);\n"
            "MOV(FPARG(R2), R0);\n"
            "JUMP(NUMERATOR_frac_loop);\n"
            ;end transform
			
            "NUMERATOR_frac_loop_end:\n"
			
            "MOV(R0, INDD(FPARG(2), 1));\n"
            "POP(FP);\n"
            "RETURN;\n"
            "NUMERATOR_EXIT:\n"
            )))
            
(define genNumber?
    (lambda ()
        (string-append
            "NUMBER:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(NUMBER_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'number? fvarTable))"), R0);\n"
            "JUMP(NUMBER_EXIT);\n"
            
            "NUMBER_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, SOB_TRUE);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(INDD(R1, 0), T_INTEGER);\n"
            "JUMP_EQ(NUMBER_END);\n"
            "CMP(INDD(R1, 0), T_FRACTION);\n"
            "JUMP_EQ(NUMBER_END);\n"
            "MOV(R0, SOB_FALSE);\n"
            "NUMBER_END:\n"
            "POP(FP);\n"
            "RETURN;\n"
            "NUMBER_EXIT:\n")))
            
(define genRational?
    (lambda ()
        (string-append
            "RATIONAL:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(RATIONAL_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'rational? fvarTable))"), R0);\n"
            "JUMP(RATIONAL_EXIT);\n"
            
            "RATIONAL_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 2);\n"
            "JUMP_NE(L_error_args_count);\n"
            "MOV(R0, SOB_TRUE);\n"
            "MOV(R1, FPARG(2));\n"
            "CMP(INDD(R1, 0), T_INTEGER);\n"
            "JUMP_EQ(RATIONAL_END);\n"
            "CMP(INDD(R1, 0), T_FRACTION);\n"
            "JUMP_EQ(RATIONAL_END);\n"
            "MOV(R0, SOB_FALSE);\n"
            "RATIONAL_END:\n"
            "POP(FP);\n"
            "RETURN;\n"
            "RATIONAL_EXIT:\n")))
            
(define genEq?
    (lambda ()
        (string-append
            "EQ:\n"
            "PUSH(3);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(INDD(R0, 0), T_CLOSURE);\n"
            "MOV(INDD(R0, 1), 0XDEF);\n"
            "MOV(INDD(R0, 2), LABEL(EQ_BODY));\n"
            "MOV(IND("(toString (getMemFromTable 'eq? fvarTable))"), R0);\n"
            "JUMP(EQ_EXIT);\n"
            
            "EQ_BODY:\n"
            "PUSH(FP);\n"
            "MOV(FP,SP);\n"
            "CMP(FPARG(1), 3);\n"
            "JUMP_NE(L_error_args_count);\n"
            
            
            "MOV(R0, SOB_FALSE);\n"
            "MOV(R1, FPARG(2));\n"
            "MOV(R2, FPARG(3));\n"
			
			
            "CMP(IND(R1), IND(R2));\n"
            "JUMP_NE(EQ_END);\n"
            "CMP(IND(R2), T_SYMBOL);\n"
            "JUMP_EQ(EQ_FIELD);\n"
            "CMP(IND(R2), T_CHAR);\n"
            "JUMP_EQ(EQ_FIELD);\n"
            "CMP(IND(R2), T_INTEGER);\n"
            "JUMP_EQ(EQ_FIELD);\n"
			
			"CMP(IND(R2), T_FRACTION);\n"
            "JUMP_EQ(EQ_FIELD_FRACTION);\n"
            
            "EQ_ADDRESS:\n"
            "CMP(R1, R2);\n"
            "JUMP_NE(EQ_END);\n"
            "MOV(R0,SOB_TRUE);\n"
            "JUMP(EQ_END);\n"
            
            
            "EQ_FIELD:\n"
            "CMP(INDD(R1, 1), INDD(R2,1));\n"
            "JUMP_NE(EQ_END);\n"
            "MOV(R0,SOB_TRUE);\n"
            "JUMP(EQ_END);\n"
            
            "EQ_FIELD_FRACTION:\n"
			"MOV(R3, INDD(R1, 1))"
			"MOV(R4, INDD(R2,1));\n"
			"CMP(INDD(R3, 1), INDD(R4, 1));\n"
			"JUMP_NE(EQ_END);\n"
			"MOV(R3, INDD(R1, 2))"
			"MOV(R4, INDD(R2,2));\n"
			"CMP(INDD(R3, 1), INDD(R4, 1));\n"
			"JUMP_NE(EQ_END);\n"
			"MOV(R0,SOB_TRUE);\n"
            "JUMP(EQ_END);\n"
			
            "EQ_END:\n"
            "POP(FP);\n"
            "RETURN;\n"
            "EQ_EXIT:\n")))
            
(define genLibraryFunctions
    (lambda ()
        (string-append
            (genCons)
            (genCar)
            (genCdr)
            (genMap)
            (genAppend)
            (genNull?)
            (genPair?)
            (genBoolean?)
            (genChar?)
            (genInteger?)
            (genProcedure?)
            (genString?)
            (genSymbol?)
            (genVector?)
            (genApply)
            (genMakeString)
            (genMakeVector)
            (genNot)
            (genStringLength)
            (genVectorLength)
            (genZero?)
            (genVector)
            (genCharToInteger)
            (genIntegerToChar)
            (genSetCar)
            (genSetCdr)
            (genStringRef)
            (genVectorRef)
            (genSymbol->string)
            (genString->symbol)
            (genList)
			(genMakeFraction)
            (genMul)
            (genDiv)
            (genMinus)
            (genEqual)
            (genSmallerThan)
            (genBiggerThan)
            (genSetString)
            (genSetVector)
            (genPlus)
            (genRemainder)
            (genDenominator)
            (genNumerator)
            (genNumber?)
            (genRational?)
            (genEq?)
			(genExpt)
            )))
    
(define counters '())
    
(define getNextCount
    (lambda (tag)
       (let ((pair (assoc tag counters)))
                (if pair
                    (number->string (begin (set-box! (cdr pair) (+ (unbox (cdr pair)) 1)) (unbox (cdr pair))))
                    (begin (set! counters (append counters (list (cons tag (box 0))))) (getNextCount tag))))))
                                            
                                            
(define genIf
    (lambda(pe)
        (let ((cIf3 (getNextCount 'if3)))
            (string-append
                (code-gen (cadr pe))
                "CMP(R0, IMM(SOB_FALSE));\n"
                "JUMP_EQ(L_if3_else_" cIf3 ");\n"
                (code-gen (caddr pe))
                "JUMP(L_if3_exit_" cIf3 ");\n"
                "L_if3_else_" cIf3 ":\n"
                (code-gen (cadddr pe))
                "L_if3_exit_" cIf3 ":\n"))))
                    
(define genSeq
    (lambda (pe)
        (fold-right
            string-append
            ""
            (map code-gen (cadr pe)))))
        
(define genOr
    (lambda (pe)
        (let ((cOr (getNextCount 'or)))
            (string-append
                (code-gen (caadr pe))
                (fold-right
                    string-append
                    ""
                    (map (lambda (el)
                            (string-append "CMP(R0,IMM(SOB_FALSE));\n"
                                        "JUMP_NE(L_or_exit_" cOr ");\n"
                                        (code-gen el)))
                        (cdadr pe)))
                "L_or_exit_" cOr ":\n"))))
                
(define genApp
    (lambda (pe)
        (let ((args (reverse (caddr pe))))
        (string-append
            "PUSH(SOB_NIL);\n"
            (fold-right
                string-append
                ""
                (map (lambda (el)
                    (string-append
                        (code-gen el)
                        "PUSH(R0)\n"))
                 args))
            "PUSH(" (number->string (+ (length args) 1)) ")\n"
			(code-gen (cadr pe))
            "CMP(IND(R0), IMM(T_CLOSURE));\n"
			
            "JUMP_NE(L_error_cannot_apply_non_clos);\n"
            "PUSH(INDD(R0,1))\n"            ;env
			"CALLA(INDD(R0,2));\n"
			
            "DROP(1)\n"
            "POP(R1)\n"
            "DROP(R1)\n"
            ))))
            
(define genTcApp
    (lambda (pe)
        (let ((args (reverse (caddr pe)))
              (cLs (getNextCount 'tca)))
        (string-append
            "PUSH(SOB_NIL);\n"
            (fold-right
                string-append
                ""
                (cMap (lambda (el)
                    (string-append
                        (code-gen el)
                        "PUSH(R0)\n"))
                 args))
            "PUSH(" (number->string (+ (length args) 1)) ")\n"
            (code-gen (cadr pe))
            "CMP(IND(R0), IMM(T_CLOSURE));\n"
            "JUMP_NE(L_error_cannot_apply_non_clos);\n"
            "PUSH(INDD(R0,1))\n"            ;env
            "PUSH(FPARG(-1));\n"
            
            "MOV(R1, FPARG(-2));\n"                                     ; R1 holds the old FP
            "MOV(R2, -3);\n"                                             ; R2 holds the bottom of new stack
            "MOV(R3, "(number->string (+ (length args) 4))");\n"        ; R3 holds how many times we loop
            
            "DROP("(number->string (+ (length args) 4))");\n"
            
            "DROP(FPARG(1));\n"
            "DROP(4);\n"
            
            ;Copy the up stack to down stack
            "L_tc_applic_loop_"cLs":\n"
            "CMP(R3, 0);\n"
            "JUMP_EQ(L_tc_applic_loop_end_"cLs");\n"
            
            "PUSH(FPARG(R2));\n"
            "DECR(R2);\n"
            "DECR(R3);\n"
            "JUMP(L_tc_applic_loop_"cLs");\n"
            "L_tc_applic_loop_end_"cLs":\n"
            
            "MOV(FP, R1);\n"
            
            "JUMPA(INDD(R0,2));\n"))))
            
            
(define genLamSimp
    (lambda(pe)
        (set! major (+ major 1))
            (let* ((cLs (getNextCount 'lms))
                  (str (string-append
                    "MOV(R1,FPARG(0));\n" ; old env
                    "PUSH(" (number->string major) ")\n" ; the newely constructed env
                    "CALL(MALLOC);\n"
                    "DROP(1)\n"
                    "MOV(R2,R0);\n" 								; R2 - address of malloc
                    "MOV(R4,0);\n"
                    "MOV(R5,1);\n"
                    "L_lambda_simple_for_" cLs ":\n"
                    "CMP(R4," (number->string (- major 1)) ");\n"
                    "JUMP_GE(L_lambda_simple_end_for_" cLs ");\n"
					"MOV(INDD(R2,R5),INDD(R1,R4));\n"
                    "ADD(R4,1);\n"
                    "ADD(R5,1);\n"
                    "JUMP(L_lambda_simple_for_" cLs ");\n"
                    "L_lambda_simple_end_for_" cLs ":\n"
					
                    "MOV(R3,FPARG(1));\n" ; now R3 holds the number of parameters
                    "PUSH(R3)\n"
                    "CALL(MALLOC);\n"
                    "DROP(1)\n"
                    "MOV(IND(R2),R0);\n"
                    
                    ; this loop is for copying the parameters from the old environment to the newely one
                    "MOV(R4,0);\n"
                    "MOV(R5,2);\n"
                    "L_lambda_simple_for2_" cLs ":\n"
                    "CMP(R4,R3);\n"
                    "JUMP_EQ(L_lambda_simple_end_for2_" cLs ");\n"
                    "MOV(INDD(R0,R4),FPARG(R5));\n"
                    "ADD(R4,1);\n"
                    "ADD(R5,1);\n"
                    "JUMP(L_lambda_simple_for2_" cLs ");\n"
                    "L_lambda_simple_end_for2_" cLs ":\n"
                    
                    ; push to the stack the trio - T_CLOSURE, env, code
                    "PUSH(3)\n"
                    "CALL(MALLOC);\n"
                    "DROP(1)\n"
                    "MOV(IND(R0),T_CLOSURE);\n"
                    "MOV(INDD(R0,1),R2);\n"
                    "MOV(INDD(R0,2),LABEL(L_clos_simp_body_"cLs"));\n"
                    "JUMP(L_clos_simp_exit_" cLs ");\n"
                    
                    
                    "L_clos_simp_body_" cLs ":\n"
                    
                    "PUSH(FP)\n"
                    "MOV(FP, SP);\n"
                    "CMP(FPARG(1), IMM(" (toString (+ (length (cadr pe)) 1)) "));\n"
                    "JUMP_NE(L_error_args_count);\n"
                    (code-gen (caddr pe))
                    "POP(FP)\n"
                    "RETURN;\n"
                    "L_clos_simp_exit_" cLs ":\n")))

            (set! major (- major 1))
        str)))

(define genLamOpt
    (lambda(pe)
        (set! major (+ major 1))
        (let* ((cLs (getNextCount 'lmo))
                (str (string-append
                "MOV(R1,FPARG(0));\n" ; old env
                "PUSH(" (number->string major) ")\n" ; the newely constructed env
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(R2,R0);\n" ; now in R2 we have the address of malloc
                "MOV(R4,0);\n"
                "MOV(R5,1);\n"
                "L_lambda_optional_for_" cLs ":\n"
                "CMP(R4," (number->string (- major 1)) ");\n"
                "JUMP_GE(L_lambda_optional_end_for_" cLs ");\n"
                "MOV(INDD(R2,R5),INDD(R1,R4));\n"
                "ADD(R4,1);\n"
                "ADD(R5,1);\n"
                "JUMP(L_lambda_optional_for_" cLs ");\n"
                "L_lambda_optional_end_for_" cLs ":\n"
                "MOV(R3,FPARG(1));\n" ; now R3 holds the number of parameters
                "PUSH(R3)\n"
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(IND(R2),R0);\n"
                
                ; this loop is for copying the parameters from the old environment to the newely one
                "MOV(R4,0);\n"
                "MOV(R5,2);\n"
                "L_lambda_optional_for2_" cLs ":\n"
                "CMP(R4,R3);\n"
                "JUMP_EQ(L_lambda_optional_end_for2_" cLs ");\n"
                "MOV(INDD(R0,R4),FPARG(R5));\n"
                "ADD(R4,1);\n"
                "ADD(R5,1);\n"
                "JUMP(L_lambda_optional_for2_" cLs ");\n"
                "L_lambda_optional_end_for2_" cLs ":\n"
                
                ; push to the stack the trio - T_CLOSURE, env, code
                "PUSH(3)\n"
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(IND(R0),T_CLOSURE);\n"
                "MOV(INDD(R0,1),R2);\n"
                "MOV(INDD(R0,2),LABEL(L_clos_optional_body_" cLs"));\n"
                "JUMP(L_clos_optional_exit_" cLs ");\n"
                
				
				"L_clos_optional_body_" cLs ":\n"
                "PUSH(FP)\n"
                "MOV(FP, SP);\n"
				
                "CMP("(number->string (length (cadr pe)))", FPARG(1));\n"
                "JUMP_GE(L_error_args_count);\n"
                
                ; fixing the stack for lambda opt
                ; building the list
                "MOV(R1, IMM(SOB_NIL));\n"                          ; R1 holds the list itself which is in construction
                "MOV(R2, FPARG(1));\n"								; FPARG(1) - how many values we truely got
																	; the next line is how many did we expect
																	; FPARG(1) includes the nil
                "SUB(R2, "(number->string (+ (length (cadr pe)) 1))");\n" ; R2 holds how many loops we still need to do
				"MOV(R3, FPARG(1));\n"                                    ; R3 holds how many elements are there
                "L_clos_body_fix_optional_" cLs ":\n"
                "CMP(R2, 0);\n"
                "JUMP_EQ(L_clos_body_fix_optional_end_" cLs ");\n"
                "PUSH(R1);\n"
                "PUSH(FPARG(R3));\n"
                "CALL(MAKE_SOB_PAIR);\n"
                "DROP(2);\n"
                "MOV(R1, R0);\n"
                "DECR(R2);\n"
                "DECR(R3);\n"
                "JUMP(L_clos_body_fix_optional_" cLs ");\n"
                "L_clos_body_fix_optional_end_" cLs ":\n"
                
				
				
				; pushing all elements on top of the stack
				"PUSH(SOB_NIL);\n"
				"PUSH(R1);\n"                                ; copying the list to the bottom of the stack
                
				;"MOV(R4, FPARG(1));\n"                               ; baphoal
						  ; this is how many we can handle
                "MOV(R4, "(number->string (length (cadr pe)))");\n"  ; R4 - position of the now to copy value
				"ADD(R4, 2);\n"
				"MOV(R2, R4);\n"
				"ADD(R2, 3);\n"											   ; R2 - how many loops we need to do
				"MOV(R3, R2);\n"
				"ADD(R3, 5);\n"											   ; R3 - holds how many drop we should do to get to 
																		   ; the bottom of this newely stack
               	"L_clos_body_fix_optional_stack_" cLs ":\n"
				"DECR(R2);\n"
				"DECR(R4);\n"
				"CMP(R2,0);\n"
				"JUMP_EQ(L_clos_body_fix_optional_stack_end_" cLs ");\n"
                "PUSH(FPARG(R4));\n"
				"JUMP(L_clos_body_fix_optional_stack_" cLs ");\n"
				"L_clos_body_fix_optional_stack_end_" cLs ":\n"
				
				; now we start overwriting the old variables with the new one
				"DROP(R3);\n"
				"DROP(FPARG(1));\n"
				
				"MOV(R1, -2);\n" 			; R1 - pointer to the first FPARG we want to copy
				"MOV(R2, R3);\n"			; R2 - how many loops we should do to 
				"SUB(R2, 3);\n"
				"MOV(R3, FPARG(1));\n" 			; R3 - how many drops fp needs to do
				"SUB(R3, R2);\n"
				"ADD(R3, 4);\n"
				 
				
				"L_clos_body_fix_optional_stack_2_" cLs ":\n"
				"DECR(R1);\n"
				"DECR(R2);\n"
				"CMP(R2,0);\n"
				"JUMP_EQ(L_clos_body_fix_optional_stack_end_2_" cLs ");\n"
                "PUSH(FPARG(R1));\n"
				"JUMP(L_clos_body_fix_optional_stack_2_" cLs ");\n"
				"L_clos_body_fix_optional_stack_end_2_" cLs ":\n"
				
				"SUB(FP, R3);\n"
				"DECR(FP);\n"
				"MOV(FPARG(1), "(number->string (+ (length (cadr pe)) 2))")\n"
				
				(code-gen (cadddr pe))
                "POP(FP)\n"
                "RETURN;\n"
                "L_clos_optional_exit_" cLs ":\n")))

            (set! major (- major 1))
        str)))

(define genLamVar
    (lambda(pe)
        (set! major (+ major 1))
        (let* ((cLs (getNextCount 'lmv))
                (str (string-append
                "MOV(R1,FPARG(0));\n" ; old env
                "PUSH(" (number->string major) ")\n" ; the newely constructed env
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(R2,R0);\n" ; now in R2 we have the address of malloc
                "MOV(R4,0);\n"
                "MOV(R5,1);\n"
                "L_lambda_variadic_for_" cLs ":\n"
                "CMP(R4," (number->string (- major 1)) ");\n"
                "JUMP_GE(L_lambda_variadic_end_for_" cLs ");\n"
                "MOV(INDD(R2,R5),INDD(R1,R4));\n"
                "ADD(R4,1);\n"
                "ADD(R5,1);\n"
                "JUMP(L_lambda_variadic_for_" cLs ");\n"
                "L_lambda_variadic_end_for_" cLs ":\n"
                "MOV(R3,FPARG(1));\n" ; now R3 holds the number of parameters
                "PUSH(R3)\n"
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(IND(R2),R0);\n"
                
                ; this loop is for copying the parameters from the old environment to the newely one
                "MOV(R4,0);\n"
                "MOV(R5,2);\n"
                "L_lambda_variadic_for2_" cLs ":\n"
                "CMP(R4,R3);\n"
                "JUMP_EQ(L_lambda_variadic_end_for2_" cLs ");\n"
                "MOV(INDD(R0,R4),FPARG(R5));\n"
                "ADD(R4,1);\n"
                "ADD(R5,1);\n"
                "JUMP(L_lambda_variadic_for2_" cLs ");\n"
                "L_lambda_variadic_end_for2_" cLs ":\n"
                
                ; push to the stack the trio - T_CLOSURE, env, code
                "PUSH(3)\n"
                "CALL(MALLOC);\n"
                "DROP(1)\n"
                "MOV(IND(R0),T_CLOSURE);\n"
                "MOV(INDD(R0,1),R2);\n"
                "MOV(INDD(R0,2),LABEL(L_clos_variadic_body_" cLs"));\n"
                "JUMP(L_clos_variadic_exit_" cLs ");\n"
				
				
                "L_clos_variadic_body_" cLs ":\n"
                "PUSH(FP)\n"
                "MOV(FP, SP);\n"
                ; don't do a thing if received 0 args
                "CMP(FPARG(1), 1);\n"
                "JUMP_EQ(L_clos_body_fix_variadic_stack_end_2_" cLs ");\n"
                
                ; fixing the stack for lambda var
                ; building the list
                "MOV(R1, IMM(SOB_NIL));\n"                          ; R1 holds the list itself which is in construction
                "MOV(R2, FPARG(1));\n"                              ; R2 holds how many loops we still need to do
                "SUB(R2, 1);\n"
                "MOV(R3, FPARG(1));\n"                              ; R3 holds how many elemnts are there
                "L_clos_body_fix_variadic_" cLs ":\n"
                "CMP(R2, 0);\n"
                "JUMP_EQ(L_clos_body_fix_variadic_end_" cLs ");\n"
                "PUSH(R1);\n"
                "PUSH(FPARG(R3));\n"
                "CALL(MAKE_SOB_PAIR);\n"
                "DROP(2);\n"
                "MOV(R1, R0);\n"
                "DECR(R2);\n"
                "DECR(R3);\n"
                "JUMP(L_clos_body_fix_variadic_" cLs ");\n"
                "L_clos_body_fix_variadic_end_" cLs ":\n"
                
                
                ; put the list down there
                "MOV(R4, FPARG(1));\n"                               ; baphoal
                "DECR(R4);\n"
                "MOV(R2, 1);\n"                                      ; R2 holds how many loops we still need to do (from -3 
                                                                     ; to R2)
                                                                     
                "MOV(R3, FPARG(1));\n"                               
                ;"ADD(R3, 1);\n"                            
                "MOV(FPARG(R3), R1);\n"                              ; copying the list to the bottom of the stack (after nil)
                
                "L_clos_body_fix_variadic_stack_" cLs ":\n"
                "DECR(R2);\n"
                "DECR(R3);\n"
                "CMP(R2, -4);\n"
                "JUMP_EQ(L_clos_body_fix_variadic_stack_end_" cLs ");\n"
                "MOV(R5,R3);\n"
                "SUB(R5,R4);\n"
                "ADD(R5, 1);\n"                            
                "MOV(FPARG(R3), FPARG(R5));\n"
                "JUMP(L_clos_body_fix_variadic_stack_" cLs ");\n"
                
                
                "L_clos_body_fix_variadic_stack_end_" cLs ":\n"
                "SUB(FP, R4);\n"
                "ADD(FP, 1);\n"
                
                "DECR(R4);\n"
                "DROP(R4);\n"
                
                "MOV(FPARG(1), 2);\n" ; there are only two variables now
                "L_clos_body_fix_variadic_stack_end_2_" cLs ":\n"
                (code-gen (caddr pe))
                "POP(FP)\n"
                "RETURN;\n"
                "L_clos_variadic_exit_" cLs ":\n")))

            (set! major (- major 1))
        str)))
        
(define genDef
    (lambda(pe)
        (string-append
            (code-gen (caddr pe))
            "MOV(IND(" (toString (getMemFromTable (cadadr pe) fvarTable)) "), R0);\n"
            "MOV(R0, IMM(SOB_VOID));\n")))
            
(define genConst
    (lambda(pe)
        (string-append
                "MOV(R0, " (toString (getMemFromTable (cadr pe) constantTable)) ");\n")))
                
(define genFvar
    (lambda(pe)
        (string-append
                "MOV(R0, IND(" (toString (getMemFromTable (cadr pe) fvarTable)) "));\n")))
                
(define genPvar
    (lambda(pe)
        (string-append
                "MOV(R0, 2);\n"
                "ADD(R0, "(toString (caddr pe))");\n"
                "MOV(R0, FPARG(R0));\n")))
                
(define genBvar
    (lambda (pe)
        (let ((major (caddr pe))
              (minor (cadddr pe)))
        (string-append
            "MOV(R1, FPARG(0));\n"
            "MOV(R1, INDD(R1,"(toString major)"));\n"
            "MOV(R0, INDD(R1,"(toString minor)"));\n"))))

(define genSet
    (lambda (pe)
            (let ((tag (caadr pe))
                  (var (cadadr pe))
                  (val (code-gen (caddr pe))))
                 (cond ((equal? tag 'pvar) 
                            (let ((minor (car (cddadr pe))))
                              (string-append
                                  val
                                  "MOV(R1, 2);\n"
                                  "ADD(R1,"(toString minor)");\n"
                                  "MOV(FPARG(R1), R0);\n"
								  ;"SHOW(\"\",FPARG(R1));\n"
                                  "MOV(R0, IMM(SOB_VOID));\n")))
                       ((equal? tag 'bvar)
                            (let ((minor (car (cddadr pe)))
                                  (major (car(cdr (cddadr pe)))))
                              (string-append
                                  val
                                  "MOV(R1, FPARG(0));\n"
                                  "MOV(R1, INDD(R1,"(toString major)"));\n"
                                  "MOV(INDD(R1,"(toString minor)"), R0);\n"
                                  "MOV(R0, IMM(SOB_VOID));\n")))
                       ((equal? tag 'fvar) 
                            (string-append
                                val
                                "MOV(IND(" (toString (getMemFromTable var fvarTable)) "), R0);\n"
                                "MOV(R0, IMM(SOB_VOID));\n"
                                ))
                      (else "WRONG")))))
                      
                      
(define genBox
    (lambda(pe)
        (string-append
            (code-gen (cadr pe))
            "MOV(R1,R0);\n"
            "PUSH(1);\n"
            "CALL(MALLOC);\n"
            "DROP(1);\n"
            "MOV(IND(R0), R1);\n"
            )))
            
(define genBoxSet
    (lambda (pe)
            (let ((tag (caadr pe))
                  (var (cadadr pe))
                  (val (code-gen (caddr pe))))
                 (cond ((equal? tag 'pvar) 
                            (let ((minor (car (cddadr pe))))
                              (string-append
                                  val
                                  "MOV(R1, 2);\n"
                                  "ADD(R1," (toString minor)");\n"
                                  "MOV(IND(FPARG(R1)), R0);\n"
                                  "MOV(R0, IMM(SOB_VOID));\n")))
                       ((equal? tag 'bvar)
                            (let ((major (car (cddadr pe)))
                                  (minor (car(cdr (cddadr pe)))))
                              (string-append
                                  val
								  "MOV(R1, FPARG(0));\n"
								  "MOV(R1, INDD(R1,"(toString major)"));\n"
								  "MOV(R1, INDD(R1,"(toString minor)"));\n"
								  "MOV(IND(R1), R0);\n"
								  "MOV(R0, IMM(SOB_VOID));\n"
                                  )))
                       ((equal? tag 'fvar) 
                            (string-append
                                val
								"MOV(R2, R0);\n"
								"PUSH(1);\n"
								"CALL(MALLOC);\n"
								"DROP(1);\n"
								"MOV(IND(R0), R2);\n"
                                "MOV(IND(" (toString (getMemFromTable var fvarTable)) "), R0);\n"
                                "MOV(R0, IMM(SOB_VOID));\n"
                                ))
                      (else "WRONG")))))
                      
(define genBoxGet
    (lambda (pe)
        (let ((tag (caadr pe)))
            (cond ((equal? tag 'pvar) 
                        (let ((minor (car (cddadr pe))))
                            (string-append
                                "MOV(R1, 2);\n"
                                "ADD(R1," (toString minor)");\n"
                                "MOV(R0, IND(FPARG(R1)));\n")))
                    ((equal? tag 'bvar)
                        (let ((major (car (cddadr pe)))
                              (minor (car(cdr (cddadr pe)))))
                            (string-append
								"MOV(R1, FPARG(0));\n"
                                "MOV(R1, INDD(R1,"(toString major)"));\n"
                                "MOV(R0, IND(INDD(R1,"(toString minor)")));\n"
								)))
                    ((equal? tag 'fvar) 
                        (string-append
                            "MOV(R0, IND(" (toString (getMemFromTable var fvarTable)) "));\n"
                            ))
                    (else "WRONG")))))
                    


(define symTblInit
    (lambda ()
        (let ((start #t))
            (fold-left
                string-append
                (string-append "MOV(R1,"(toString symTblStartMem)");\n""SHOW(\"start of  the symTab\", R1);\n")
                (cMap (lambda(el)
                        (if (and (list? (caddr el)) (equal? (caaddr el) 'T_STRING))
                            (if start
                                (let ((str
                                        (string-append 
                                            "MOV(INDD(R1,0), "(toString (getMemFromTable (cadr el) constantTable))");\n"
                                            "MOV(INDD(R1,1), 0);\n")))
                                     (set! start #f)
                                     str)
                                (string-append
                                    "PUSH(2);\n"
                                    "CALL(MALLOC);\n"
                                    "DROP(1);\n"
                                    "MOV(INDD(R0,0), "(toString (getMemFromTable (cadr el) constantTable))");\n"
                                    "MOV(INDD(R0,1), 0);\n"
                                    "MOV(INDD(R1,1), R0);\n"
                                    "MOV(R1, R0);\n"
                                    ))
                                    ""))
                                    
                    constantTable)))))


        
(define code-gen
    (lambda(pe)
        (cond 
            ((equal? (car pe) 'if3) (genIf  pe))
            ((equal? (car pe) 'seq) (genSeq pe))
            ((equal? (car pe) 'or)  (genOr  pe))
            ((equal? (car pe) 'applic)  (genApp  pe))
            ((equal? (car pe) 'tc-applic)  (genTcApp  pe))
            ((equal? (car pe) 'lambda-simple)  (genLamSimp pe))
            ((equal? (car pe) 'lambda-opt)  (genLamOpt pe))
            ((equal? (car pe) 'lambda-var)  (genLamVar pe))
            ((equal? (car pe) 'def) (genDef pe))
            ((equal? (car pe) 'const) (genConst pe))
            ((equal? (car pe) 'fvar) (genFvar pe))
            ((equal? (car pe) 'bvar) (genBvar pe))
            ((equal? (car pe) 'pvar) (genPvar pe))
            ((equal? (car pe) 'set) (genSet pe))
            ((equal? (car pe) 'box) (genBox pe))
            ((equal? (car pe) 'box-set) (genBoxSet pe))
            ((equal? (car pe) 'box-get) (genBoxGet pe))
            
            (else "MOV(R0, IMM(SOB_FALSE));\n")
        )))

(define symTblStartMem 0)		
(define compile-scheme-file
  (lambda (scmFile cTrgFile)
    (let* ((out-port (open-output-file cTrgFile 'truncate))
           (parsedFile (map fullOpt (map parse (listOfExps <sexpr> (file->string scmFile)))))
           (constTableStr (begin (resetConstantTable)
                                 (addConstants parsedFile) 
                                 (removeDuplicatesOfConstTable) 
                                 (addTagAndMem)
                                 (getConstantTableStr)))
           (fvarTableStr (begin  (resetFvarTable)
                                 (addFvars parsedFile) 
                                 (removeDuplicatesOfFvarTable) 
                                 (addMemFvar)
                                 (getFvarTableStr)))
           (symTblStr            (begin (set! symTblStartMem (- (memCountIncAndGet 2) 0))
                                 (symTblInit)))
           (stm ""))
      (display
       (string-append 
                prologue
                constTableStr
                fvarTableStr
                "ADD(IND(0), " (toString (memCountIncAndGet 5)) ");\n"
                (genLibraryFunctions)
                symTblStr
                
                
                
                (fold-left (lambda(acc pe) 
                                    (string-append acc (code-gen pe) 
	"CMP(R0, SOB_VOID);\n"
    "JUMP_EQ(L_STOP_MACHINE_"stm");\n"
    "PUSH(R0);\n"
    "CALL(WRITE_SOB);\n"
    "OUT(2, 10);\n"
	"POP(R0);\n"
    "L_STOP_MACHINE_"(begin (set! stm (getNextCount 'stm)) stm)":\n"))
                      ""
                      parsedFile)
                epilogue
                )
                
       
       out-port)
      (close-output-port out-port))))