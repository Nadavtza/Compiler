(load "qq.scm")

(define parse
  (lambda (exp)     
    ;body run
    (cond   ;main cond
          
          ((null? exp) (Nil->sexp exp))
          ((not (list? exp)) 
           (cond ((boolean? exp) (Booleans->sexp exp))
                 ((string? exp) (String->sexp exp))
                 ((number? exp) (Number->sexp exp))
                 ((char? exp) (Characters->sexp exp))
                 ((vector? exp)(Vectors->sexp exp))
                 ((quote? exp)(Quoted->sexp exp))
                 ((not (member exp reserved-words)) (Var->sexp exp))))
          
          ((equal? (car exp) 'quote) `(const ,@(cdr exp)))               
          ((equal? (car exp) 'if) (If->sexp exp))
          ((equal? (car exp) 'or) (Or->sexp exp))
          ((equal? (car exp) 'lambda) (lambda->sexp exp))
          ((equal? (car exp) 'begin)(Begin->sexp exp))
          ((equal? (car exp) 'define)(Define->sexp exp))
          ((equal? (car exp) 'let) (Let->sexp exp))
          ((equal? (car exp) 'let*) (Let*->sexp exp))
          ((equal? (car exp) 'letrec) (Letrec->sexp  exp))
          ((applic? (car exp)) (Applications->sexp exp))
          ((equal? (car exp) 'set!) (Set->sexp exp))
          ((equal? (car exp) 'cond) (Cond->sexp (cdr exp)))
          ((equal? (car exp) 'and) (And->sexp (cdr exp)))
          ((equal? (car exp) 'quasiquote) (parse (expand-qq (cadr exp))))
          
          
          
         	);end main cond    
  		))


;*********************reserved-words***********************
(define reserved-words
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))

;*********************Constants***********************

(define Number->sexp (lambda (exp) `(const ,exp)))

(define Booleans->sexp (lambda (exp) `(const ,exp)))

(define String->sexp (lambda (exp) `(const ,exp)))

(define Characters->sexp (lambda (exp) `(const ,exp)))

(define Quoted->sexp (lambda (exp) '(const ,(cadr exp))))

(define Nil->sexp (lambda (exp) `(const ())))

(define Vectors->sexp (lambda (exp)  `(const ,exp)))

(define Var->sexp (lambda (exp) `(var ,exp)))

;*********************END OF Constants***********************

;*********************Conditionals***************************

(define If->sexp (lambda (exp)      
                   (if (= (length exp) 4)
                       `(if3 ,(parse (cadr exp)) ,(parse (caddr exp)),(parse (cadddr exp)))
                       `(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) (const ,(void))))))


;*********************Disjunctions***************************

(define Or->sexp (lambda (exp)
                   (cond ((= (length exp) 1) `(const ,#f))
                         ((= (length exp) 2)  (parse (cadr exp)))
                         (else `(or (,@(map parse (cdr exp))))))    
                   ))

;*********************Lambda forms***************************

(define lambda->sexp (lambda (exp)
                       (cond 
                         ((list? (cadr exp))(Regular_Lambda->sexp exp))  ; regular
                         ((pair? (cadr exp))(Optional_Lambda->sexp exp))  ; regular
                         (else (Variadic_Lambda->sexp exp))
                         )))

(define Regular_Lambda->sexp (lambda (exp)
                               `(lambda-simple ,(cadr exp) ,(parse `(begin ,@(cddr exp))))))


(define Optional_Lambda->sexp (lambda (exp)
                                `(lambda-opt , (slice (cadr exp) 1 (- (howMany (cadr exp)) 1)),(cdr(last-pair (cadr exp))) ,(parse `(begin ,@(cddr exp))))))

(define Variadic_Lambda->sexp (lambda (exp)
                                `(lambda-opt ,'(),(cadr exp) ,(parse `(begin ,@(cddr exp))))))
  

;******************************Begin  + seq***********************************

(define Begin->sexp (lambda (exp)            
                      (let ((seqExp (deleteAllSeq exp)))
                        (cond ((= (length seqExp) 0) `(const ,(void))) 
                              ((= (length seqExp) 1) (parse (car seqExp)))         
                              (else `(seq ,(map parse seqExp)))))))


;******************************Begin  Helper***********************************

(define deleteAllSeq (lambda (exp) 
                           (letrec ((deleteSeq (lambda (exp)
                                                  (cond ((null? exp) '())
                                                        ((and  (list? (car exp)) (equal? (caar exp) 'begin)) `(,@(deleteSeq (cdar exp)) ,@(deleteSeq (cdr exp))))
                                                        (else `(,(car exp) ,@(deleteSeq (cdr exp))))))))
                             (deleteSeq (cdr exp)))))

;*******************************Define*************************************

(define Define->sexp (lambda (exp)
                       (if (or (list? (cadr exp)) (pair? (cadr exp)))  
                           (MIT_Define->sexp exp)
                           (Regular_Define->sexp exp)
                           )))  
;***********Regular define**********
(define Regular_Define->sexp (lambda (exp)
                               `(define,(parse(cadr exp)) ,(parse (caddr exp)))))

;***********MIT-style define**********
(define MIT_Define->sexp (lambda (exp)
                         `(define,(parse (caadr exp)) ,(parse `(lambda ,(cdadr exp) ,@(cddr exp))))))


;*********************************Applications********************************

(define applic? (lambda (exp)
                  (and (not (member `(,@exp) reserved-words)) (equal? exp `(,@exp)))))

(define Applications->sexp (lambda (exp)
                             `(applic ,(parse (car exp)) (,@(map parse (cdr exp))))))

;*********************Set***************************

(define Set->sexp (lambda (exp)
                    (if (= (length exp) 3) 
                        `(set ,(Var->sexp (cadr exp)) ,(parse (caddr exp)))                  
                        `(set ,(Var->sexp (cadr exp)) ,@(map parse (cddr exp))))))


(define set? (lambda (car_exp cadr_exp) 
               (and (not (member car_exp reserved-words)) (equal? cadr_exp 'set!))))


;*******************************Let*************************************

(define Let->sexp (lambda (exp)
                    (let ((args  (get_args  (cadr exp)))
                          (vals  (get_vals (cadr exp)))
                          (body  (cddr exp)))
                      ; body let
                      (parse `((lambda ,args ,@body) ,@vals))   
                      )))    

;******************************* Let Star *************************************

(define Let*->sexp (lambda (exp)
                     (let ((args   (cadr exp))
                           (body   (cddr exp)))
                       (if (< (length args) 2)
                           (parse `(let ,args ,@body))
                           (parse `(let ((,(caar args) ,(cadar args))) (let* ,(cdr args) ,@body)))))))


(define Letrec->sexp (lambda (exp)
                       (let ((args (get_args (cadr exp)))
                             (vals (get_vals (cadr exp))))
                         (parse `((lambda ,args ,(checkIfBegin (append (map make_set args vals) 
                                                                         `((let () ,@(cddr exp)))))) ,@(map dont_care vals))))))

;*********************letrec helpers***************************

(define make_set (lambda (var val) `(set! ,var ,val)))

(define dont_care (lambda (val) #f))

(define checkIfBegin
  (lambda (exp)
    (if (null? (cdr exp))
      (car exp)
      `(begin ,@exp))))

;*********************list itreation helpers***************************

(define get-n-items
  (lambda (lst num)
    (if (> num 0)
        (cons (car lst) (get-n-items (cdr lst) (- num 1)))
        '())))

(define slice
  (lambda (lst start count)
    (if (> start 1)
        (slice (cdr lst) (- start 1) count)
        (get-n-items lst count))))


(define (howMany lst)
  (cond ((null? lst) 0)             
        ((not (pair? lst)) 1)            
        (else (+ (howMany (car lst))     
                 (howMany (cdr lst))))))

(define flatten (lambda (x)
                  
                  (cond ((null? x) '())
                        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
                        (else (list x)))))
;*********************END list itreation helpers***************************

;******************************* Let Helpers *************************************

(define get_args (lambda (lst)
                   
                   (cond ((null? lst) '())
                         ((not (pair? lst)) (list lst))
                         ((pair? (car lst))  (append (get_args (car lst)) (get_args (cdr lst))))
                         (else (list (car lst))))))

(define get_vals (lambda (lst)
                   (cond ((null? lst) '())
                         ((not (pair? lst)) (list lst))
                         ((pair? (car lst))  (append (get_vals (car lst)) (get_vals (cdr lst))))
                         (else list (cdr lst)))))


;************************ Cond *********************************************
(define Cond->sexp (lambda (exp)       
                     (cond 
                           ((= (length exp) 1)  (if (= (length (car exp)) 1)
                                                  (parse `(if ,(caar exp) ,(caar exp)))
                                                   (if (equal? (caar exp) 'else) 
                                                    (parse `(begin ,@(cdar exp)))  
                                                      (parse `(if ,(caar exp) ,`(begin ,@(cdar exp)))) )))
                           (else (parse `(if  ,(caar exp) ,`(begin ,@(cdar exp)) (cond ,@(cdr exp))))))))
;************************ AND *********************************************

(define And->sexp (lambda (exp)
                    (cond ((equal? exp '()) (parse #t)) 
                          ((= (length exp) 1)  (parse (car exp)))
                          (else 
                            ((lambda (val_1 rest)
                               (parse `(if ,val_1 (and ,@(rest)) #f)))
                             (car exp)
                             (lambda () (cdr exp)))))))