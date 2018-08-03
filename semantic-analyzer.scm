(define remove-applic-lambda-nil
  (lambda (exp)
    (if (or (null? exp) (not (pair? exp)))
        exp (checkIfRemove (map remove-applic-lambda-nil exp)))))
(define box-set 
  (lambda (exp)
    (cond ((isLstEmpty exp) exp) ((equal? (car exp) 'lambda-opt) (to_box exp)) ((equal? (car exp) 'lambda-simple) (to_box exp))
          (else (map box-set exp)))))
(define pe->lex-pe
  (lambda (pe)
    (lex_func pe '() '())))

(define annotate-tc
  (lambda (pe)
    (annotate pe #f)))

(define checkIfRemove
  (lambda (exp)
    (if (equal? (car exp) 'applic)
        (let ((isSimple (caadr exp))
              (expList (cadadr exp))
              (isNull (caddr exp)))
          (if (and (equal?  isSimple 'lambda-simple) (equal? expList (list)) (null? isNull))
              (car (cddadr exp))
              exp ))
        exp )))
;***************Remove redundant******************



;;;;;;;;;;;;ANNOTATE;;;;;;;;;;;;;;;;;;;;;;;;

(define lex_vars (list 'pvar 'bvar 'fvar 'box-get 'const))


(define annotate-seq
  (lambda (exp tp?)
    (if(null? exp)
        '()
        (if (= 1(length exp))
            (list (annotate (car exp) tp?))
            (cons (annotate (car exp) #f) (annotate-seq (cdr exp) tp?))))))


(define annotate
  (lambda (exp tp?)
    (if (not (or (null? exp) (not (pair? exp))))
        (let ((kdr (cdr exp))
              (kar (car exp)))
          (cond 
            ((member kar lex_vars) exp)
            ((equal? 'if3 kar)   `(if3 ,(annotate (car kdr) #f) ,(annotate (caddr exp) tp?) ,(annotate (cadddr exp) tp?)))
            ((equal? 'or kar) `(or ,(annotate-seq (car kdr) tp?)))
            ((equal? 'seq kar) `(seq ,(annotate-seq (cadr exp) tp?)))   
            ((equal? 'define kar) `(define ,(car kdr) ,(annotate (caddr exp) #f)))
            
            ((and (equal? 'applic kar) tp?)
             `(tc-applic ,(annotate (cadr exp) #f) ,@(map annotate (cdr kdr) (make-list (length (cddr exp) ) #f))))
            
            ((and (equal? 'applic kar) (not tp?))
             `(applic  ,(annotate (car kdr) #f) ,@(map annotate (cdr kdr) (make-list (length (cdr kdr) ) #f))))
            
            ((equal? 'lambda-simple kar) `(lambda-simple ,(car kdr) ,(annotate (caddr exp) #t)))
            ((equal? 'lambda-opt kar) `(lambda-opt ,(car kdr)  ,(caddr exp),(annotate (cadddr exp) #t)))             
            ((equal? kar 'set) `(set ,(cadr exp) ,(annotate (caddr exp) #f)))
            ((equal? kar 'box-set) `(box-set ,(car kdr) ,(annotate (caddr exp) #f)))
            (else (cons (annotate (car exp) tp?) (annotate kdr tp?))) 
            
            ))exp)))



;;;;;;;;;;;;LEXYCAL;;;;;;;;;;;;;;;;;;;;;;;


(define lex_func
  (lambda (ast prm bound)
    (if (not (or (null? ast) (not (pair? ast))))
        (let* ((kdr (cdr ast))
               (kar (car ast))
               (karList (list kar))
               (kdrList (list kdr)))
          
          (cond 
            ((pair? kar) (cons (lex_func (car ast) prm bound) (lex_func kdr prm bound)))
            ((equal? kar 'lambda-opt)  (append karList (list (cadr ast) (caddr ast)) (lex_func (cdddr ast) (append (cadr ast) (caddr ast)) (cons prm bound))))
            ((equal? kar 'lambda-simple) (append karList (list (cadr ast)) (lex_func (cddr ast)  (cadr ast) (cons prm bound))))
            ((equal? kar 'var) (var_index (cadr ast) prm bound ))
            (else (list* (lex_func kar prm bound) (lex_func kdr prm bound))))) ast)))


(define bound_index
  (lambda (exp var helper1 helper2)
    (if (pair? exp)
        (let ((kdr (cdr exp))
              (kar (car exp))
              (incHelper1 (+ helper1 1))
              (incHelper2 (+ helper2 1)))
          (cond ((and (pair? kar) (equal? var (car kar))) `(bvar ,var ,helper1 ,helper2))
                ((pair? kar) (bound_index (cons (cdr kar) kdr) var helper1 incHelper2))
                ((equal? var kar) `(bvar ,var ,helper1 ,helper2))
                (else (bound_index kdr var incHelper1 0))))
        `(fvar ,var))))

(define prm_index
  (lambda (exp var helper)
    (cond ((and (pair? exp) (equal? var (car exp))) `(pvar ,var ,helper))
          ((pair? exp) (prm_index (cdr exp) var (+ helper 1)))
          ((equal? var exp) `(pvar ,var ,helper))
          (else `(fvar ,var)))))


(define var_index
  (lambda (var args bounds)
    (if (equal? (car (prm_index args var 0)) 'fvar)
        (bound_index bounds var 0 0)
        (prm_index args var 0))))


;;;;;;;;;;;;BOX;;;;;;;;;;;;;;;;;;;;;;;



(define isLstEmpty
  (lambda (exp)
    (or (not (list? exp)) (null? exp))))


(define to_box  
  (lambda (exp) 
    (let*
      ((kar_exp (car exp))
       (vars (if (equal? kar_exp 'lambda-opt) (list (cadr exp) (caddr exp))  (cadr exp)))
       (body (if (equal? kar_exp 'lambda-opt) (cadddr exp)  (caddr exp)))
       (boxedVars (boxesList exp)))
      (cond ((null? boxedVars) (checkAndSetLambda exp body vars))
            (else
              (let* ((box (box_me boxedVars))
                     (box_body (main_box_body boxedVars (box-set (if (equal? (car body) 'seq) (cadr body) body)))))
                (cond 
                  ((and (equal? (car body) 'seq)  (lambda_opt? exp))
                   `(,kar_exp ,(car vars) ,(cadr vars) 
                              (seq (,@box ,@box_body))))
                  
                  ((equal? (car body) 'seq) 
                   `(,kar_exp ,vars 
                              (seq (,@box  ,@box_body))))
                  
                  ((lambda_opt? exp) 
                   `(,kar_exp ,(car vars) ,(cadr vars) 
                              (seq (,@box ,box_body))))
                  (else 
                    `(,kar_exp ,vars 
                               (seq (,@box  ,box_body))))
                  )))))))


(define box_me (lambda (exp)
                 (let ((kar (car exp))
                       (kdr (cdr exp))) 
                   (if (isLstEmpty kdr) 
                       `((set ,kar (box ,kar))) 
                       `((set ,kar (box ,kar)) ,@(box_me kdr))))))

(define boxesList 
  (lambda (exp)  
    (let* ((body (if (equal? (car exp) 'lambda-opt) (cadddr exp)  (caddr exp)))
           (myLst (append (cadr exp) (list (caddr exp))))
           (vars (map (lambda (var) 
                        (if (and (var_bound? var body)  (var_get? var body) (var_set?  var body)) `(var ,var))) myLst)))
      (remove (void) vars))))


(define checkAndSetLambda (lambda (exp body prms) 
                            (if (equal? (car exp) 'lambda-opt) 
                                `(lambda-opt ,(car prms)  ,(cadr prms) ,(box-set body)) 
                                `(lambda-simple ,prms ,(box-set body)))))


(define lambda_opt? (lambda (exp)
                      (cond ((and (list? exp) (equal? (car exp) 'lambda-opt)) #t)
                            ((and (equal? (car exp) 'lambda-opt) (list? exp) (equal? (cadr exp) 'args)) #t)
                            (else #f)
                            )))


(define lambda_exp? (lambda (exp) 
                      (if (equal? (car exp) 'lambda-simple) 
                          #t
                          (lambda_opt? exp))))


(define var_bound? (lambda (var exp) 
                     (letrec ((bool_in #f) 
                              (go_in (lambda (exp_in)   
                                       (if (not (or (null? exp_in) (not (pair? exp_in))))
                                           (let ((kdr (cdr exp_in))
                                                 (kar (car exp_in)))
                                             (cond 
                                               
                                               ((lambda_opt? exp_in) (if (and (is_var_bound var (caddr kdr)) (checkMember exp_in var kdr)) (set! bool_in #t)))
                                               
                                               ((lambda_exp? exp_in) (if (and (is_var_bound var (cadr kdr)) (checkMember exp_in var kdr)) (set! bool_in #t)))
                                               
                                               ((and (lambda_exp? exp_in) (not (checkMember exp_in var kdr))) bool_in)
                                               
                                               (else (if (or (null? kar) (not (pair? kar)))
                                                         (go_in kdr)
                                                         (begin (go_in kar) (go_in kdr)))))
                                             )
                                           ) bool_in)))
                       (go_in exp))))

(define checkMember (lambda (exp_in var kdr)
                      (if(lambda_opt?  exp_in)
                          (or (not (member var (cadr exp_in))) (not (member var `((,(caddr exp_in))) )))
                          (not (member var (car kdr))))))


(define is_var_bound  
  (lambda (var exp) 
    (letrec ((bool_in #f)
             (go_in (lambda (exp_in)
                      (if (not (or (null? exp_in) (not (pair? exp_in))))
                          (let ((kdr (cdr exp_in))
                                (kar (car exp_in)))
                            (cond
                              
                              ((lambda_opt? exp_in) (if (checkMember exp_in var kdr) (go_in (caddr kdr)) bool_in))
                              
                              ((lambda_exp? exp_in) (if (checkMember exp_in var kdr) (go_in (cadr kdr)) bool_in))
                              
                              ((and (lambda_exp? exp_in) (not (checkMember exp_in var kdr))) bool_in)
                              
                              ((and (not bool_in) (equal? `(var ,var) exp_in))  (set! bool_in #t))
                              
                              (else  (if (or (null? exp_in) (not (pair? exp_in)))
                                         (go_in kdr)
                                         (begin (go_in kar) (go_in kdr) )  ))
                              
                              ) bool_in)))))
      (go_in exp) )))


(define var_get? (lambda (var exp) 
                   (letrec ((bool_in #f)
                            (go_in (lambda (exp_in)
                                     (if (not (or (null? exp_in) (not (pair? exp_in))))
                                         (let ((kdr (cdr exp_in))
                                               (kar (car exp_in)))
                                           (cond 
                                             ((and (not bool_in) (equal? `(var ,var) exp_in)) (set! bool_in #t))
                                             
                                             ((and (equal? 'set kar) (equal? (cadadr exp_in) var)) (go_in (cdr kdr)) bool_in)
                                             
                                             ((lambda_opt? exp_in) (if (not (member var (append (car kdr) (list  (cadr kdr)))))  (go_in (caddr kdr))) bool_in)
                                             
                                             ((and (lambda_opt? exp_in) (checkMember exp_in var kdr) ))
                                             
                                             ((and (lambda_exp? exp_in) (checkMember exp_in var kdr) )
                                              
                                              (go_in (cadr kdr)))
                                             
                                             (else (if (or (null? kar) (not (pair? kar)))
                                                       (go_in kdr) 
                                                       (begin (go_in kar) (go_in kdr))))) bool_in )))))
                     (go_in exp))))



(define var_set? (lambda (var exp) 
                   (letrec ((bool_in #f)
                            (go_in (lambda (exp_in)
                                     (if (not (or (null? exp_in) (not (pair? exp_in))))
                                         (let ((kdr (cdr exp_in))
                                               (kar (car exp_in)))
                                           (cond 
                                             
                                             ((lambda_opt? exp_in) (if (checkMember exp_in var kdr) (go_in (caddr kdr)) bool_in))
                                             
                                             ((lambda_exp? exp_in) (if (checkMember exp_in var kdr) (go_in (cadr kdr))))      
                                             
                                             ((and (equal? 'set kar) (equal? (cadar kdr) var)) (set! bool_in #t))
                                             
                                             (else (if (or (null? kar) (not (pair? kar)))
                                                       (go_in kdr) 
                                                       (begin (go_in kar) (go_in kdr))))))))))
                     (begin (go_in exp) bool_in ))))



(define main_box_body (lambda (prms body)  
                        (if (not (or (null? prms) (not (pair? prms))))
                            (let* ((kdr (cdr prms))
                                   (kar (car prms))
                                   (boxed_body (box_body kar body))
                                   )
                              (main_box_body kdr boxed_body)) body)))

(define box_body (lambda (var body)  
                   (if (not (or (null? body) (not (pair? body))))
                       (let ((kdr (cdr body))
                             (kar (car body)))
                         (cond 
                           
                           ((and (lambda_exp? body) (member (cadr var) (car kdr))) body)
                           
                           ((and (equal? 'set (car body))  (equal? (car kdr) var)) `(box-set ,var ,@(box_body var (cdr kdr))))
                           
                           ((and (lambda_opt? body)  (member (cadr var) `( ,(car kdr) (,(cadr kdr)))))  body)
                           
                           ((equal? body var) `(box-get ,var))
                           
                           (else (cons (box_body var kar) (box_body var kdr))))) body)))




