(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

(define counter 0)

(define pipeline
  (lambda (s)
    ((star <sexpr>) s
     (lambda (m r)
       (map (lambda (e)
              (annotate-tc
                (pe->lex-pe
                  (box-set
                    (remove-applic-lambda-nil
                      (parse e))))))
            m))
     (lambda (f) 'fail))))

(define file->list
  (lambda (in-file)
    (let (  (in-port (open-input-file in-file)))
      (letrec ((run
                 (lambda ()
                   (let ((ch (read-char in-port)))
                     (if (eof-object? ch)
                         (begin
                           (close-input-port in-port)
                           '())
                         (cons ch (run)))))))
        (run)))))






;problems
;1 --> code_gen_applic is looking for free var proc when proc is pvar
(define counter 0)
(define my_map "(define map (lambda (func lst) (if (null? lst) lst (cons (func (car lst)) (map func (cdr lst))))))\n")
; (define append_var_string  (string-append "(define append_var (lambda (lst)"
;                                  "(let ((append__2_args (lambda (first second )"
;                                             "(if (null? first) second"
;                                             "(cons (car first) (append (cdr first) second))))) )"
;                                      "(if (null? lst) lst"
;                                       "(append__2_args (car lst) (append_var (cdr lst) )))))) \n" ))
 
;(define foo2 
;  (lambda (lst)
;    (append lst (string->list (string-append  my_map lib-fold-left)))))
 
(define run-time-support
  (lambda (lst)
    (string->list (string-append lib-fold-left my_map lib-bin-append lib-append (list->string lst) 
                                 ))))
 
;(define run-time-support
;  (lambda (lst)
;    (string->list (string-append my_map (list->string lst) 
;                                 ))))
 
 
;(define lib-list "(define list (lambda x x))\n")
(define lib-fold-left "(define fold_left (lambda (proc init lst) (if (null? lst)  init (fold_left proc (proc init (car lst)) (cdr lst)))))\n")
                                   
                                   
(define lib-bin-append "(define bin_append (lambda (lst1 lst2) (if (null? lst1)   lst2  (cons (car lst1) (bin_append (cdr lst1) lst2)))))\n")
                                
                                 
(define lib-append "(define append (lambda x (fold_left bin_append '() x)))\n")
 
 
(define append_var (lambda (lst)
       (let ((append__2_args (lambda (first second )
                  (if (null? first) second
                  (cons (car first) (append (cdr first) second))))) )
            (if (null? lst) lst
            (append__2_args (car lst) (append_var (cdr lst) ))))))              



(define true_label "l0") ; temp labels
(define false_label "l1")
(define nil_label "l2")
(define void_label "l3")

(define compile-scheme-file (lambda (in_file out_file) ; remove find index and get lengths else row in create const table

                              (let*
                                (
                                 ;(input_pe (pipeline (run-time-support (file->list in_file))))
                                 (input_pe (pipeline (file->list in_file)))
                                 (prologue (list->string (file->list "prologue.s")))
                                 (output_as (del_if_created out_file))
                                 (consts_lst (create_constant_list input_pe));;;maybe to remove duplicates  after append
                                 (const_table (create_const_table consts_lst '()))
                                 (fvar_lst (counter_fvar_list (create_fvar_list input_pe)));;;maybe to remove duplicates  after append, if fvar doesnt work try to change fvar list in scheme
                                 ;(symbolLst (symbolListCreator input_pe))
                                 (gen_table (gen_constant_table const_table))
                                 (fvar_table (gen_fvar_table fvar_lst))
                                 
                                 )
                                
                                (set! true_label (find_const_label `(const #t) const_table))
                                (set! false_label (find_const_label `(const #f) const_table))
                                ;(set! nil_label (find_const_label `(const ,()) const_table))
                                (set! void_label (find_const_label `(const ,(void)) const_table))
                                ;(display "consts_lst\n")
                                ;(display consts_lst)
                                ;(display "\n\n\nconststable\n")
                                ;(display const_table)
                                ;(display "\n\ninput_pe\n")
                                ;(display input_pe)
                                ;(display "\n\n\nfvar_lst\n")
                                ;(display fvar_lst)
                                ;(display "fvar_table")
                                ;(display fvar_table)
                                ;(display "cosds")
                                ;(display const1)
                                (write_to_file output_as (string-append
                                                           prologue
                                                           gen_table 
                                                           
                                                           fvar_table
                                                   
                                                           "section .text\n"
                                                            "main: \n"
                                                            ;";****is_zero****\n"
                                                            (gen_zero? const_table fvar_lst)
                                                            (gen_car const_table fvar_lst)
                                                            (gen_boolean? const_table fvar_lst)
                                                            (gen_list const_table fvar_lst)
                                                            (gen_char? const_table fvar_lst)
                                                            (gen_integer? const_table fvar_lst)
                                                            (gen_sym_to_str const_table fvar_lst)
                                                            (gen_set_string const_table fvar_lst)
                                                            (gen_integer_to_char const_table fvar_lst)
                                                            (gen_char_to_integer const_table fvar_lst)
                                                            (gen_string_length const_table fvar_lst)
                                                            (gen_string_to_symbol const_table fvar_lst)
                                                            (gen_vector_length const_table fvar_lst)
                                                            (gen_string_ref const_table fvar_lst)
                                                            (gen_vector_ref const_table fvar_lst)
                                                            (gen_number? const_table fvar_lst)
                                                            (gen_not const_table fvar_lst)
                                                            (gen_vector const_table fvar_lst)
                                                            (gen_plus const_table fvar_lst)
                                                            (gen_sub const_table fvar_lst)
                                                            (gen_mul const_table fvar_lst)
                                                            (gen_div const_table fvar_lst)
                                                            (gen_smaller_than const_table fvar_lst)
                                                            (gen_bigger_than const_table fvar_lst)
                                                            (gen_equal_to const_table fvar_lst)
                                                            (gen_numerator const_table fvar_lst)
                                                            (gen_denominator const_table fvar_lst)
                                                            (gen_rational? const_table fvar_lst)
                                                            (gen_remainder const_table fvar_lst)
                                                             (gen_make_vector const_table fvar_lst)
                                                             (gen_vector_set const_table fvar_lst)
                                                             (gen_make_string const_table fvar_lst)
     
                                                            ;(gen_car const_table fvar_lst)
                                                            ;(gen_cdr const_table fvar_lst)
                                                            ;(gen_cons const_table fvar_lst)
                                                                ;"\t push rbp\n"
                                                                ;"\t mov rax, 0\n"
                                                                ;"\t push rax\n"
                                                                ;"\t push rax\n"
                                                                ;"\t push rax\n"
                                                                ;"\t push rax\n"
                                                                ;"\tmov rbp,rsp\n"
                                                           (fold-left string-append "" (map (lambda (parsed_var) (code-gen parsed_var const_table fvar_lst 0)) input_pe ))
                                                           epilogue
                                                           )))))
;add rsp, 4*8\n
(define epilogue
  "\n\n
  epilog:
  \n

  push qword [rax]\n
  call write_sob_if_not_void\n
  add rsp, 1*8\n
  \n"
  )

;;;;;;;;;;;;;;;;;;===========================================LIBRARY_FUNCS=================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define general_lambda (lambda (fvar fvar_code fvar_list)
                         (let* (
                               (lambda_label (number->string (label_counter 'general_lambda)))
                               (end_label (string-append "END" lambda_label))
                               (lambda_body (string-append "lambda_body" lambda_label))
                               )
                              (string-append
                                "mymalloc 2\n"       
                                "mov rbx, 0\n"       
                                "MAKE_LITERAL_CLOSURE rax, rbx, "lambda_body"\n"     ;rax = return value, rbx = env, rcx = body address
                                   
                                "jmp " end_label "\n"
                                lambda_body":\n"
                                "push rbp\n"
                                "mov rbp,rsp\n"
                                fvar_code "\n"
                                "leave\n"
                                "ret\n"
                                end_label ":\n"   
                                "mov ["(find_free_label fvar fvar_list) "], rax\n\n"))))

(define gen_vector?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_vector (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_VECTOR" "\n"
                    "jne false_label_vector" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_vector" "\n"
                    "false_label_vector:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_vector:")))
                    
                    (general_lambda 'vector? code_is_vector fvar_lst))))
 
(define gen_string?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_string (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_STRING" "\n"
                    "jne false_label_string" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_string" "\n"
                    "false_label_string:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_string:")))
                    
                    (general_lambda 'string? code_is_string fvar_lst))))

(define gen_zero? (lambda (const_lst fvar_lst)
                  (let ((code_is_zero (string-append
                    ";*************gen zero*************\n"
                    "mov r15, 0\n"
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "DATA rax\n"
                    "cmp rax, r15" "\n"
                    "jne false_label_is_zero" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_zero" "\n"
                    "false_label_is_zero:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_zero:"
                    )))
                    (general_lambda 'zero? code_is_zero fvar_lst)
                  )))



(define gen_rational?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_rational (string-append

                    "mov rax, A0\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_FRACTION" "\n"
                    "jne false_label_rational" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_rational" "\n"
                    "false_label_rational:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_rational:")))
                    
                    (general_lambda 'rational? code_is_rational fvar_lst))))

(define gen_remainder
  (lambda (const_lst fvar_lst)
                  (let ((code_is_remainder (string-append
                        
                    "push rbx\n"
                    "push rdx\n"

                    
                    "mov rax, A0\n"
                    "mov rax, [rax]\n"   
                    "mov rbx, A1\n"                        
                    "mov rbx, [rbx]\n"       
                    "DATA rax\n"
                    "DATA rbx\n"
                    "CQO\n"
                    "mov rdx, 0\n"
                    "idiv rbx\n"
                    "shl rdx, TYPE_BITS\n"     
                    "or rdx, T_INTEGER\n"
                    

                    "mymalloc 1\n"
                    "mov [rax], rdx\n" 

                    "pop rdx\n" 
                    "pop rbx\n"     
                    "end_is_remainder:")))                
                    (general_lambda 'remainder code_is_remainder fvar_lst))))


(define gen_make_vector  ;;to change
  (lambda (const_lst fvar_lst)
                  (let ((code_is_make_vector (string-append
                        
                  "mov r10, A0\n"     
                  "mov r10, [r10]\n"
                  "DATA r10\n"          

                  "mov rdx,arg_count\n";     
                  "cmp rdx, 1\n"
                  "jg make_vector_two_ \n" 


                
                  "mymalloc 1\n"
                  "mov rcx, rax\n"
                  "mov qword [rcx], 0\n"
                  "or qword [rcx], T_INTEGER\n"
                  "jmp _init_vector \n" 



                  "make_vector_two_:" "\n"

                  "mov rcx, A1\n";     
                    
                  
                  "_init_vector:\n" 
                  "mymalloc r10\n"      
                  "mov rdi, 0\n"    

                  ;create new string
                  "make_vector_loop: ""\n"
                  "cmp rdi, r10\n"
                  "je make_vector_end_loop \n"
                  "mov [rax + rdi * 8], rcx\n"    
                  "inc rdi\n"
                  "jmp make_vector_loop \n " 



                  "make_vector_end_loop:\n"

                  "mov rcx, rax\n"        ;BU pointer to new string
                  "sub rcx, start_of_data\n"
                  "mymalloc 1\n"
                  "mov qword [rax], r10\n"
                  "shl qword [rax], 30\n"
                  "or  qword [rax],rcx\n"
                  "shl qword [rax], TYPE_BITS\n"
                  "or qword [rax], T_VECTOR\n"

                    "end_is_make_vector:")))                
                    (general_lambda 'make-vector code_is_make_vector fvar_lst))))

 






;(define gen_vector_set
;  (lambda (const_lst fvar_lst)
;                  (let ((code_is_gen_vector_set (string-append
                        
;                    "push rbx\n"
;                    "push rdx\n"
;                    "push rcx\n"

;              "\tmov rbx, qword [rbp + 4*8]\n"      
;                    "\tmov rbx, [rbx]\n"          ;rbx = addres vector
;                    ;"\tDATA rbx\n"             

;                    "\tmov rcx, qword [rbp + 5*8]\n";       
;                    "\tmov rcx, [rcx]\n"
;                    "\tDATA rcx\n"              ;rcx = pos to set (index)


;                    "\tmov rdx, qword [rbp + 6*8]\n";       ;rdx = value to set in [index]
;                    ;"\tmov rdx, [rdx]\n"
;                    ;"\tDATA rdx\n"             

;                    "\tDATA_LOWER rbx\n"
;                    "\tmov qword [rbx + rcx*8], rdx\n"
;                    "\tmov rax, " nil_label "\n"
;                    "\tjmp vector_set_end \n" 

            
;                    ;****ONLY ONE ARG****
;                    "\tmymalloc 1\n"
;                    "\tmov rcx, rax\n"
;                    "\tmov qword [rcx], 0\n"
;                    "\tor qword [rcx], T_INTEGER\n"
;                    "\tjmp set_vector_init_vector\n"


                    
;                      "\tmov rcx, qword [rbp + 5*8]\n";     
                      
                    
;                    "set_vector_init_vector:\n"

;                      "\tmymalloc rbx\n"      ;allocate (vector-len) address
;                      "\tmov rdi, 0\n"      ;init index loop

;                    ;create new string
;                    "set_vector_loop:\n"
;                      "\tcmp rdi, rbx\n"
;                      "\tje set_vector_end_loop\n" 
;                      "\tmov [rax + rdi * 8], rcx\n"    
;                      "\tinc rdi\n"
;                      "\tjmp set_vector_loop \n"



;                  "set_vector_end_loop:\n"

;                    "\tmov rcx, rax\n"        ;BU pointer to new string
;                    "\tsub rcx, start_of_data\n"
;                    "\tmymalloc 1\n"
;                    "\tmov qword [rax], rbx\n"
;                    "\tshl qword [rax], 30\n"
;                    "\tor  qword [rax],rcx\n"
;                    "\tshl qword [rax], TYPE_BITS\n"
;                    "\tor qword [rax], T_VECTOR\n"
;                    ;"\tmov rax, qword [rax]\n"
            
;                    "vector_set_end:\n"


;                    "push rcx\n"
;                    "push rdx\n"
;                     "push rbx\n"


                   
;                    "end_is_vector_set:")))                
;                    (general_lambda 'vector-set! code_is_gen_vector_set fvar_lst))))



(define gen_make_string   ;;;to change
  (lambda (const_lst fvar_lst)
                  (let ((code_make_string (string-append
                        
                   
        "\tmov rbx, qword [rbp + 4*8]\n"     
        "\tmov rbx, [rbx]\n"
        "\tDATA rbx\n"              ;rbx = string len

        "\tmov rdx, qword [rbp + 3*8]\n";     
        "\tcmp rdx, 1\n"
        "\tjg make_string_\n" 


        ;ONLY ONE ARG
        "\tmov rcx, 0\n"
        "\tjmp string_init\n" 

         "make_string_:\n"

          "\tmov rcx, qword [rbp + 5*8]\n";     
          "\tmov rcx, [rcx]\n"
          "\tDATA rcx\n"              ;rcx = char to string rbx times
        
        "string_init:\n"

          "\tmymalloc rbx\n"           ;init new string
          ;"\tmov rcx, 0\n"           ;*********
          "\tmov rdi, 0\n"


        ;create new string
         "string_loop:\n"
          "\tcmp rdi, rbx\n"
          "\tje end_string_\n" 
          "\tmov byte [rax + rdi], cl\n"    ;rcx low byte
          "\tinc rdi\n"
          "\tjmp string_loop\n"



        "end_string_:\n"

        "\tmov rcx, rax\n"        ;BU pointer to new string
        "\tsub rcx, start_of_data\n"
        "\tmymalloc 1\n"
        "\tmov qword [rax], rbx\n"
        "\tshl qword [rax], 30\n"
        "\tor  qword [rax],rcx\n"
        "\tshl qword [rax], TYPE_BITS\n"
        "\tor qword [rax], T_STRING\n"



                    "end_is_make_string:")))                
                    (general_lambda 'make-string code_make_string fvar_lst))))



(define gen_boolean?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_boolean (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_BOOL" "\n"
                    "jne false_label_boolean" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_boolean" "\n"
                    "false_label_boolean:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_boolean:")))
                    
                    (general_lambda 'boolean? code_is_boolean fvar_lst))))

(define gen_char?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_char (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_CHAR" "\n"
                    "jne false_label_char" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_char" "\n"
                    "false_label_char:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_char:")))
                    (general_lambda 'char? code_is_char fvar_lst))))

(define gen_integer?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_int (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_INTEGER" "\n"
                    "jne false_label_int" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_int" "\n"
                    "false_label_int:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_int:")))
                    
                    (general_lambda 'integer? code_is_int fvar_lst))))
(define gen_null?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_null (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_NIL" "\n"
                    "jne false_label_null" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_null" "\n"
                    "false_label_null:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_null:")))
                    
                    (general_lambda 'null? code_is_null fvar_lst))))

(define gen_number? (lambda (const_lst fvar_lst) ;works
                         
                         (let ((code_is_number (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "TYPE r15\n"
                                                    "cmp r15, T_INTEGER\n"
                                                    "je is_number_true_label\n"
                                                    "cmp r15, T_FRACTION\n"
                                                    "je is_number_true_label\n"
                                                    "mov r15, " (find_const_label '(const #f) const_lst) "\n"
                                                    "jmp end_is_number\n"
                                                    "is_number_true_label:\n"
                                                    "mov r15, "(find_const_label '(const #t) const_lst) "\n"
                                                    "end_is_number:\n"
                                                    "mov rax, r15"
                                                    )))
                           
                           (general_lambda 'number? code_is_number fvar_lst))))

(define gen_symbol?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_symbol (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_SYMBOL" "\n"
                    "jne false_label_symbol" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_symbol" "\n"
                    "false_label_symbol:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_symbol:")))
                    
                    (general_lambda 'symbol? code_is_symbol fvar_lst))))
 
(define gen_pair?
  (lambda (const_lst fvar_lst)
                  (let ((code_is_pair (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"
                    "TYPE rax\n"
                    "cmp rax, T_PAIR" "\n"
                    "jne false_label_pair" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_pair" "\n"
                    "false_label_pair:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_pair:")))
                    
                    (general_lambda 'pair? code_is_pair fvar_lst))))
 
(define gen_procedure?
  (lambda (const_lst fvar_lst)
 
 
                  (let ((code_is_procedure (string-append
                    "mov rax, qword[rbp + 4*8]\n"
                    "mov rax, [rax]\n"    
                    "TYPE rax\n"
                    "cmp rax, T_CLOSURE" "\n"
                    "jne false_label_procedure" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_procedure" "\n"
                    "false_label_procedure:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_procedure:")))
 
                    (general_lambda 'procedure? code_is_procedure fvar_lst))))
 
(define gen_eq?
  (lambda (const_lst fvar_lst)
 
 
                  (let ((code_is_eq (string-append
                    "mov rax, A0\n"
                    "mov r10, A1\n"   
                    "cmp rax, r10" "\n"
                    "jne false_label_eq" "\n"
                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                    "jmp end_is_eq" "\n"
                    "false_label_eq:" "\n"
                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                    "end_is_eq:")))
 
                    (general_lambda 'eq? code_is_eq fvar_lst))))
 
 
 (define gen_list
  (lambda (const_lst fvar_lst)
                  (let ((code_is_list (string-append;   
 
                    "create_empty_list:\n"
 
 
 
                    "mymalloc 1\n"                  
                    "mov r8, rax\n" ;save the address r8
                    "mov r8, " (find_const_label ''() const_lst) "\n" ;create empty list
                 
                    "mov r9, arg_count\n"  ;r9 =counter down
 
                     "start_loop:\n"           
                     "cmp r9, 0" "\n"
 
                     "jne create_pair_\n"
                     "jmp end_loop_list\n"
 
                     "create_pair_:\n"
                     "sub r9 , 1\n"
                     "mymalloc 1\n"                  
                     "mov r10, rax\n"    ;r10= address of pair         
                     "mov r11, An(r9)\n"                                    
                     "MAKE_MALLOC_LITERAL_PAIR r10 ,r11 , r8 \n"
                     "mov r8, r10 \n"
 
                     "jmp start_loop\n"
 
                     "end_loop_list:\n"
                     "mov rax, r8\n"
                    "end_is_list:")))
                    (general_lambda 'list code_is_list fvar_lst))))
 
 
 
 
 
(define gen_cons
  (lambda (const_lst fvar_lst)
                  (let ((code_is_cons (string-append    
                    "cons_label:\n"   
 
                    "mymalloc 1\n"                  
                    "mov r10, rax\n"
                    "mov r9, A0\n"
                    "mov r11, A1\n"  
                    "MAKE_MALLOC_LITERAL_PAIR r10 ,r9 , r11 \n" 
                    "end_is_cons:")))
                    (general_lambda 'cons code_is_cons fvar_lst))))

(define gen_sym_to_str (lambda (const_lst fvar_lst) ; maybe not working
                         
                         (let ((code_sym_to_str (string-append
                                                    "mov rax, A0\n"
                                                    "mov rax, [rax]\n"
                                                    "DATA rax\n"
                                                    "or rax, T_STRING"
                                                    )))
                           (general_lambda 'symbol->string code_sym_to_str fvar_lst))))



(define gen_set_string (lambda (const_lst fvar_lst) ; maybe not working
                         
                         (let ((code_set_string (string-append
                                                    "mov rax, A0\n"
                                                    "mov r15, A1\n"
                                                    "mov r15, [r15]\n"
                                                    "mov rax, r15\n"
                                                    )))
                           (general_lambda 'string-set! code_set_string fvar_lst))))


;(define gen_set_string (lambda (const_lst fvar_lst) code from tzabari
 
;   (let ((code_set_string (string-append
;            (string-append
;              "mov rax, qword[rbp + 4*8]\n"
 
;              "mov rax, [rax]\n"
;              "DATA rax\n"
;              "or rax, T_STRING"
 
;                          ))))
 
;         (general_lambda 'null? code_is_null fvar_lst))))









(define gen_integer_to_char (lambda (const_lst fvar_lst) ;works
                         
                         (let ((code_int_to_char (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "DATA r15\n"
                                                    "sal r15, 4\n"
                                                    "or r15, T_CHAR\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r15\n"
                                                    )))
                           
                           (general_lambda 'integer->char code_int_to_char fvar_lst))))

(define gen_char_to_integer (lambda (const_lst fvar_lst);works
                         
                         (let ((code_char_to_integer (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "DATA r15\n"
                                                    "sal r15, 4\n"
                                                    "or r15, T_INTEGER\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r15\n"
                                                    )))
                           
                           (general_lambda 'char->integer code_char_to_integer fvar_lst))))

(define gen_string_length (lambda (const_lst fvar_lst);works
                         
                         (let ((code_string_length (string-append
                                                          "mov r15, A0\n"
                                                          "mov r15, [r15]\n"
                                                          "STRING_LENGTH r15\n"
                                                          "mymalloc 1\n"
                                                          "sal r15,4\n"
                                                          "or r15, T_INTEGER\n"
                                                          "mov [rax], r15"
                                                    )))
                           
                           (general_lambda 'string-length code_string_length fvar_lst))))

(define gen_string_to_symbol (lambda (const_lst fvar_lst) ;  maybe nnot working, need to sort out symbol first
                         
                         (let ((code_string_to_symbol (string-append
                                                    "mov r15, qword[rbp+ 4*8]\n"
                                                    "mov r15, [r15]\n"
                                                    "DATA r15\n"
                                                    "sal r15, 4\n"
                                                    "or r15, T_SYMBOL\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r15"

                                                    )))
                           
                           (general_lambda 'string->symbol code_string_to_symbol fvar_lst))))

(define gen_vector_length (lambda (const_lst fvar_lst);works
                         
                         (let ((code_vector_length (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "VECTOR_LENGTH r15\n"
                                                    "mymalloc 1\n"
                                                    "sal r15,4\n"
                                                    "or r15, T_INTEGER\n"
                                                    "mov [rax], r15"

                                                    )))
                           
                           (general_lambda 'vector-length code_vector_length fvar_lst))))
(define gen_string_ref (lambda (const_lst fvar_lst);works
                         
                         (let ((code_string_ref (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "mov r14, A1\n"
                                                    "mov r14 ,[r14]\n"
                                                    "DATA r14\n"
                                                    "mov r13, 0\n"
                                                    "STRING_REF r13B, r15, r14\n"
                                                    "sal r13, 4\n"
                                                    "or r13, T_CHAR\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r13"
                                                    )))
                           
                           (general_lambda 'string-ref code_string_ref fvar_lst))))

(define gen_vector_ref (lambda (const_lst fvar_lst) ; works
                         
                         (let ((code_vector_ref (string-append
                                                    "mov r15, A0\n"
                                                    "mov r15, [r15]\n"
                                                    "mov r14, A1\n"
                                                    "mov r14 ,[r14]\n"
                                                    "DATA r14\n"
                                                    "mov r13, 0\n"
                                                    "VECTOR_REF r13, r15, r14\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r13"
                                                    )))
                           
                           (general_lambda 'vector-ref code_vector_ref fvar_lst))))

(define gen_not (lambda (const_lst fvar_lst) ; works
                  
                  (let ((code_not (string-append
                                    "mov r15, A0\n"
                                    "cmp r15, " (find_const_label '(const #f) const_lst) "\n"
                                    "je not_var_is_false\n"
                                    "mov rax, " (find_const_label '(const #f) const_lst) "\n"
                                    "jmp not_end\n"
                                    "not_var_is_false:\n"
                                    "mov rax, " (find_const_label '(const #t) const_lst) "\n"
                                    "not_end:"
                                    )))
                    
                    (general_lambda 'not code_not fvar_lst))))

                       

(define gen_plus (lambda (const_lst fvar_lst)
             
             (let ((code_plus (string-append
                                        "mov r15, arg_count\n"
                                        "mov r14, 0\n" ; acc
                                        "mov r13, 0\n" ; arg_num
                                        "mov r12, 0\n" ; cur arg
                                        "mov r11, 0\n" ; type check
                                        "mov r10, 1\n" ; current common denumator
                                        "mov r9, 0\n"  ; fraction numerator
                                        "mov r8, 1\n"  ; fraction denumarator
                                        "mov rdx, 0\n"  ; contains copy of cur arg with type
                                        "plus_loop:\n"
                                        "cmp r15, 0\n" 
                                        "je plus_add_end\n"
                                        "mov r12, An(r13)\n" ;get arg
                                        "mov r12, [r12]\n"
                                        "mov r11, r12\n"  ;check type
                                        "mov rdx, r12\n"
                                        "DATA r12\n"
                                        "TYPE r11\n"
                                        "cmp r11, T_FRACTION\n"
                                        "je plus_fraction_exp\n"
                                        "mov r9, r12\n" ; move number to numerator and 1 to denu
                                        "mov r8, 1\n"
                                        "jmp plus_not_fraction\n"
                                        "plus_fraction_exp:\n"
                                        "mov r9, rdx\n" ;get numarator denumartor
                                        "mov r8, rdx\n"
                                        "CAR r9\n"
                                        "DATA r9\n" 
                                        "CDR r8\n"
                                        "DATA r8\n" 
                                        "plus_not_fraction:\n"  
                                        "cmp r8, r10\n"  ;check if denumartor same as curent
                                        
                                        "je plus_same_denumartor\n"
                                        "mov rax, r9\n"  ;mul new fraction numrator with common denu
                                        "mul r10\n" 
                                        "mov r9, rax\n"
                                        
                                        "mov rax, r8\n" ; mul acculmated sum with fraction denu
                                        "mul r14\n"
                                        "mov r14, rax\n"
                                        
                                        "mov rax, r10\n" ; new common denu
                                        "mul r8\n"
                                        "mov r10, rax\n"
                                        "plus_same_denumartor:\n"
                                        
                                        "add r14, r9\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "jmp plus_loop\n"
                                        "plus_add_end:\n"
                                        "cmp r10, 1\n"
                                        "je plus_print_integer\n"
                                        "NUM_GCD r14, r10\n"
                                        "mov rbx, rdi\n"
                                        "mov rax, r14\n"
                                        "mov rdx, 0\n"            ; must do this for div operation to work
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r14, rax\n"
                                        "mov rax, r10\n"
                                        "mov rdx, 0\n"
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r10, rax\n"
                                        "cmp r10, 1\n"
                                        "je plus_print_integer\n"                                                                      
                                        
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "sal r10, 4\n"
                                        "or r10, T_INTEGER\n"

                                        "mymalloc 1\n"
                                        "mov rdx, rax\n"
                                        "mov [rdx], r14\n"
                                        "mymalloc 1\n"
                                        "mov r12, rax\n"
                                        "mov [r12], r10\n"
                                        "mymalloc 1\n"
                                                         
                                        "MAKE_MALLOC_LITERAL_FRACTION rax ,rdx ,r12\n"
                                        
                                        "jmp end_plus\n"
                                        "plus_print_integer:\n"
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "mymalloc 1\n"
                                        "mov qword[rax], r14\n"
                                        "end_plus:"
                                        
                                        )))
               
               (general_lambda '+ code_plus fvar_lst))))


(define gen_sub (lambda (const_lst fvar_lst)
             
             (let ((code_sub (string-append
                                        "mov r15, arg_count\n"
                                        "mov r14, 0\n" ; acc
                                        "mov r13, 0\n" ; cur_arg_num first argument is inserted to r14 after we see we have atleast 1 arg
                                        "mov r12, 0\n" ; cur arg
                                        "mov r11, 0\n" ; type check
                                        "mov r10, 1\n" ; current common denumator
                                        "mov r9, 0\n"  ; fraction numerator
                                        "mov r8, 1\n"  ; fraction denumarator
                                        "mov rdx, 0\n"  ; contains copy of cur arg with type
                                        "sub_loop:\n"
                                        "cmp r15, 0\n"
                                        "je sub_add_end\n"
                                        "mov r12, An(r13)\n" ;get arg
                                        "mov r12, [r12]\n"
                                        "mov r11, r12\n"  ;check type
                                        "mov rdx, r12\n"
                                        "DATA r12\n"
                                        "TYPE r11\n"
                                        "cmp r11, T_FRACTION\n"
                                        "je sub_fraction_exp\n"
                                        "cmp r13,0\n"
                                        "jne sub_not_first\n" ; change the sign of the first argument so we get 0 - - first_arg
                                        "neg r12\n"
                                        "sub_not_first:\n"
                                        "mov r9, r12\n" ; move number to numerator and 1 to denu
                                        "mov r8, 1\n"
                                        "jmp sub_not_fraction\n"
                                        "sub_fraction_exp:\n"
                                        "mov r9, rdx\n" ;get numarator denumartor
                                        "mov r8, rdx\n"
                                        "CAR r9\n"
                                        "DATA r9\n"
                                        "cmp r13,0\n"
                                        "jne sub_not_first_fract\n" ; change the sign of the first argument so we get 0 - - first_arg
                                        "neg r9\n"
                                        "sub_not_first_fract:\n"
                                        "CDR r8\n"
                                        "DATA r8\n" 
                                        "sub_not_fraction:\n"  
                                        "cmp r8, r10\n"  ;check if denumartor same as curent
                                        
                                        "je sub_same_denumartor\n"
                                        "mov rax, r9\n"  ;mul new fraction numrator with common denu
                                        "mul r10\n" 
                                        "mov r9, rax\n"
                                        
                                        "mov rax, r8\n" ; mul acculmated sum with fraction denu
                                        "mul r14\n"
                                        "mov r14, rax\n"
                                        
                                        "mov rax, r10\n" ; new common denu
                                        "mul r8\n"
                                        "mov r10, rax\n"
                                        "t3:\n"
                                        "sub_same_denumartor:\n"
                                        
                                        "t4:\n"
                                        "sub r14, r9\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "jmp sub_loop\n"
                                        "sub_add_end:\n"
                                        "cmp r10, 1\n"
                                        "je sub_print_integer\n"
                                        "mov r11, r14\n"
                                        "sub_gcd:\n"
                                        "NUM_GCD r11, r10\n" ; result in rdi
                                        "sub_after_gcd:"
                                        "mov rbx, rdi\n"
                                        "mov rax, r14\n"
                                        "mov rdx, 0\n" ; must do this for div operation to work
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r14, rax\n"
                                        "mov rax, r10\n"
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r10, rax\n"
                                        "cmp r10, 1\n"
                                        "je sub_print_integer\n"                                                                       
                                        
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "sal r10, 4\n"
                                        "or r10, T_INTEGER\n"

                                        "mymalloc 1\n"
                                        "mov r13, rax\n"
                                        "mov [r13], r14\n"
                                        "mymalloc 1\n"
                                        "mov r12, rax\n"
                                        "mov [r12], r10\n"
                                        "mymalloc 1\n"
                                                         
                                        "MAKE_MALLOC_LITERAL_FRACTION rax ,r13 ,r12\n"
                                        "jmp end_sub\n"
                                        "sub_print_integer:\n"
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "mymalloc 1\n"
                                        "mov qword[rax], r14\n"
                                        "end_sub:"
                                        
                                        )))
               
               (general_lambda '- code_sub fvar_lst))))



(define gen_mul (lambda (const_lst fvar_lst)
             
             (let ((code_mul (string-append
                                        "mov r15, arg_count\n"
                                        "mov r14, 1\n" ; acc
                                        "mov r13, 0\n" ; cur_arg_num first argument is inserted to r14 after we see we have atleast 1 arg
                                        "mov r12, 0\n" ; cur arg
                                        "mov r11, 0\n" ; type check
                                        "mov r10, 1\n" ; current common denumator
                                        "mov r9, 0\n"  ; fraction numerator
                                        "mov r8, 1\n"  ; fraction denumarator
                                        "mov rdx, 0\n"  ; contains copy of cur arg with type
                                        "mul_loop:\n"
                                        "cmp r15, 0\n"
                                        "je mul_add_end\n"
                                        "mov r12, An(r13)\n" ;get arg
                                        "mov r12, [r12]\n"
                                        "mov r11, r12\n"  ;check type
                                        "mov rdx, r12\n"
                                        "DATA r12\n"
                                        "TYPE r11\n"
                                        "cmp r11, T_FRACTION\n"
                                        "je mul_fraction_exp\n"
                                        "mov r9, r12\n" ; move number to numerator and 1 to denu
                                        "mov r8, 1\n"
                                        "jmp mul_not_fraction\n"
                                        "mul_fraction_exp:\n"
                                        "mov r9, rdx\n" ;get numarator denumartor
                                        "mov r8, rdx\n"
                                        "CAR r9\n"
                                        "DATA r9\n" 
                                        "CDR r8\n"
                                        "DATA r8\n" 
                                        "mul_not_fraction:\n"  
                                       
                                        "mov rax, r10\n" ; new common denu
                                        "mul r8\n"
                                        "mov r10, rax\n"
                                        "mov rax, r9\n"
                                        "mul r14\n"
                                        "mov r14, rax\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "jmp mul_loop\n"
                                        "mul_add_end:\n"
                                        "cmp r10, 1\n"
                                        "je mul_print_integer\n"
                                        "mov r11, r14\n"
                                        "mul_gcd:\n"
                                        "NUM_GCD r11, r10\n" ; result in rdi
                                        "mul_after_gcd:"
                                        "mov rbx, rdi\n"
                                        "mov rax, r14\n"
                                        "mov rdx, 0\n" ; must do this for div operation to work
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r14, rax\n"
                                        "mov rax, r10\n"
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r10, rax\n"
                                        "cmp r10, 1\n"
                                        "je mul_print_integer\n"                                                                       
                                        
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "sal r10, 4\n"
                                        "or r10, T_INTEGER\n"

                                        "mymalloc 1\n"
                                        "mov r13, rax\n"
                                        "mov [r13], r14\n"
                                        "mymalloc 1\n"
                                        "mov r12, rax\n"
                                        "mov [r12], r10\n"
                                        "mymalloc 1\n"
                                                         
                                        "MAKE_MALLOC_LITERAL_FRACTION rax ,r13 ,r12\n"
                                        "jmp end_mul\n"
                                        "mul_print_integer:\n"
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "mymalloc 1\n"
                                        "mov qword[rax], r14\n"
                                        "end_mul:"
                                        
                                        )))
               
               (general_lambda '* code_mul fvar_lst))))



(define gen_div (lambda (const_lst fvar_lst)
             
             (let ((code_div (string-append
                                        "mov r15, arg_count\n"
                                        "mov rcx, arg_count\n"
                                        "mov r14, 1\n" ; acc
                                        "mov r13, 0\n" ; cur_arg_num first argument is inserted to r14 after we see we have atleast 1 arg
                                        "mov r12, 0\n" ; cur arg
                                        "mov r11, 0\n" ; type check
                                        "mov r10, 1\n" ; current common denumator
                                        "mov r9, 0\n"  ; fraction numerator
                                        "mov r8, 1\n"  ; fraction denumarator
                                        "mov rdx, 0\n"  ; contains copy of cur arg with type
                                        "div_loop:\n"
                                        "cmp r15, 0\n"
                                        "je div_add_end\n"
                                        "mov r12, An(r13)\n" ;get arg
                                        "mov r12, [r12]\n"
                                        "mov rdx, r12\n"
                                        "mov r11, r12\n"  ;check type
                                        "DATA r12\n"
                                        "TYPE r11\n"
                                        "cmp r11, T_FRACTION\n"
                                        "je div_fraction_exp\n"
                                        "mov r8, r12\n" ; move number to denum and 1 to numerator
                                        "mov r9, 1\n"
                                        "cmp r13,0\n"       ; if first arg switch back numerator and denum
                                        "jne div_int_not_first\n"
                                        "cmp rcx, 1\n"
                                        "je div_int_not_first\n"
                                        "xchg r8, r9\n"       
                                        "div_int_not_first:\n" 
                                        "jmp div_not_fraction\n"
                                        "div_fraction_exp:\n"
                                        "mov r9, rdx\n" ;get numarator denumartor
                                        "mov r8, rdx\n"
                                        "CAR r8\n" ; this is the change in div switch numerator and denum
                                        "DATA r8\n" 
                                        "CDR r9\n"
                                        "DATA r9\n"
                                        "cmp r13, 0\n"       ; if first arg switch back numerator and denum
                                        "jne div_not_first\n"
                                        "cmp rcx, 1\n"
                                        "je div_not_first\n"
                                        "xchg r8, r9\n"
                                        "div_not_first:\n" 
                                        "div_not_fraction:\n" 
                                        
                                        "mov rax, r10\n" ; new common denu
                                        "mul r8\n"
                                        "mov r10, rax\n" 
                                        
                                        "mov rax, r9\n" ; mul numerators
                                        "mul r14\n"
                                        "mov r14, rax\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "jmp div_loop\n"
                                        "div_add_end:\n"
                                        "cmp r10, 1\n"
                                        "je div_print_integer\n"
                                        "mov r11, r14\n"
                                        "div_gcd:\n"
                                        "NUM_GCD r11, r10\n" ; result in rdi
                                        "div_after_gcd:"
                                        "mov rbx, rdi\n"
                                        "mov rax, r14\n"
                                        "mov rdx, 0\n" ; must do this for div operation to work
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r14, rax\n"
                                        "mov rax, r10\n"
                                        "CQO\n"
                                        "idiv rbx\n"
                                        "mov r10, rax\n"
                                        "cmp r10, 1\n"
                                        "je div_print_integer\n"
                                        "cmp r10, -1\n"
                                        "je div_print_integer\n"  
                                        "cmp r10, 0\n"          ;if denu is neg then change is to positive and numerator to neg
                                        "jge div_print_continue\n" 
                                        "neg r10\n"                                                                     
                                        "neg r14\n"             ;also if both are neg it will change both to positiove

                                        "div_print_continue:\n"
                                        
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "sal r10, 4\n"
                                        "or r10, T_INTEGER\n"

                                        "mymalloc 1\n"
                                        "mov r13, rax\n"
                                        "mov [r13], r14\n"
                                        "mymalloc 1\n"
                                        "mov r12, rax\n"
                                        "mov [r12], r10\n"
                                        "mymalloc 1\n"
                                                         
                                        "MAKE_MALLOC_LITERAL_FRACTION rax ,r13 ,r12\n"
                                        "jmp end_div\n"
                                        "div_print_integer:\n"
                                        "sal r14, 4\n"
                                        "or r14, T_INTEGER\n"
                                        "mymalloc 1\n"
                                        "mov qword[rax], r14\n"
                                        "end_div:"
                                        
                                        )))
               
               (general_lambda '/ code_div fvar_lst))))

(define gen_smaller_than (lambda (const_lst fvar_lst) (gen_compare const_lst fvar_lst "smaller_than" "jge" '< )))
(define gen_bigger_than (lambda (const_lst fvar_lst) (gen_compare const_lst fvar_lst "bigger_than" "jle" '> )))
(define gen_equal_to (lambda (const_lst fvar_lst) (gen_compare const_lst fvar_lst "equal_to" "jne" '=)))

(define gen_compare (lambda (const_lst fvar_lst compare_string compare_var compare_symbol)
             
             (let* (
                    
                   (compare_loop (string-append compare_var "_loop"))
                   (compare_fraction_exp (string-append compare_var "_fraction_exp"))
                   (compare_not_fraction (string-append compare_var "_not_fraction"))
                   (compare_same_denumartor (string-append compare_var "_same_denumartor"))
                   (compare_false (string-append compare_var "_false"))
                   (compare_end (string-append compare_var "_end"))
                   (compare_add_end (string-append compare_var "_add_end"))
                   (compare_continue (string-append compare_var "_continue"))
                   (code_compare 
                   
                     (string-append
                                        "mov r15, arg_count\n"
                                        "mov r14, 0\n" ; acc
                                        "mov r13, 0\n" ; cur_arg_num first argument is inserted to r14 after we see we have atleast 1 arg
                                        "mov r12, 0\n" ; cur arg
                                        "mov r11, 0\n" ; type check
                                        "mov r10, 1\n" ; current common denumator
                                        "mov r9, 0\n"  ; fraction numerator
                                        "mov r8, 1\n"  ; fraction denumarator
                                        "mov rdx, 0\n"  ; contains copy of cur arg with type
                                        
                                        compare_loop":\n"
                                        "cmp r15, 0\n"
                                        "je " compare_add_end "\n"
                                        "mov r12, An(r13)\n" ;get arg
                                        "mov r12, [r12]\n"
                                        "mov r11, r12\n"  ;check type
                                        "mov rdx, r12\n"
                                        "DATA r12\n"
                                        "TYPE r11\n"
                                        "cmp r11, T_FRACTION\n"
                                        "je "compare_fraction_exp"\n"
                                        "mov r9, r12\n" ; move number to numerator and 1 to denu
                                        "mov r8, 1\n"
                                        "jmp "compare_not_fraction"\n"
                                        compare_fraction_exp":\n"
                                        "mov r9, rdx\n" ;get numarator denumartor
                                        "mov r8, rdx\n"
                                        "CAR r9\n"
                                        "DATA r9\n"
                                        "CDR r8\n"
                                        "DATA r8\n" 
                                        compare_not_fraction":\n"  
                                        "cmp r8, r10\n"  ;check if denumartor same as curent
                                        
                                        "je "compare_same_denumartor"\n"
                                        "mov rax, r9\n"  ;mul new fraction numrator with common denu
                                        "mul r10\n" 
                                        "mov r9, rax\n"
                                        
                                        "mov rax, r8\n" ; mul acculmated sum with fraction denu
                                        "mul r14\n"
                                        "mov r14, rax\n"
                                        
                                        "mov rax, r10\n" ; new common denu
                                        "mul r8\n"
                                        "mov r10, rax\n"
                                        compare_same_denumartor":\n"
                                        "cmp r13, 0\n" ; if its the first arg then dont compare
                                        "jne "compare_continue "\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "mov r14, r9\n"
                                        "jmp " compare_loop "\n"
                                        
                                        
                                        compare_continue ":\n"
                                        "cmp r14, r9\n"
                                        compare_var" "compare_false"\n"
                                        "mov r14, r9\n"
                                        "add r13, 1\n"
                                        "sub r15, 1\n"
                                        "jmp "compare_loop"\n"
                                        compare_add_end ":\n"
                                        "mov rax, l0"  "\n"
                                        "jmp "compare_end"\n"
                                        compare_false":\n"
                                        "mov rax, l1" "\n"
                                        compare_end":"                                                                      
                                        )))
               
               (general_lambda compare_symbol code_compare fvar_lst))))






(define gen_vector (lambda (const_lst fvar_lst)
             
             (let ((code_vector (string-append
                                        "mov r15, arg_count\n"
                                        "mymalloc 1\n"
                                        
                                        "cmp r15, 1\n"
                                        ;"jne vector_1_args\n"
                                        "mov r15, A0\n"
                                        "mov r15, [r15]\n"
                                        ;"vector_1_args:\n"
                                        ;"mov r15, A0\n"
                                        ;"mov r15, [r15]\n"
                                        ;"MAKE_LITERAL_VECTOR rax, r15\n"
                                        )))
               
               (general_lambda 'vector code_vector fvar_lst))))

(define gen_car
  (lambda (const_lst fvar_lst)
                  (let ((code_is_car (string-append;   
                    "mov r10, A0\n"
                    "mov r10,[r10]\n"
                    "CAR r10\n"
                    "mymalloc 1\n"
                    "mov [rax], r10\n"      
                    
                    )))
                    (general_lambda 'car code_is_car fvar_lst))))

(define gen_cdr
  (lambda (const_lst fvar_lst)
                  (let ((code_is_cdr (string-append;   
                    "mov r10, A0\n"
                    "mov r10,[r10]\n"
                    "CDR r10\n"
                    "mymalloc 1\n"
                    "mov [rax], r10\n"      
                    )))
                    (general_lambda 'cdr code_is_cdr fvar_lst))))

(define gen_numerator (lambda (const_lst fvar_lst)
                        
                        (let ((code_numerator (string-append
                                                "mov r10, A0\n"
                                                "mov r10,[r10]\n"
                                                "CAR r10\n"
                                                "mymalloc 1\n"
                                                "mov [rax], r10\n"      
                                                )))
                          
                          (general_lambda 'numerator code_numerator fvar_lst))))

(define gen_denominator (lambda (const_lst fvar_lst)
                          
                          (let ((code_denominator (string-append
                                                    "mov r10, A0\n"
                                                    "mov r10,[r10]\n"
                                                    "CDR r10\n"
                                                    "mymalloc 1\n"
                                                    "mov [rax], r10\n"      
                                                    )))
                            
                            (general_lambda 'denominator code_denominator fvar_lst))))

; 

;(apply eq? map rational? remainder vector make-string make-vector string-set! symbol)


;(find_const_label '(const #f) const_lst) 
;;;;;;;;;;;;;;;;;;===========================================HELPERS=======================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tables_label_counter 0)
(define symbol_list '())

(define set_tabel_counter_plus_one 
  (lambda ()
    (set! tables_label_counter (+ tables_label_counter 1))))

(define del_if_created
  (lambda (file_check)
    (if (file-exists? file_check) 
        (begin (delete-file file_check) (open-output-file file_check))
        (open-output-file file_check))))


(define is_list_null? 
  (lambda (checked_list)
    (or (not (list? checked_list))
        (null? checked_list))))

(define get_length_string 
  (lambda (checked_string)
    (length (string->list checked_string))))

(define get_length_vector 
  (lambda (checked_vector)
    (length (vector->list checked_vector))))

(define get_length_symbol 
  (lambda (check_symbol)
    (string-length (symbol->string check_symbol))))


(define write_to_file
  (lambda (output_file content)
    (begin (display content output_file) (close-output-port output_file))))

(define label_counter
  (let ((if_count 0)
        (or_count 0)
        (general_lambda_count 0)
        (lamda_simple_count 0)
        (lambda_opt_count 0)
        (applic_count 0)
        (tc_applic_count 0)
        )
    (lambda (type)
      (cond ((equal? type 'if) (begin (set! if_count (+ if_count 1)) (- if_count 1)) )
            ((equal? type 'or) (begin (set! or_count (+ or_count 1)) (- or_count 1)) )
            ((equal? type 'lambda_simple) (begin (set! lamda_simple_count (+ lamda_simple_count 1)) (- lamda_simple_count 1)) )
            ((equal? type 'lambda_opt) (begin (set! lambda_opt_count (+ lambda_opt_count 1)) (- lambda_opt_count 1)) )
            ((equal? type 'tc_applic) (begin (set! tc_applic_count (+ tc_applic_count 1)) (- tc_applic_count 1)) )
            ((equal? type 'applic) (begin (set! applic_count (+ applic_count 1)) (- applic_count 1)) )
            ((equal? type 'general_lambda) (begin (set! general_lambda_count (+ general_lambda_count 1)) (- general_lambda_count 1)) )
            
            ))))

;;;;;;;;;;;;;;;;;;===========================================CONST=LIST=======================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define create_constant_list 
  (lambda (ast)                  
    (let* (
           (base `(#t ,#f () ,(void) ))
           (consts (find_consts ast))
           (lst_sort_topological (append base (map_slice_sort consts)))
           (reversed_consts_list (dups_delete (reverse lst_sort_topological)))
           )
      (reverse reversed_consts_list))))

(define is_const? 
  (lambda (var)
    (if (equal? var 'const) #t #f)))

(define find_consts
  (lambda (check_const)
    (cond ((null? check_const) '())
          ((not (list? check_const)) '())
          (else
            (let ((pre (car check_const)))
              (cond ((is_const? pre) 
                     (if  (and (number? (cadr check_const)) (not (integer? (cadr check_const)))) 
                         `(,(numerator (cadr check_const)) ,(denominator (cadr check_const)) ,(cadr check_const))
                         `(,(cadr check_const)))
                         )
                                           
                    (else `( ,@(find_consts pre) ,@(find_consts (cdr check_const) )))))))))

(define dups_delete 
  (lambda (var)
    (if (null? var) '() (let ((car_list (car var))
                              (cdr_list (cdr var))
                              )
                          (cond ((member car_list cdr_list) (dups_delete cdr_list))
                                (else (cons car_list (dups_delete cdr_list))))))))

(define map_slice_sort
  (lambda (var)
    
    (if (null? var) 
        '() 
        (let (
              (car_list (car var))
              (cdr_list  (cdr var))
              )
          `(,@(sort_topologicaly car_list) ,@(map_slice_sort cdr_list))))))

(define sort_topologicaly
  (lambda (var)
    
    (cond 
      ((vector?  var) `(,@(map_slice_sort (vector->list var)) ,var )) 
      ((not (pair? var)) `(,var))
      
      ((not (pair? (car var))) `(,(car var) ,@(sort_topologicaly (cdr var)) ,var))      		; list first element not pair
      
      (else `(,@(sort_topologicaly (car var)) ,@(sort_topologicaly (cdr var) ) ,var)))))		; list first element is complex


;;;;;;;;;;;;;;;;;;===========================================FreeVar========================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define free_vars_in_scheme
  '( apply < = > + / * - boolean? car cdr char->integer char? cons denominator eq? integer? integer->char list make-string make-vector map not null? number? numerator pair? procedure? rational? remainder set-car! set-cdr! string-length string-ref string-set! string->symbol string? symbol? symbol->string vector vector-length vector-ref vector-set! vector? zero?)
  )


(define create_fvar_list
  (lambda (var)
    (let* ((scheme_fvar free_vars_in_scheme)
           (fvar_in_ast (dups_delete (find_fvar var))))
      
       (append fvar_in_ast scheme_fvar))))

(define counter_fvar_list
  (lambda (fvar_list)
    (map (lambda (x) (begin (set! tables_label_counter (+ tables_label_counter 1)) `(,tables_label_counter ,x))) fvar_list)
    ))

(define find_fvar
  (lambda (var)
    (cond ((null? var) '())
          ((not (list? var)) '())
          ((equal? (car var) 'fvar) (cdr var))
          (else `( ,@(find_fvar (car var) ) ,@(find_fvar (cdr var) ))))
    ))

(define gen_fvar_table 
  (lambda (fvar_table)
    (if (eq? fvar_table '()) ""
        (let ((make_var (lambda (var)    
                          (string-append "l" (number->string (car var)) ":\ndq SOB_UNDEFINED\n"))))
          (apply string-append (map make_var fvar_table))))))


(define find_free_label (lambda (parsed_var fvar_lst)
                          (if (is_list_null? fvar_lst) (begin (display "free var not found") (string-append "free var not found " parsed_var))
                              (if (equal? (cadar fvar_lst) parsed_var)  
                                  (string-append "l" (number->string (caar fvar_lst)))
                                  (find_free_label parsed_var (cdr fvar_lst))
                                  ))))

;;;;;;;;;;;;;;;;;;===========================================END FreeVar========================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;=========================================== CONSTVar=TABLE=======================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define startPrefix (lambda (address)
                      (string-append "l" address ":\ndq ")))

(define gen_constant_table (lambda (table)
                             (if (is_list_null? table) ""         
                                 (let ((make_var (lambda (var)
                                                   (let* ((type (caddr var))
                                                          (address (number->string (car var))))
                                                     
                                                     (cond ((equal? type 'T_NIL) (string-append (startPrefix address) " SOB_NIL\n"))
                                                           ((equal? type 'T_VOID) (string-append (startPrefix address)" SOB_VOID\n"))
                                                           ((and (equal? type 'T_BOOL) (equal? (cadr var) #f))  (string-append (startPrefix address) "SOB_FALSE\n"))
                                                           ((and (equal? type 'T_BOOL) (equal? (cadr var) #t))  (string-append (startPrefix address) "SOB_TRUE\n"))
                                                           ((equal? type 'T_CHAR) (string-append (startPrefix address) " MAKE_LITERAL(T_CHAR, " (number->string (char->integer (cadr var))) ")\n "))
                                                           ((equal? type 'T_INTEGER)  (string-append (startPrefix address) "MAKE_LITERAL(T_INTEGER, "  (number->string (cadr var)) ")\n"))
                                                           ((equal? type 'T_PAIR)  (string-append (startPrefix address) "MAKE_LITERAL_PAIR(l" (number->string (car (cadddr var))) ", l" (number->string (cadr (cadddr var))) ")\n"))
                                                           ((equal? type 'T_STRING) (string-append  "l" address ":\n" "MAKE_LITERAL_STRING \"" (cadr var) "\"\n"))
                                                           ((equal? type 'T_VECTOR)  (string-append  "l" address ":\n"  " MAKE_LITERAL_VECTOR l" (number->string (car (cadddr var))) (process_vec (cdr (cadddr var))) "\n"))
                                                           ((equal? type 'T_SYMBOL) (string-append (startPrefix address) "MAKE_LITERAL_T_SYMBOL," (symbol->string (cadr var)) ")\n" ))
                                                           ((equal? type 'T_FRACTION)  (string-append (startPrefix address) "MAKE_LITERAL_FRACTION(l" (number->string (car (cadddr var))) ", l" (number->string (cadr (cadddr var))) ")\n"))
                                                           
                                                           )))))
                                   
                                   
                                   (apply string-append (map make_var table))))))



(define process_vec (lambda (vector_vars) 
                      (let* (
                             (mapped (map (lambda (x) (string-append ", l" (number->string x))) vector_vars))
                             (vec_string (apply string-append mapped))
                             )
                        vec_string
                        )))




;;;;;;;;;;;;;;;;;;=========================================== CODE_GEN =======================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define code-gen
  (lambda (var_parsed const_list fvar_list major)
    (if (null? var_parsed) 
        ""
        (let ((car_var (car var_parsed)))
          (display "\n")
          (display var_parsed)
          (display "\n")
          (string-append 
            
 
          (cond 
            ((equal? 'const car_var) (gen_const var_parsed const_list fvar_list major))
            ((equal? 'if3 car_var) (gen_if3 var_parsed const_list fvar_list major)) 
            ((equal? 'or car_var) (gen_or (cadr var_parsed) const_list fvar_list major))
            ((equal? 'seq car_var) (gen_seq (cadr var_parsed) const_list fvar_list major))    
            ((equal? 'set car_var) (gen_set var_parsed const_list fvar_list major))      
            ((equal? 'pvar car_var) (gen_pvar var_parsed const_list fvar_list major))       
            ((equal? 'bvar car_var) (gen_bvar var_parsed const_list fvar_list major))       
            ((equal? 'fvar car_var) (gen_fvar var_parsed const_list fvar_list major)) 
            ((equal? 'define car_var) (gen_define var_parsed const_list fvar_list major))      
            ((equal? 'lambda-simple car_var) (gen_lambda_simple var_parsed const_list fvar_list major))      
            ((equal? 'lambda-opt car_var) (gen_lambda_opt var_parsed const_list fvar_list major))      
            ((equal? 'applic car_var) (gen_applic var_parsed const_list fvar_list major)) 
            ((equal? 'tc-applic car_var) (gen_tc_applic var_parsed const_list fvar_list major))     
            (else    "mov rax, -1\n" )) 
          
          ) 
 
          )))) 

(define gen_const (lambda (var_parsed const_list fvar_list major)
                    (string-append "mov rax, " (find_const_label var_parsed const_list) "\n")))

(define gen_seq (lambda (parsed_list const_list fvar_list major)
                  (if (is_list_null? parsed_list) ""
                      (string-append (code-gen (car parsed_list) const_list fvar_list major) (gen_seq (cdr parsed_list) const_list fvar_list major)))))


(define gen_applic (lambda (parsed_list const_list fvar_lst major)
                     (let* (
                           (rev_arguments (reverse (caddr parsed_list)))
                           (proc (cadr parsed_list))
                           (args_len (length (caddr parsed_list)))
                           (applic_label (number->string (label_counter 'applic)))
                           (code_gened_arguments (apply string-append (map (lambda (x) 
                                                   (let (
                                                     (gened_var (code-gen x const_list fvar_lst major)))
                                                      (string-append "\n"
                                                        gened_var "\n"
                                                        "push rax"))) rev_arguments))))
                       (string-append
                         "\n;--------------APPLIC--------------\n"
                       "push " nil_label "\n"
                       code_gened_arguments "\n"
                       "push " (number->string args_len) "\n"
                       
                       (code-gen proc const_list fvar_lst major ) "\n"
                       
                       "mov rbx, [rax]\n"
                       "CLOSURE_ENV rbx\n"
                       "push rbx\n"
                       "mov rax, [rax]\n"
                       "CLOSURE_CODE rax\n"
                       "call rax\n"
                        "mov r15, [rsp+8]\n"
                        "add r15, 2\n"
                        "pop_args"applic_label":\n"
                        "cmp r15, 0\n"
                        "je " "end_pop_args"applic_label" \n"
                        "pop r14\n"  
                        "sub r15, 1\n"
                        "jmp " "pop_args"applic_label" \n"

                        "end_pop_args"applic_label":\n"
                        "pop r14\n"
                        "end_applic"applic_label":\n"
                        "\n;----------------END APPLIC-------------------\n"
                        
                       ))))




(define gen_tc_applic (lambda (parsed_list const_list fvar_lst major)
                     (let* (
                           (rev_arguments (reverse (caddr parsed_list)))
                           (proc (cadr parsed_list))
                           (args_len (length (caddr parsed_list)))
                           (applic_label (number->string (label_counter 'tc_applic))) ;same counter doesnt matter
                           (code_gened_arguments (apply string-append (map (lambda (x) 
                                                   (let (
                                                     (gened_var (code-gen x const_list fvar_lst major)))
                                                      (string-append "\n"
                                                        gened_var "\n"
                                                        "push rax"))) rev_arguments))))
                       (string-append
                      code_gened_arguments "\n"
                      "\n;-----------TC_APPLIC ----------\n"
                      "push " (number->string args_len) "\n"
                      
                      (code-gen proc const_list fvar_lst major ) "\n"
                      
                      "mov rbx, [rax]\n"
                      "CLOSURE_ENV rbx\n"
                      "push rbx\n"
                      "mov rax, [rax]\n"
                      "CLOSURE_CODE rax\n"

                      "mov rbx, ret_addr\n"
                       "push rbx\n"
                       "mov rdx, arg_count\n"
                        
                       "mov rbx, rbp\n"      
                       "mov rbp, old_rbp\n"    
                       
                       "add rdx, 4\n" ;arg_count + 4
                       "shl rdx, 3\n" ; 8 * (arg_count + 4)
                       "add rdx, rbx\n" ; prev rbp + 8 * (arg_count + 4)
                       "mov rcx, "(number->string (+ args_len 4)) "\n"
                       
                       "tc_applic_move_loop_"applic_label":\n"
                       "cmp rcx, 0\n" ; init = argcount + 4
                       "je tc_applic_done_" applic_label "\n"
                       "sub rbx, 8\n" ; rbp
                       "sub rdx, 8\n" ; old rbp + 8 * (arg_count + 4)
                       "mov r15, [rbx]\n"
                       "mov [rdx], r15\n"
                       "sub rcx, 1\n"
                       "jmp tc_applic_move_loop_" applic_label "\n"
                       "tc_applic_done_"applic_label ":\n" 
                       "mov rsp, rdx\n"
                       "jmp rax\n" 
                        "\n;-----------END TC_APPLIC ----------\n"
                       ))))

(define gen_lambda_opt (lambda (exp const_list fvar_list major)
                            (let(
                                  (lambda_opt_label (number->string (label_counter 'lambda_opt)))
                                  (paramLoop (string-append "paramLoop" lambda_opt_label))
                                  (paramLoopEnd (string-append "paramLoopEnd" lambda_opt_label))
                                  (paramCounter 0)
                                  (envLoop (string-append "envLoop" lambda_opt_label))
                                  (envLoopEnd (string-append "envLoopEnd" lambda_opt_label))
                                  (lambdaBody (string-append "lambdaBody" lambda_opt_label))
                                  (END (string-append "endLambdaOpt" lambda_opt_label))
                                  (argNumber (number->string (length (cadr exp))))
                                  (i 0)
                                  (j 1)
                                  )


                                (string-append
                                ;"mov rdi, 2\n"
                                "mov r13, " argNumber "\n"
                                "mymalloc 2\n"
                                "mov rdx, rax\n" 
                                "malloc1:\n"
                                ;"mov rdi, " (number->string  (+ major 1)) "\n"

                                "mymalloc " (number->string  (+ major 1)) "\n"
                                "mov rbx, rax\n"
                                "malloc2:\n"
                                ;"mov rdi, arg_count\n" 

                                "mymalloc r13\n"
                                "malloc3:\n"
                                "mov rcx, rax\n"
                                "mov rax, rdx\n"
                                "mov rdx, r13\n"

                                "mov r12, 0\n"    ; define qword [rbp + offset]
                                paramLoop ":\n\n"
                                "   cmp rdx, 0\n"
                                "   je " paramLoopEnd "\n"
                                "   mov r8, qword [rbp + (4 + r12) * 8]\n"
                                "checkr8:\n"
                                "   mov qword [rcx + r12 * 8], r8\n" 
                                "   add r12, 1\n"
                                "   sub rdx, 1\n"
                                "jmp " paramLoop "\n\n"

                                paramLoopEnd ":\n"

                                "mov [rbx], rcx\n"
                                "mov r14, [rbx]\n"
                                "mov r10, 0\n"
                                "mov r11, 1\n"

                                envLoop ":\n"     
                                "   cmp  r10, " (number->string major) "\n"
                                "   jge " envLoopEnd "\n"
                                "   mov r9, env\n"
                                "   mov r8, [r9 + r10 * 8]\n"
                                "   mov [rbx + r11 * 8], r8\n"
                                "   add r10, 1\n"
                                "   add r11, 1\n"
                                "   jmp " envLoop "\n\n"
                                envLoopEnd ":\n"


                                "mov r8, " lambdaBody "\n"
                                "MAKE_LITERAL_CLOSURE rax, rbx, r8\n"

                                "jmp END\n"
                                lambdaBody ":\n"
                                "push rbp\n"
                                "mov rbp, rsp\n"
                                
                                ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ZABARI CHANGE
                                
                                "mov rbx, " nil_label "\n"
                                "mov r10, arg_count\n"
                                
                                "for_fix_stack"":\n"
                                "cmp r10, " argNumber "\n"

                                "je " "end_of_fix_stack" "\n"

                                
                                "mymalloc 1\n"     
                                "mov rdx, rbp\n"        
                                "add rdx, 4*8\n"            ;rdx point to n+m in stack (offset)
                                "mov r11, r10\n"            ;r10 is helper for point of arg[i]
                                "dec r11\n"
                                "shl r11, 3\n"              ;now offset+r10 = address of curr arg       
                                "add rdx, r11\n"            ;rdx = address of arg[i]
                                "mov rdx, qword [rdx]\n"    
                                "asdasd:"
                                "MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"  ;rax = target, rbx = cdr, rcx = car
                                "mov rbx, rax\n"        ;rbx points to the new pair as cdr for the new allocate in next iteration
                                "dec r10\n"         
                                "jmp " "for_fix_stack" "\n"
                                "end_of_fix_stack"":\n"
                                "mov qword [rbp+4 * 8 + " argNumber "*8], rbx\n" ;add the list in stack after const params (not optinals)
                                "mov qword [rbp + 3*8], "(number->string (+ 1 (length (cadr exp))))"\n"
                                "mov r10, arg_count\n"
                                
                                ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ZABARI CHANGE

                                (code-gen (cadddr exp) const_list fvar_list (+ major 1))  "\n"
                                "leave\n"
                                "ret\n"
                                END ":\n\n"
                                ))))

 (define gen_lambda_simple (lambda (exp const_list fvar_list major)
                             (let*(
                                   (lambda_simple_label (number->string (label_counter 'lambda_simple)))
                                   (paramLoop (string-append "paramLoop_" lambda_simple_label))
                                   (paramLoopEnd (string-append "paramLoopEnd_" lambda_simple_label))
                                   (paramCounter 0)
                                   (envLoop (string-append "envLoop_" lambda_simple_label))
                                   (envLoopEnd (string-append "envLoopEnd_" lambda_simple_label))
                                   (lambdaBody (string-append "lambdaBody_" lambda_simple_label))
                                   (END (string-append "END_" lambda_simple_label))
                                   (arg_is_not_zero (string-append "arg_is_not_zero_" lambda_simple_label))
                                   (argNumber (length (cadr exp)))
                                   (i 0)
                                   (j 1)
                                   )
                               (string-append
                                 ;"mov rdi, 2\n"
                                 "\n;-------------- LAMBDA_SIMPLE-----------------------\n"
                                 "mymalloc 2\n"
                                 "mov rdx, rax\n" 
                                 
                                 "mymalloc 1 \n" ; 2
                                 "mov rbx, rax\n"       
                                 
                                 
                                 (if (> major 0)
                                     (string-append
                                         
                                       
                                       
                                       "mymalloc " (number->string  (+ major 1)) "\n" ; 2
                                       "mov rbx, rax\n"  
                                       
                                       "mov r13, arg_count\n"
                                       
                                       "mymalloc r13\n" ; malloc 0
                                       
                                       "mov rcx, rax\n" ;rdx = closuree, rbx = env , rcx = params 
                                       "mov rax, rdx\n"
                                       "mov rdx, r13\n" ; rax = closure  rdx = arg num
                                       
                                       "mov r12, 0\n"    ; define qword [rbp + offset]
                                       paramLoop ":\n\n"
                                       "   cmp rdx, 0\n"
                                       "   je " paramLoopEnd "\n"
                                       "   mov r8, qword [rbp + (4 + r12) * 8]\n"
                                       
                                       "   mov qword [rcx + r12 * 8], r8\n" 
                                       "   add r12, 1\n"
                                       "   sub rdx, 1\n"
                                       "jmp " paramLoop "\n\n"
                                       
                                       paramLoopEnd ":\n"
                                       
                                       "mov [rbx], rcx\n"
                                       "mov r14, [rbx]\n" ; r14 = params ; rcx = params   rbx = env
                                       
                                       "mov r10, 0\n" 
                                       "mov r11, 1\n"
                                       
                                       envLoop ":\n"     
                                       "   cmp  r10, " (number->string major) "\n" ; 1
                                       "   jge " envLoopEnd "\n"
                                       "   mov r9, env\n"
                                       "   mov r8, [r9 + r10 * 8]\n"
                                       "   mov [rbx + r11 * 8], r8\n"
                                       "   add r10, 1\n"
                                       "   add r11, 1\n"
                                       "   jmp " envLoop "\n\n"
                                       envLoopEnd ":\n"
                                       )"")
                                 
                                 "mov r8, " lambdaBody "\n"
                                 "MAKE_LITERAL_CLOSURE rax, rbx, r8\n"
                                 
                                 "jmp "END"\n"
                                 lambdaBody ":\n"
                                 "push rbp\n"
                                 "mov rbp,rsp\n"
                                 (code-gen (caddr exp) const_list fvar_list (+ major 1))  "\n"
                                 "leave\n"
                                 "ret\n"
                                 END ":\n\n"
                                 
                                 "\n;--------------END LAMBDA_SIMPLE-----------------------\n"
                                 ))))








(define gen_if3 (lambda (parsed_list const_list fvar_list major)
                  (let* ((test (cadr parsed_list))
                         (if_t (caddr parsed_list))
                         (if_f (cadddr parsed_list))
                         (label_num (label_counter 'if))
                         (else-label (string-append "lif_else" (number->string label_num)))
                         (exit-label (string-append "lif_end" (number->string label_num))))
                    (string-append
                      (code-gen test const_list fvar_list major)
                      "cmp rax, " (find_const_label '(const #f) const_list) " \n"
                      "je " else-label "\n"
                      (code-gen if_t const_list fvar_list major)
                      "jmp " exit-label "\n"
                      else-label ":\n"
                      (code-gen if_f const_list fvar_list major)
                      exit-label ":\n"))))

(define find_const_label (lambda (parsed_var const_list)               
                     (if (is_list_null? const_list) (string-append "const not found " (number->string  (cadr parsed_var)))
                         (let (
                               (var_value_const_list (cadar const_list))
                               (var_value_parse (cadr parsed_var))
                               (var_label (caar const_list)))
                           (if (equal? var_value_const_list var_value_parse) (string-append "l" (number->string var_label))
                               (find_const_label parsed_var (cdr const_list)))))))


(define gen_define (lambda (parsed_var const_list fvar_list major)
                     (string-append
                       (code-gen (caddr parsed_var) const_list fvar_list major)
                       "mov [" (find_free_label (cadadr parsed_var) fvar_list )"], rax\n"
                       "mov rax, " void_label "\n")))



(define gen_or (lambda (parsed_list const_list fvar_list major)
                 (letrec ((gen_list (map (lambda (parsed_list_var) (code-gen parsed_list_var const_list fvar_list major)) parsed_list))
                          (or_label (label_counter 'or))
                          (gen_cmp (lambda (gen_vars)
                                     (if (is_list_null? (cdr gen_vars)) (string-append (car gen_vars) "else_l" (number->string or_label) ":\n\n")
                                         (string-append (car gen_vars)
                                                        "cmp rax, " (find_const_label '(const #f) const_list) " \n"
                                                        "jne else_l" (number->string or_label) "\n"
                                                        (gen_cmp (cdr gen_vars)))))))
                   (gen_cmp gen_list))))

(define gen_box (lambda (parsed_var const_list fvar_list major)
                  (let ((minor (car (cddadr parsed_var))))
                    (string-append
                      "call mymalloc\n"
                      "mov [rax], qword[rbp + "(number->string minor)"*8]\n"
                      ))))

(define gen_box_get (lambda (parsed_var const_list fvar_list major)
                      (let ((gen_var (code-gen (cadr parsed_var) major)))
                        (string-append
                          gen_var
                          "mov rax, [rax]\n"
                          ))))

(define gen_box_set
  (lambda (parsed_var const_list fvar_list major)
    (let ((pe (caddr parsed_var))
          (var (cadr parsed_var)))
      (string-append (code-gen pe const_list fvar_list major)   
                     "mov rbx, rax\n"
                     (code-gen var const_list fvar_list major)
                     "mov [rax], rbx\n"
                     "mov rax, " void_label "\n"))  ; check if (void) is good
    ))


(define orLabelGenerator
  (let ((count 0))
    (lambda ()
      (begin (set! count (+ count 1)) (string-append "l_or_exit_" (number->string (- count 1)))))))



(define gen_pvar (lambda (parsed_var const_list fvar_list major)
                   (let ((p_minor (caddr parsed_var)))
                     (string-append
                       "mov rax, qword[rbp+(4+"(number->string p_minor)")*8]\n"))
                   ))



(define gen_bvar (lambda (parsed_var const_list fvar_list major)
                   (let ((b_major (caddr parsed_var))
                         (b_minor (cadddr parsed_var)))
                     (string-append
                       "mov rax, qword [rbp + 2*8]\n"
                       "mov rax, qword [rax+"(number->string b_major)" * 8]\n"
                       "mov rax, qword [rax+"(number->string b_minor)" * 8]\n"
                       ))
                   
                   ))

(define gen_fvar (lambda (parsed_var const_list fvar_list major)  
                   (string-append
                     "mov rax, [" (find_free_label (cadr parsed_var) fvar_list) "]\n")))

(define gen_set (lambda (parsed_var const_list fvar_list major)
                  
                  (let* ((var (cadr parsed_var))
                         (parsed_exp (caddr parsed_var))
                         (gen_parsed_exp (code-gen parsed_exp const_list fvar_list major))
                         (type (car var)) 
                         (getArgument (cond ((equal? type 'pvar)
                                             (let* ((p_minor (caddr var)))
                                               (string-append "mov qword[rbp + (4+"(number->string (*  p_minor 8))")], rax\n"
                                                              "mov rax, " void_label)))
                                            ((equal? type 'bvar)
                                             (let* ((env_var (cddr var)) ;major and minor in a list
                                                    (b_major (car env_var))
                                                    (b_minor (cadr env_var)))
                                               (string-append "mov rbx, qword[rbp + 2 * 8]\n" 
                                                              "mov rbx, qword[rbx + " (number->string (*  b_major 8)) "]\n"
                                                              "mov qword[rbx + " (number->string (*  b_minor 8)) "], rax"
                                                              "mov rax, "void_label "\n"
                                                              )))
                                            ((equal? type 'fvar)
                                             (let* ((fvar (cadr var))
                                                    (env_var (find_free_label fvar fvar_list)))
                                               (string-append 
                                                 ";##free var##\n"
                                                 "mov [" env_var "], rax\n" )))
                                            (else 'gen_set_error))))
                    (string-append  gen_parsed_exp
                                   getArgument
                                   "mov rax, " void_label "\n" ))))

;;;;;;;;;;;;;;;;;;===========================================END code-gen=======================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define create_const_table (lambda (lst accumlated_list)
                             (if (eq? lst (list)) (reverse accumlated_list)
                                 (let (
                                       (car_lst (car lst))
                                       (cdr_lst (cdr lst))
                                       (counter tables_label_counter)
                                       )
                                   (begin (set_tabel_counter_plus_one) 
                                     (cond ((equal? car_lst (list)) (create_const_table cdr_lst (cons `(,counter ,car_lst T_NIL ()) accumlated_list)))
                                           ((equal? car_lst (void))  (create_const_table cdr_lst (cons `(,counter ,car_lst T_VOID ()) accumlated_list)))
                                           ((equal? car_lst #t)    (create_const_table cdr_lst (cons `(,counter ,car_lst T_BOOL ()) accumlated_list)))
                                           ((equal? car_lst #f)  (create_const_table cdr_lst (cons `(,counter ,car_lst T_BOOL ()) accumlated_list)))
                                           ((char? car_lst)  (create_const_table cdr_lst (cons `(,counter ,car_lst  T_CHAR ,`( ,(char->integer car_lst))) accumlated_list))  )
                                           ((string? car_lst)   (create_const_table cdr_lst (cons `(,counter ,car_lst T_STRING ,`( ,(get_length_string car_lst) ,@(map (lambda (ch) (char->integer ch)) (reverse (string->list car_lst ))))) accumlated_list)))
                                           ((integer? car_lst)   (create_const_table cdr_lst(cons `(,counter ,car_lst T_INTEGER ,`(  ,car_lst)) accumlated_list)))
                                           ((vector? car_lst)  (create_const_table cdr_lst (cons `(,counter ,car_lst T_VECTOR ,`( ,(get_length_vector car_lst) ,@(map (lambda (const) (find_index const accumlated_list)) (reverse (vector->list car_lst ))))) accumlated_list)))
                                           ((pair? car_lst)  (create_const_table cdr_lst (cons `(,counter ,car_lst T_PAIR ,`( ,(find_index (caar lst) accumlated_list) ,(find_index (cdar lst) accumlated_list) )) accumlated_list))  )
                                           ((symbol? car_lst)  (set! symbol_list (cons (find_index (symbol->string car_lst) accumlated_list) symbol_list)) (create_const_table (cdr lst) (cons `(,counter ,car_lst T_SYMBOL ,`( ,(find_index (symbol->string car_lst ) accumlated_list))) accumlated_list)))
                                           (else (create_const_table cdr_lst (cons `(,counter ,car_lst T_FRACTION ,`( ,(find_index (numerator car_lst) accumlated_list) ,(find_index (denominator car_lst) accumlated_list) )) accumlated_list)))
                                           ))))))


(define find_index (lambda (var const_lst)
                     (cond ((equal? const_lst (list)) "const not found")
                           ((equal? var (cadar const_lst)) (caar const_lst))
                           (else (find_index var (cdr const_lst))))))








;(compile-scheme-file "tzabari.scm" "tzabari.s")

;(define gen_lambda_opt (lambda (exp const_list fvar_list major)
;                            (let(
;                                  (lambda_opt_label (number->string (label_counter 'lambda_opt)))
;                                  (paramLoop (string-append "paramLoop" lambda_opt_label))
;                                  (paramLoopEnd (string-append "paramLoopEnd" lambda_opt_label))
;                                  (paramCounter 0)
;                                  (envLoop (string-append "envLoop" lambda_opt_label))
;                                  (envLoopEnd (string-append "envLoopEnd" lambda_opt_label))
;                                  (lambdaBody (string-append "lambdaBody" lambda_opt_label))
;                                  (END (string-append "endLambdaOpt" lambda_opt_label))
;                                  (argNumber (number->string (length (cadr exp))))
;                                  (i 0)
;                                  (j 1)
;                                  )


;                                (string-append
;                                ;"mov rdi, 2\n"
;                                "mov r13, " argNumber "\n"
;                                "mymalloc 2\n"
;                                "mov rdx, rax\n" 
;                                "malloc1:\n"
;                                ;"mov rdi, " (number->string  (+ major 1)) "\n"

;                                "mymalloc " (number->string  (+ major 1)) "\n"
;                                "mov rbx, rax\n"
;                                "malloc2:\n"
;                                ;"mov rdi, arg_count\n" 

;                                "mymalloc r13\n"
;                                "malloc3:\n"
;                                "mov rcx, rax\n"
;                                "mov rax, rdx\n"
;                                "mov rdx, r13\n"

;                                "mov r12, 0\n"    ; define qword [rbp + offset]
;                                paramLoop ":\n\n"
;                                "   cmp rdx, 0\n"
;                                "   je " paramLoopEnd "\n"
;                                "   mov r8, qword [rbp + (4 + r12) * 8]\n"
;                                "checkr8:\n"
;                                "   mov qword [rcx + r12 * 8], r8\n" 
;                                "   add r12, 1\n"
;                                "   sub rdx, 1\n"
;                                "jmp " paramLoop "\n\n"

;                                paramLoopEnd ":\n"

;                                "mov [rbx], rcx\n"
;                                "mov r14, [rbx]\n"
;                                "mov r10, 0\n"
;                                "mov r11, 1\n"

;                                envLoop ":\n"     
;                                "   cmp  r10, " (number->string major) "\n"
;                                "   jge " envLoopEnd "\n"
;                                "   mov r9, env\n"
;                                "   mov r8, [r9 + r10 * 8]\n"
;                                "   mov [rbx + r11 * 8], r8\n"
;                                "   add r10, 1\n"
;                                "   add r11, 1\n"
;                                "   jmp " envLoop "\n\n"
;                                envLoopEnd ":\n"


;                                "mov r8, " lambdaBody "\n"
;                                "MAKE_LITERAL_CLOSURE rax, rbx, r8\n"

;                                "jmp END\n"
;                                lambdaBody ":\n"
;                                "push rbp\n"
;                                "mov rbp, rsp\n"
                                
;                                ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ZABARI CHANGE
                                
;                                "mov rbx, " nil_label "\n"
;                                "mov r10, arg_count\n"
                                
;                                "for_fix_stack"":\n"
;                                "cmp r10, " argNumber "\n"

;                                "je " "end_of_fix_stack" "\n"

                                
;                                "mymalloc 1\n"     
;                                "mov rdx, rbp\n"        
;                                "add rdx, 4*8\n"            ;rdx point to n+m in stack (offset)
;                                "mov r11, r10\n"            ;r10 is helper for point of arg[i]
;                                "dec r11\n"
;                                "shl r11, 3\n"              ;now offset+r10 = address of curr arg       
;                                "add rdx, r11\n"            ;rdx = address of arg[i]
;                                "mov rdx, qword [rdx]\n"    
;                                "asdasd:"
;                                "MAKE_MALLOC_LITERAL_PAIR rax, rdx, rbx\n"  ;rax = target, rbx = cdr, rcx = car
;                                "mov rbx, rax\n"        ;rbx points to the new pair as cdr for the new allocate in next iteration
;                                "dec r10\n"         
;                                "jmp " "for_fix_stack" "\n"
;                                "end_of_fix_stack"":\n"
;                                "mov qword [rbp+4 * 8 + " argNumber "*8], rbx\n" ;add the list in stack after const params (not optinals)
;                                "mov qword [rbp + 3*8], "(number->string (+ 1 (length (cadr exp))))"\n"
;                                "mov r10, arg_count\n"
                                
;                                ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ZABARI CHANGE

;                                (code-gen (cadddr exp) const_list fvar_list (+ major 1))  "\n"
;                                "leave\n"
;                                "ret\n"
;                                END ":\n\n"
;                                ))))







                                ;"mov r14, " nil_label "\n"
                                ;"mov r13, arg_count\n"
                                
                                ;"for_fix_stack"":\n"
                                ;"cmp r13, " argNumber "\n"

                                ;"je " "end_of_fix_stack" "\n"

                                
                                ;"mymalloc 1\n"     
                                ;"mov rbx, rbp\n"        
                                ;"add rbx, 4*8\n"            ;rbx point to n+m in stack (offset)
                                ;"mov r12, r13\n"            ;r13 is helper for point of arg[i]
                                ;"sub r12, 1\n"
                                ;"shl r12, 3\n"              ;now offset+r13 = address of curr arg       
                                ;"add rbx, r12\n"            ;rbx = address of arg[i]
                                ;"mov rbx, qword [rbx]\n"    
                                
                                ;"MAKE_MALLOC_LITERAL_PAIR rax, rbx, r14\n"  ;rax = target, r14 = cdr, rcx = car
                                ;"mov r14, rax\n"        ;r14 points to the new pair as cdr for the new allocate in next iteration
                                ;"sub r13, 1\n"         
                                ;"jmp " "for_fix_stack" "\n"
                                ;"end_of_fix_stack"":\n"
                                ;"mov rdx, A0\n"
                                ;"mov qword [rbp+ 4 * 8 + " argNumber " * 8], r14\n" ;add the list in stack after const params (not optinals)
                                ;"mov qword [rbp + 3 * 8], "(number->string (+ 1 (length (cadr exp))))"\n"
                                ;"mov r13, arg_count\n"