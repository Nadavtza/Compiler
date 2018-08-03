(load "pc.scm")

(define <sexpr>
  (new
    (*delayed (lambda () <SexprSkip>)) *star 
    (*delayed (lambda () <Boolean>))
    (*delayed (lambda () <Char>))
    (*delayed (lambda () <Number>))
    (*delayed (lambda () <Symbol>))
    (*delayed (lambda () <String>))
    (*delayed (lambda () <ProperList>))
    (*delayed (lambda () <ImproperList>))
    (*delayed (lambda () <Vector>))
    (*delayed (lambda () <Quoted>))
    (*delayed (lambda () <QuasiQuoted>))
    (*delayed (lambda () <Unquoted>))
    (*delayed (lambda () <UnquoteAndSpliced>))
    (*delayed (lambda () <CBName>))
    (*delayed (lambda () <InfixExtension>))
    (*disj 14)
    (*delayed (lambda () <SexprSkip>)) *star
    (*caten 3)
    (*pack-with (lambda (space sexpr space2) sexpr))
    done))


(define <WhiteSpace>
  (const (lambda (ch) (char<=? ch #\space))))

;***********************************************************************************************************************************
(define <LineComment>
  (new
    (*parser (char #\;))
    (*parser <any-char>)
    (*parser (char #\newline))
    (*parser <end-of-input>)
    (*disj 2)
    *diff
    *star
    (*parser (char #\newline))
    (*parser <end-of-input>)
    (*disj 2)
    (*caten 3)
    done))

;***********************************************************************************************************************************


(define <SexprComment>
  (new
    (*parser (word "#;")) *plus
    (*parser <WhiteSpace>) *star
    (*delayed (lambda () <sexpr>))
    (*parser <WhiteSpace>) *star
    (*caten 4)
    (*pack-with (lambda (prefix skip1 exp skip2) (list)))
    done))


;***********************************************************************************************************************************

(define <InfixComment>
  (new (*parser (word "#;")) *plus
       (*parser <WhiteSpace>) *star
       (*delayed (lambda () <InfixExpression>))
       (*parser <WhiteSpace>) *star
       (*caten 4)
       (*pack-with (lambda (prefix skip1 exp skip2) (list)))
       done))

;***********************************************************************************************************************************

(define <SexprSkip>
  (new
    (*parser <SexprComment>)
    (*parser <LineComment>)
    (*parser <WhiteSpace>)
    (*disj 3) 
    done))

;***********************************************************************************************************************************

(define <InfixSkip>
  (new
    (*parser <InfixComment>)
    (*parser <LineComment>)
    (*parser <WhiteSpace>)
    (*disj 3)
    done))


;***********************************************************************************************************************************

(define <Boolean>
  (new
    (*parser (char #\#))
    (*parser (char-ci #\t))
    (*parser (char-ci #\f))
    (*disj 2)
    (*caten 2)
    (*pack (lambda(bool)
             (if (equal? (list->string bool) "#t") #t #f))
           )
    done))

;***********************************************************************************************************************************
(define <op>
  (new ;initialize a parser stack
       (*parser (char #\+)) ; create and push a parser for the ’+’ operator
       (*parser (char #\-)) ; create and push a parser for the ’-’ operator
       (*disj 2) ; pop top 2 parsers, and push their disjunction
       ;(*pack (lambda (op-char) ; transform the output
       ;         (string->symbol (string op-char))))
       done))
;*****************************************************     NUMBERS    ****************************************************************

(define <digit-0-9>
  (range #\0 #\9) )

(define <digit-1-9>
  (range #\1 #\9))

(define <Natural>
  (new
    (*parser (char #\0))
    (*pack (lambda (_) 0))
    (*parser <digit-1-9>)
    (*parser <digit-0-9>) *star
    (*caten 2)
    (*pack-with
      (lambda (x xs)
        (string->number (list->string   `(,x ,@xs)))))
    (*disj 2)
    done))

;***********************************************************************************************************************************
(define <Integer>
  (new
    (*parser <op>)
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda (op natural)
        (if (char=? op #\-)
            (* natural -1)
            natural)))
    (*parser <Natural>)
    (*disj 2)
    done))
;***********************************************************************************************************************************
(define <Fraction>
  (new
    (*parser <Integer>)
    (*parser (char #\/))
    (*parser <Natural>)
    (*caten 3)
    (*pack-with (lambda (int divide natural)
                  (/ int natural)))
    done))
;***********************************************************************************************************************************
(define <OpHelper>
  (new
    (*parser (char #\/))
    (*parser (char #\*))
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 6)
    done))

;***********************************************************************************************************************************
(define <Number>
  (new
    (*parser <Fraction>)
    (*parser <Integer>)
    (*disj 2)
    (*delayed (lambda () <Symbol>))
    (*parser <OpHelper>)
    *diff
    *not-followed-by
    done))
;******************************************************************  CHAR   ***************************************************************
(define <CharPrefix>
  (new
    (*parser (char #\#))
    (*parser (char #\\))
    (*caten 2)
    done))
;***********************************************************************************************************************************
(define <VisibleSimpleChar>
  (new
    (*parser (range #\! #\~))
    (*parser  <Number>)
    *not-followed-by
    (*delayed (lambda () <NamedChar>))
    *not-followed-by
    done))
;***********************************************************************************************************************************
(define <NamedChar>
  (new
    (*parser (word-ci "lambda"))
    (*pack (lambda (namedChar) (integer->char 955)))
    (*parser (word-ci "newline"))
    (*pack (lambda (namedChar) #\newline))
    (*parser (word-ci "nul"))
    (*pack (lambda (namedChar) #\nul))
    (*parser (word-ci "page"))
    (*pack (lambda (namedChar) #\page))
    (*parser (word-ci "return"))
    (*pack (lambda (namedChar)  #\return))
    (*parser (word-ci "space"))
    (*pack (lambda (namedChar) #\space))
    (*parser (word-ci "tab"))
    (*pack (lambda (namedChar) #\tab))
    (*disj 7)
    done))
;***********************************************************************************************************************************


(define <HexChar>
  (new
    (*parser <digit-0-9>)
    (*parser (range #\a #\f))
    (*parser (range #\A #\F))
    (*pack (lambda (x)(integer->char (+ (char->integer x) 32))))
    (*disj 3)
    done))
;***********************************************************************************************************************************


(define <HexUnicodeChar>
  (new
    (*parser (char-ci #\x))
    (*parser (char #\X))
    (*pack (lambda (x)(integer->char (+ (char->integer x) 32))))
    (*disj 2)
    (*parser <HexChar>) *plus
    (*caten 2)
    (*pack-with (lambda (x hex)
                  (integer->char (string->number (list->string `(#\# #\x ,@hex))))))
    
    done))
;;***********************************************************************************************************************************

(define <Char>
  (new
    (*parser <CharPrefix> )
    (*parser <NamedChar> )
    (*parser <HexUnicodeChar> )
    (*parser <VisibleSimpleChar> )
    
    (*disj 3)
    (*caten 2)
    (*pack-with (lambda (w e) e))
    done))
;***********************************************************************************************************************************
(define <SymbolChar>
  (new
    (*parser (char #\^ ))
    (*parser (char #\* ))
    (*parser (char #\- ))
    (*parser (char #\_ ))
    (*parser (char #\= ))
    (*parser (char #\+ ))
    (*parser (char #\> ))
    (*parser (char #\< ))
    (*parser (char #\? ))
    (*parser (char #\! ))
    (*parser (char #\$ ))
    (*parser (char #\/ ))
    (*parser <digit-0-9>)
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack (lambda (x) (integer->char (+ (char->integer x) 32))))
    (*disj 15)
    done)
  )
;***********************************************************************************************************************************
(define <Symbol>
  (new 
    (*parser <SymbolChar>)
    *plus
    (*pack (lambda (x) (string->symbol (list->string `(,@x)))))
    (*delayed (lambda () <Vector>))
    *not-followed-by
    done)
  )

;***********************************************************************************************************************************


(define <StringLiteralChar>
  (new
    (*parser (range #\nul #\~))
    (*parser (char #\")) *diff
    (*parser (char #\\)) *diff
    done))

;***********************************************************************************************************************************

(define <StringMetaChar>
  (new
    (*parser (word "\\\\"))
    (*pack (lambda (x) #\\))
    (*parser (word "\\\""))
    (*pack (lambda (x) #\#))
    (*parser (word-ci "\\t"))
    (*pack (lambda (x) #\t))
    (*parser (word-ci "\\f"))
    (*pack (lambda (x) #\f))
    (*parser (word-ci "\\n"))
    (*pack (lambda (x) #\n))
    (*parser (word-ci "\\r"))
    (*pack (lambda (x) #\r))
    (*disj 6)
    done))


;***********************************************************************************************************************************

(define <StringHexChar>
  (new
    (*parser (char #\\ ))
    (*parser (char #\x ))
    (*parser <HexChar>) *star
    (*parser (char #\; ))
    (*caten 4)
    (*pack-with (lambda (y x hex z)
                  (integer->char (string->number (list->string `(#\# #\x ,@hex))))))
    done))
;***********************************************************************************************************************************
(define <StringChar>
  (new
    (*parser <StringLiteralChar>)
    (*parser <StringMetaChar>)
    (*parser <StringHexChar>)
    (*disj 3)
    done))
;***********************************************************************************************************************************

(define <String>
  (new
    (*parser (char #\" ))
    (*parser <StringChar>) *star
    (*parser (char #\" ))
    (*caten 3)
    (*pack-with (lambda (x str z) (list->string `(,@str))))
    ;(*pack-with (lambda (x str z) (list->string str)))
    done))

;***********************************************************************************************************************************

(define <ProperList>
  (new
    (*parser (char #\( ))
            
             (*parser <SexprSkip>) *star
             (*delayed (lambda () <sexpr>)) *star
             (*parser <SexprSkip>) *star
             (*parser (char #\) ))
    (*caten 5)
    (*pack-with (lambda (x sp1 str sp2 z) str))
    done))

;***********************************************************************************************************************************
(define <ImproperList>
  (new
    (*parser (char #\(  ))
             (*parser <SexprSkip>) *star
             (*delayed (lambda () <sexpr>)) *plus
             (*parser <SexprSkip>) *star
             (*parser (char #\. ))
             (*parser <SexprSkip>) *star
             (*delayed (lambda () <sexpr>))
             (*parser <SexprSkip>) *star
             (*parser (char #\)  ))
    (*caten 9)
    (*pack-with (lambda (x skip1 first skip2 y skip3 second skip4 z) `(,@first ,@second)))
    done))

;***********************************************************************************************************************************
(define <Vector>
  (new
    (*parser (char #\# ))
    (*parser <ProperList>)
    (*caten 2)
    (*pack-with (lambda (x str) (list->vector str)))
    done))

;***********************************************************************************************************************************

(define <Quoted>
  (new
    (*parser (char #\' ))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x str)  `(,'quote ,str)))
    done))

;***********************************************************************************************************************************

(define <QuasiQuoted>
  (new
    (*parser (char #\` ))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x str)  `(,'quasiquote ,str)))
    done))

;***********************************************************************************************************************************

(define <Unquoted>
  (new
    (*parser (char #\, ))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (x str)  `(,'unquote ,str)))
    done))

;***********************************************************************************************************************************

(define <UnquoteAndSpliced>
  (new
    (*parser (char #\, ))
    (*parser (char #\@ ))
    (*delayed (lambda () <sexpr>))
    (*caten 3)
    (*pack-with (lambda (x y str)  `(,'unquote-splicing  ,str)))
    done))

;***********************************************************************************************************************************
(define <CBNameSyntax1>
  (new
    (*parser (char #\@ ))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with (lambda (y str)  `(,'cbname   ,str)))
    done))

;***********************************************************************************************************************************

(define <CBNameSyntax2>
  (new
    (*parser (char #\{ ))
                      (*delayed (lambda () <sexpr>))
                      (*parser (char #\} ))
    (*caten 3)
    (*pack-with (lambda (y str x)  `(,'cbname   ,str)))
    done))


;***********************************************************************************************************************************
(define <CBName>
  (new
    (*parser <CBNameSyntax1>)
    (*parser <CBNameSyntax2>)
    (*disj 2)
    done))

;***********************************************************************************************************************************

;*********************************************************infix********************************************************
(define <InfixExtension>
  (new
    (*delayed (lambda () <InfixPrefixExtensionPrefix>))
    (*parser <InfixSkip>) *star
    (*delayed (lambda () <InfixExpression>))
    (*caten 3)
    (*pack-with (lambda (y skip infix)  infix))
    done))

;***********************************************************************************************************************************
(define <InfixPrefixExtensionPrefix>
  (new
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
    done))


;***********************************************************************************************************************************

(define <InfixSymbol>
  (new
    (*parser (char #\_ ))
    (*parser (char #\= ))
    (*parser (char #\> ))
    (*parser (char #\< ))
    (*parser (char #\? ))
    (*parser (char #\! ))
    (*parser (char #\$ ))
    (*parser <digit-0-9>)
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack (lambda (x) (integer->char (+ (char->integer x) 32))))
    (*disj 10)
    (*parser (word "**"))
    *diff
    *plus
    (*pack (lambda (x) (string->symbol (list->string `(,@x)))))
    done))
;***********************************************************************************************************************************
(define <InfixExpression>
  (new
    (*delayed (lambda () <InfixAddOrSub>))
    done))
;***********************************************************************************************************************************
(define <InfixParen>
  (new
    (*parser (char #\())
                 (*pack (lambda (op) (string->symbol (string op)))) 

             (*parser <InfixSkip>) *star
             (*delayed (lambda () <InfixExpression>))
             (*parser <InfixSkip>) *star
             
             (*parser (char #\)))
    (*caten 5)
    (*pack-with(lambda (par1 skip1 exp1 skip2 par2) exp1))
    done))
;***********************************************************************************************************************************
(define <InfixSexprEscape>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixSkip>) *star
    (*delayed (lambda () <sexpr>))
    (*caten 3)
    (*pack-with (lambda (x y z) z))
    done))
;;***********************************************************************************************************************************


(define <primaryExp>
  (new
    (*parser <Number>)
    (*parser <InfixSymbol>)
    (*parser <InfixParen>)
    (*parser <InfixSexprEscape>)
    (*disj 4)
    done))
;************************************************************************************************************************************
(define <PowerSymbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
    done))
;***********************************************************************************************************************************
(define <Neg>
  (new
    (*parser (char #\-))
    (*parser <InfixSkip>) *star
    (*parser <primaryExp>)
    (*caten 3)
    (*pack-with (lambda (w  skip arg2) `(- ,arg2)))
    (*parser <primaryExp>)
    (*disj 2)
    done))
;***********************************************************************************************************************************

(define <InfixArrayHelper>
  (lambda (array exp1)
    (list 'vector-ref array exp1))
  )

;***********************************************************************************************************************************


(define <InfixArrayGet>
  (new
    (*delayed (lambda () <InfixFuncall>))
    (*parser (char #\[))
                      (*parser <InfixSkip>) *star
                      (*delayed (lambda () <InfixExpression>))
                      (*parser <InfixSkip>) *star
                      (*parser (char #\]))
    (*caten 5)
    (*pack-with (lambda (par1 skip1 number skip2 par2)
                  number))
    *plus
    (*caten 2)
    (*pack-with (lambda (num1 num2)
                  `,(fold-left (lambda (number list)
                                 `(,@(<InfixArrayHelper> number list))) num1 num2)))
    (*delayed (lambda () <InfixFuncall>))
    (*disj 2)
    done))
;***********************************************************************************************************************************

(define <InfixArgList>
  (new
    (*parser <InfixExpression>)
    (*parser <InfixSkip>) *star
    (*parser (char #\,))  
    (*parser <InfixSkip>) *star
    (*parser <InfixExpression>)
    (*caten 3)
    (*pack-with (lambda (IE1 skip IE2) IE2))
    *star
    (*caten 3)
    (*pack-with (lambda (exp1 skip exp2)
                  `(,exp1 ,@ exp2)))
    (*parser <epsilon>)
    (*disj 2)
    done))
;***********************************************************************************************************************************
(define <InfixFuncall>
  (new
    (*parser <Neg>)
    (*parser (char #\())
                 (*pack (lambda (op) (string->symbol (string op)))) 
             (*parser <InfixSkip>) *star
             (*parser <InfixArgList>)
             (*parser <InfixSkip>) *star
             (*parser (char #\)))
        (*pack (lambda (op) (string->symbol (string op)))) 

    (*caten 6)             
    (*pack-with (lambda (exp1 par1 skip1 exp2 skip2 par2)
                  `(,exp1 ,@exp2)))
    (*parser <Neg>)
    (*disj 2)
    done))
;;;***********************************************************************************************************************************
(define <InfixPowHelper>
  (lambda(pow)
     'expt ))
;***********************************************************************************************************************************
(define <InfixPow>
  (new
    (*parser <InfixArrayGet>)
    (*parser <InfixSkip>) *star
    (*parser <PowerSymbol>)
    (*parser <InfixSkip>) *star
    (*delayed (lambda () <InfixPow>))
    ;(*parser <InfixArrayGet>)
    (*parser <InfixSkip>) *star
    (*caten 4)
    *plus
    (*caten 3)
    (*pack-with (lambda (num1 skip num2) (if (null? num2) num1
                                             `,(fold-left (lambda (number list)
                                                            `(,(<InfixPowHelper> (car list)) ,number ,(caddr list))) num1 num2))))
    (*parser <InfixArrayGet>)
    (*disj 2)
    
    
    done))
;;***********************************************************************************************************************************
;***********************************************************************************************************************************


(define <InfixMulOrDiv>
  (new
    
    (*parser <InfixPow>)
    (*parser <InfixSkip>) *star
    (*parser (char #\*))
    (*parser (char #\/))
    (*disj 2)
    (*pack (lambda (op) (string->symbol (string op)))) 

    (*parser <InfixSkip>) *star
    (*parser <InfixPow>)
    (*parser <InfixSkip>) *star
    (*caten 4)
    *plus
    (*caten 3)
    (*pack-with (lambda (num1 skip num2) (if (null? num2) num1
                                             `,(fold-left (lambda (number list)
                                                            `(,(car list) ,number ,(caddr list))) num1 num2))))
    (*parser <InfixPow>)
    (*disj 2)
    done))
;;***********************************************************************************************************************************
;;***********************************************************************************************************************************

(define <InfixAddOrSub>
  (new
    (*parser <InfixMulOrDiv>)
    (*parser <InfixSkip>) *star
    (*parser (char #\+))
    (*parser (char #\-))
    (*disj 2)
    (*pack (lambda (op) (string->symbol (string op)))) 
    (*parser <InfixSkip>) *star
    (*parser <InfixMulOrDiv>)
    (*parser <InfixSkip>) *star
    (*caten 4)
    *plus
    (*caten 3)
    (*pack-with (lambda (num1 skip num2) (if (null? num2) num1
                                             `,(fold-left (lambda (number list)
                                                            `(,(car list) ,number ,(caddr list))) num1 num2))))
    (*parser <InfixMulOrDiv>)
    (*disj 2)
    done))

