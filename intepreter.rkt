; Cade Lueker, Zora Li, Xiangyi 

(require "simpleParser.rkt")


; M-integer maps expressions to integer values
; (M-integer '(* (+ 4 3) (- 2 1))  => 7
; operators +, -, *, /, %
(define M-integer
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-operator)))))

; ABSTRACTION
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; state management function 
(define M_state
  (lambda (expresssion state)
    (cond
     ; make helper functions for each case
     ((eq? 'var (car expression))) ; declaration ---> Cade
     ((eq '= (car expression))) ; assignment ---> Cade
     ((eq 'return (car expression))) ; return 
     () ; while
     () ; if 
     ())))

(define M_if
  (lambda (expression state)
    (if(M_boolean (car expression) state)
       (M_state (cadr expression) state)
       (M_state (caddr expression) state))))


; helper functions 
(define M_int ; -----> Cade
  (lambda (expression) ; does this need state?
    ()))

(define M_name ; ----> Cade
  (lambda (variable)
    (cond
     () 
     )))

(define M_value
  (lambda (assignment state)
    (cond
     ()
     ())))

(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? condition #t) #t)
      ((eq? condition #f) #f)
      ((eq? '== (car condition)) (if(eq? (cadr condition) (caddr condition)) #t #f))
      ((eq? '!= (car condition)) (if(eq? (cadr condition) (caddr condition)) #f #t))
      ((eq? '< (car condition)) (if(< (cadr condition) (caddr condition)) #t #f))
      ((eq? '> (car condition)) (if(> (cadr condition) (caddr condition)) #t #f))
      ((eq? '<= (car condition)) (if(<= (cadr condition) (caddr condition)) #t #f))
      ((eq? '>= (car condition)) (if(>= (cadr condition) (caddr condition)) #t #f))
      ((eq? '&& (car condition)) (if(M_boolean (cadr condition) state) (if(M_boolean (caddr condition) state) #t #f) #f))
      ((eq? '|| (car condition)) (if(M_boolean (cadr condition) state) #t (if(M_boolean (caddr condition) state) #t #f)))
      ((eq? '! (car condition)) (if(M_boolean (cadr condition) state) #f #t))
       )))

(define add ; ---> Cade
  (lambda (variable value state)))

(define remove ; ---> Cade
  (lambda (variable state)))

; main function that handles the parsing of the test file
(define interpret
  (lambda (parse state)
    (cond
     ((null? (cdr parse)) '())
     ((null? (parse)) '())
     (else (interpret (cdr parse) (M_state (car parse)))))))
