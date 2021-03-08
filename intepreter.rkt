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
     ((eq 'var (car expression))) ; declaration ---> Cade
     ((eq '= (car expression))) ; assignment ---> Cade
     ((eq 'return (car expression))) ; return 
     () ; while
     () ; if 
     ())))
  
; ------------ helper functions --------------

;state abstractions 
(define vars car)
(define vals cadr)

; add with list of two lists 
(define add 
  (lambda (s variable value)
    (cons (cons variable (vars s)) (list (cons value (vals s))))))

; remove with list of two lists
(define remove 
  (lambda (s variable)
    (cond 
     ((null? (vars s)) '(()()))
     ((eq? (car (vars s)) variable) (cons (cdr (vars s))(list (cdr (vals s)))))
     (else (cons 
            (cons (car (vars s))(vars (remove (cons (cdr (vars s))(list (cdr (vals s)))) variable)))
            (list (cons (car (vals s))(vals (remove (cons (cdr (vars s))(list (cdr (vals s)))) variable)))))))))

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
    (cond)))

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
