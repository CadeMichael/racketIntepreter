; Cade Lueker, Zora Li, Xiangyi Zhang
#lang racket

(require "simpleParser.rkt")

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; state management function 
(define M_state
  (lambda (expression state)
    (cond
     ; make helper functions for each case
     ((eq? 'var (car expression)) (M_declaration expression state)) ; declaration ---> Cade
     ((eq? '= (car expression)) (M_assign expression state)) ; assignment ---> Cade
     ((eq? 'return (car expression)) (M_value (cadr expression) state)) ; return 
     ((eq? 'while (car expression)) (M_while expression state))  ; while
     ((eq? 'if (car expression)) (M_if expression state)))))

;(define M_return
;  (lambda (expression state)
;    (M_v))


;Abstraction for M_assign
(define assign_var cadr)
(define assign_val caddr)

(define M_assign
  (lambda (expression state)
    (if(number? (assign_var expression))
       (add (remove state (assign_var expression)) (assign_var expression) (assign_val expression))
       (add (remove state (assign_var expression)) (assign_var expression) (M_value (assign_val expression) state)))))

;abstractions for while
(define while_condition cadr)
(define while_body caddr)

(define M_while
  (lambda (expression state)
    (if(M_boolean (while_condition expression) state)
       (M_while expression (M_state (while_body expression) state))
       state)))

;abstractions for if
(define if_condition cadr)
(define if_body caddr)
(define if_else cadddr)

(define M_if
  (lambda (expression state)
    (if(M_boolean (if_condition expression) state)
       (M_state (if_body expression) state)
       (if(null? (if_else expression))
          state
          (M_state (if_else expression) state)))))


; helper functions 

; operators +, -, *, /, %
(define M-integer
  (lambda (expression s)
    (cond
      ((eq? (operator expression) '+) (+ (M_value (leftoperand expression) s) (M_value (rightoperand expression) s)))
      ((eq? (operator expression) '-) (- (M_value (leftoperand expression) s) (M_value (rightoperand expression) s)))
      ((eq? (operator expression) '*) (* (M_value (leftoperand expression) s) (M_value (rightoperand expression) s)))
      ((eq? (operator expression) '/) (quotient (M_value (leftoperand expression) s) (M_value (rightoperand expression) s)))
      ((eq? (operator expression) '%) (remainder (M_value (leftoperand expression) s) (M_value (rightoperand expression) s)))
      (else (error 'bad-operator)))))

; M_integer abstraction
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define get_var_value
  (lambda (var s)
    (cond
        ((null? (vars s)) (error "variable not assigned"))
        ((eq? (car (vars s)) var) (if (eq? (car (vals s))  '()) (error "var not assigned value") (car (vals s))))
        (else (get_var_value var (cons (cdr (vars s))(list (cdr (vals s)))))))))

(define M_value
  (lambda (expression s)
    (cond 
     ((null? expression) '())
     ((number? expression) expression)
     ((eq? expression 'true) 'true)
     ((eq? expression 'false) 'false)
     ((atom? expression) (get_var_value expression s))
     ((eq? '== (operator expression)) (M_boolean expression s))
     ((eq? '!= (operator expression)) (M_boolean expression s))
     ((eq? '! (operator expression)) (M_boolean expression s))
     ((eq? '< (operator expression)) (M_boolean expression s))
     ((eq? '> (operator expression)) (M_boolean expression s))
     ((eq? '>= (operator expression)) (M_boolean expression s))
     ((eq? '<= (operator expression)) (M_boolean expression s))
     ((eq? '|| (operator expression)) (M_boolean expression s))
     ((eq? '&& (operator expression)) (M_boolean expression s))
     (else (M-integer expression s)))))

(define M_declaration
  (lambda (expression s)
    (cond
      ((eq? (declr-assign expression) null) (add (remove s (declr-var expression)) (declr-var expression) '()))
      (else (add 
             (remove s (declr-var expression)) (declr-var expression) (M_value (car (declr-assign expression)) s))))))

; abstractions for declaration
(define declr-var cadr)
(define declr-assign cddr)

(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? condition 'true) 'true)
      ((eq? condition 'false) 'false)
      ((eq? '== (operator condition)) (if(eq? (M_value (leftcond condition) state) (M_value (rightcond condition) state)) 'true 'false))
      ((eq? '!= (operator condition)) (if(eq? (M_value (leftcond condition) state) (M_value (rightcond condition) state)) 'false 'true))
      ((eq? '< (operator condition)) (if(< (M_value(leftcond condition) state) (M_value (rightcond condition) state)) 'true 'false))
      ((eq? '> (operator condition)) (if(> (M_value (leftcond condition) state) (M_value (rightcond condition) state)) 'true 'false))
      ((eq? '<= (operator condition)) (if(<= (M_value (leftcond condition) state) (M_value (rightcond condition) state)) 'true 'false))
      ((eq? '>= (operator condition)) (if(>= (cadr condition) (caddr condition)) 'true 'false))
      ((eq? '&& (operator condition))(if(eq? 'true (M_value (leftcond condition) state)) (if(eq? 'true (M_value (rightcond condition) state)) 'true 'false) 'false))
      ((eq? '|| (operator condition))(if(eq? 'true (M_value (leftcond condition) state)) 'true (if(eq? 'true (M_value (rightcond condition) state)) 'true 'false)))
      ((eq? '! (operator condition)) (if(eq? 'true (M_value (leftcond condition) state)) 'false 'true))
       )))

; boolean abstractions
(define leftcond cadr)
(define rightcond caddr)

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

; main function that handles the parsing of the test file
(define interpret
  (lambda (parse s)
    (cond
     ((null? parse) s)
     (else (interpret (cdr parse) (M_state (car parse) s))))))

(interpret (parser "testFile.txt") '(()()))