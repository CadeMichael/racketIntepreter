; Cade Lueker, Zora Li, Xiangyi Zhang

(require "simpleParser.rkt")

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; state management function 
(define M_state
  (lambda (expresssion state)
    (cond
     ; make helper functions for each case
     ((eq? 'var (car expression)) (M_declaration expression state)) ; declaration ---> Cade
     ((eq? '= (car expression)) (M_assign expression state)) ; assignment ---> Cade
     ((eq? 'return (car expression)) (M_value (cdr expression) state)) ; return 
     ((eq? 'while (car expression)) (M_while (cdr expression) state))  ; while
     ((eq? 'if (car expression)) (M_if (cdr expression) state)) ; if 
     ())))

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



(define M_while
  (lambda (expression state)
    (if(M_boolean (car expression) state)
       (M_while expression (M_state (cdr expression) state))
       (state))))

(define M_if
  (lambda (expression state)
    (if(M_boolean (car expression) state)
       (M_state (cadr expression) state)
       (M_state (caddr expression) state))))


; helper functions 

; operators +, -, *, /, %
(define M-integer
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((atom? expression) (get_var_value expression s))
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression) s) (M-integer (rightoperand expression) s)))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression) s) (M-integer (rightoperand expression) s)))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression) s) (M-integer (rightoperand expression) s)))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression) s) (M-integer (rightoperand expression) s)))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression) s) (M-integer (rightoperand expression) s)))
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
  (lambda (parse state)
    (cond
     ((null? (cdr parse)) '())
     ((null? (parse)) '())
     (else (interpret (cdr parse) (M_state (car parse)))))))
