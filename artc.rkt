#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts a scheme-expression into a string
;; INPUT: a scheme-expression EXP
;; OUTPUT: a SCHEME String corresponding to EXP
(define (exp->string exp)
  (cond ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((list? exp) (exp->string (car exp)))))

;; INPUT: a list of lists
;; OUTPUT: a list containing all elements of the first-level lists
(define (flatten list-of-lists)
  (cond ((null? list-of-lists) '())
        (else (append (car list-of-lists) (flatten (cdr list-of-lists))))))

;; this is for all error handling.
;; programmers don't use this function but
;; the interpreter calls this function to
;; signal some type of programmer error
(define (error msg)
  (display "ERROR: ")
  (display msg)
  (newline))

;; THERE ARE TWO SUPPORTED TYPES: 'int and 'boolean
;; INPUT: an element of the ART-C language
;; OUTPUT: the type of that element
(define (type-of val)
  (cond ((number? val) 'int)
        ((boolean? val) 'boolean)
        ((bool? val) 'boolean)))

;; A MAP is a list of key-value pairs
;; INPUT: a MAP and a KEY
;; OUTPUT: The value associated with the key or 'error
(define (map-get map x)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) x) (cadr (car map)))
        (else (map-get (cdr map) x))))

;; INPUT : A MAP AND KEY
;; OUTPUT : true if the key is in the map and false otherwise
(define (map-contains map x)
  (cond ((null? map) #f)
        ((equal? (car (car map)) x) #t)
        (else (map-contains (cdr map) x))))

;; INPUT : A MAP, KEY and VALUE
;; OUTPUT: The map that results from replacing the key with the new value.  If
;; the map doesn't contain KEY, then 'error is returned
(define (map-replace map key val)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) key)
         (cons (list key val) (cdr map)))
        (else
         (cons (car map) (map-replace (cdr map) key val)))))

;; INPUT : A MAP, Key and Value
;; OUTPUT : The map that results from adding a key-value pair.  This
;; allows for duplicate keys (the most-recently added is nearer the front of the list
(define (map-add map key val)
  (cons (list key val) map))

;; INPUT: A MAP and KEY
;; OUTPUT: The map that results from deleting the key.  No errors occur if the map
;; doesn't contain the key
(define (map-delete map key)
  (cond ((null? map) map)
        ((equal? (car (car map)) key) (cdr map))
        (else (cons (car map)
                    (map-delete (cdr map) key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPEMAP : A SEMANTIC DOMAIN DATA TYPE
;; A typemap is a list of block-level declarations.
;; FORM: ((var1 type1) (var2 type2) (var3 type3) ... )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: NONE
;; OUTPUT: AN empty typemap
(define (typemap-create-empty) '())

;; INPUT: A TYPEMAP
;; OUTPUT: The type of variable x
(define (typemap-type-of tm x)
  (map-get tm x))

;; INPUT: A TYPEMAP
;; OUTPUT: THE TYPEMAP THAT RESULTS FROM INSERTING A DECLARATIONS
(define (typemap-add tm decl)
  (map-add tm (cadr decl) (car decl)))

(define (typemap-delete tm key)
  (map-delete tm key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE : A SEMANTIC DOMAIN DATA TYPE
;; A LIST OF (VAR, VALUE) pairs
;; FORM :  ( (var1 val1) (var2 val2) ... )
;; NOTE: A map can contain duplicate keys but innermost KEYS occur
;;       before outermost KEYS and hide them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INPUT : NONE
;; OUTPUT: AN EMPTY STATE
(define (state-create-empty) '())

;; INPUT: STATE and ID
;; OUTPUT: a new state such that the innermost scope now contains a
;;         new binding for the specified ID.  The bindings value is 'undefined.
(define (state-add state id)
  (map-add state id 'undefined))

;; INPUT : STATE and ID
;; OUTPUT: A new state such that the innermost id is removed
(define (state-delete state id)
  (map-delete state id))

;; INPUT: STATE and ID
;; OUTPUT: The value associated with the specified ID in the given state
(define (state-get-value state id)
  (map-get state id))

;; INPUT: STATE and ID
;; OUTPUT: A new state that results from changing the mapping from id->value in
;;         the specified state
(define (state-update state id value)
  (map-replace state id value))

;; INPUT: STATE and LIST-OF-IDS (VARIABLES)
;; OUTPUT: A new state that results from deleting all ids (the variables) from
;;         the specified state
(define (state-delete-all state variables)
  (cond ((null? variables) state)
        (else (state-delete-all (state-delete state (car variables)) (cdr variables)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE CLASSES CORRESPOND TO THE ABSTRACT SYNTAX SUCH THAT A "PROGRAM"
;; REPRESENTS A PARSE-TREE.  THESE FUNCTIONS OPERATE AT THE 'SYNTACTIC' LEVEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (PROGRAM BODY)
(define (program-get-body stmt)
  (cadr stmt))

;; (BLOCK S1...SN)
(define (block-get-body stmt)
  (filter (lambda (x) (not (is-declaration? x))) (cdr stmt)))

(define (block-get-declarations stmt)
  (filter (lambda (x) (is-declaration? x)) (cdr stmt)))

;; (DECLARE TYPE VAR)
(define (declaration-get-type stmt)
  (cadr stmt))

(define (declaration-get-var stmt)
  (caddr stmt))

(define (is-declaration? stmt)
  (and (list? stmt) 
       (equal? (car stmt) 'declare)))

;; (:= VAR EXP)
(define (assignment-get-var stmt)
  (cadr stmt))

(define (assignment-get-exp stmt)
  (caddr stmt))

;; (IF TEST THEN [ELSE])
(define (if-get-test stmt)
  (cadr stmt))

(define (if-get-then stmt)
  (caddr stmt))

(define (if-has-else? stmt)
  (= (length stmt) 4))

(define (if-get-else stmt)
  (cadddr stmt))

;; (WHILE TEST BODY)
(define (while-get-test stmt)
  (cadr stmt))

(define (while-get-body stmt)
  (caddr stmt))

;; (SPRINT LABEL EXP)
(define (sprint-has-exp? stmt)
  (and (list? stmt)
       (= (length stmt) 3)))

(define (sprint-get-label? stmt)
  (cadr stmt))

(define (sprint-get-exp stmt)
  (caddr stmt))

;; INPUT: an expression EXP
;; OUTPUT: the operator of EXP (an element of ART-C)
(define (exp-get-operator exp)
  (car exp))

;; INPUT: an expression EXP
;; OUTPUT: the left-operand (an expression) of EXP
(define (exp-get-left-operand exp)
  (car (cdr exp)))

;; INPUT: an expression EXP
;; OUTPUT: the exp-get-right-operand (an expression) of EXP
(define (exp-get-right-operand exp)
  (car (cdr (cdr exp))))

;; INPUT: an expression EXP
;; OUTPUT: #t if the expression is a boolean literal and #f otherwise
(define (bool? exp)
  (or (equal? exp 'true)
      (equal? exp 'false)))

;; INPUT: a symbol
;; OUTPUT: #t if the symbol is 'true and #f if it is 'false and 'void' if neither
(define (symbol->bool sym)
  (cond ((equal? sym 'true) #t)
        ((equal? sym 'false) #f)))

(define (bool->symbol bool)
  (cond (bool 'true)
        (else 'false)))


;; INPUT: A PROGRAM
;; A PROGRAM has syntactic structure (program stmt)
;; OUTPUT: THE STATE that results from executing the program
;;         in an empty state.
#|(define (interpret-program pgm)
  (interpret (program-get-body pgm) (state-create-empty)))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the INTERPRETER class
;; An INTERPRETER is simply a collection of functions that
;; operates on TYPES, STATES, BINDING, SCOPES and PROGRAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




#|----------------------------------------------------------------------------------------------|
 |                            Start of code writted by David Klein                              |
 |----------------------------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------------------------|
 |                          Start of functions for "is-program-valid?                           |
 |----------------------------------------------------------------------------------------------|#

#| VALID PROGRAM? |#
(define (is-program-valid? pgm)
  (valid-block? (cdadr pgm) (typemap-create-empty)))


#| VALID BLOCK? |#
(define (valid-block? block tm)
  (cond ((null? block) #t)
        (else (let ((kind (caar block)))
                (cond ((equal? kind 'declare) (and (valid-declaration? (car block) tm)
                                                   (valid-block? (cdr block) (typemap-add tm (cdar block)))))
                      ((equal? kind ':=) (and (valid-assignment? (car block) tm)
                                              (valid-block? (cdr block) tm)))
                      ((equal? kind 'if) (and (valid-if? (car block) tm)
                                              (valid-block? (cdr block) tm)))
                      ((equal? kind 'sprint) (and (valid-sprint? (car block) tm)
                                                  (valid-block? (cdr block) tm)))
                      ((equal? kind 'while) (and (valid-while? (car block) tm)
                                                 (valid-block? (cdr block) tm)))
                      ((equal? kind 'block) (and (valid-block? (cdar block) tm)
                                                 (valid-block? (cdr block) tm)))                      
                      (else #f))))))

#| VALID DECLARATION? |#
(define (valid-declaration? dclr tm)
  (cond ((map-contains tm (caddr dclr)) #f)
        ((not (is-alpha (string-upcase (substring (symbol->string (caddr dclr)) 0 1)))) #f)
        (else
         (not
          (or
           (equal? (caddr dclr) 'true)
           (equal? (caddr dclr) 'false)
           (equal? (caddr dclr) 'while)
           (equal? (caddr dclr) 'int)
           (equal? (caddr dclr) 'boolean)
           (equal? (caddr dclr) 'if)
           (equal? (caddr dclr) 'sprint)
           (equal? (caddr dclr) 'declare)
           (equal? (caddr dclr) 'block))))))

#| IS ALPHA |#
(define (is-alpha str)
  (or
   (string=? str "A")
   (string=? str "B")
   (string=? str "C")
   (string=? str "D")
   (string=? str "E")
   (string=? str "F")
   (string=? str "G")
   (string=? str "H")
   (string=? str "I")
   (string=? str "J")
   (string=? str "K")
   (string=? str "L")
   (string=? str "M")
   (string=? str "N")
   (string=? str "O")
   (string=? str "P")
   (string=? str "Q")
   (string=? str "R")
   (string=? str "S")
   (string=? str "T")
   (string=? str "U")
   (string=? str "V")
   (string=? str "W")
   (string=? str "X")
   (string=? str "Y")
   (string=? str "Z")))

#| VALID ASSIGNMENT? |#
(define (valid-assignment? stmt tm)
  (and
   (map-contains tm (assignment-get-var stmt))
   (valid-expression? (assignment-get-exp stmt) tm)
   (equal? (typemap-type-of tm (assignment-get-var stmt))
           (get-type-of (caddr stmt) tm))))

#| GET TYPE OF |#
(define (get-type-of exp tm)
  (cond ((boolean-exp? exp tm) 'boolean)
        ((integer-exp? exp tm) 'int)))

#| VALID EXPRESSION? |#
(define (valid-expression? tm exp)
  (cond ((or (boolean-exp? tm exp)
             (integer-exp? tm exp)) #t)
        (else #f)))



#| VALID INTEGER? |#
(define (integer? op)
  (and (real? op)
       (and
        (>= op -2147483648)
        (<= op 2147483647))))

#| VALID IF? |#
(define (valid-if? exp tm)
  (or
   (and;;;Else statement
    (= (length exp) 4)
    (boolean-exp? (cadr exp) tm)
    (valid-statement? (caddr exp) tm)
    (valid-statement? (cadddr exp) tm))
   (and;;;No else statement
    (= (length exp) 3)
    (boolean-exp? (cadr exp) tm)
    (valid-statement? (caddr exp) tm))))

#| BOOLEAN-EXP? |#
(define (boolean-exp? exp tm)
  (cond ((and;;;Arity = 2 and Operators are integer
          (list? exp)
          (= (length exp) 3)
          (or
           (equal? (car exp) '<)
           (equal? (car exp) '>)
           (equal? (car exp) '=)
           (equal? (car exp) '<=)
           (equal? (car exp) '>=))
          (integer-exp? (cadr exp) tm)
          (integer-exp? (caddr exp) tm)) #t)
        ((and;;;Arity = 2 and Operators are boolean
          (list? exp)
          (= (length exp) 3)
          (or
           (equal? (car exp) '&)
           (equal? (car exp) '%))
          (boolean-exp? (cadr exp) tm)
          (boolean-exp? (caddr exp) tm)) #t)
        ((and;;;Arity = 1 and Operator is boolean
          (list? exp)
          (= (length exp) 2)
          (equal? (car exp) '~)
          (boolean-exp? (cadr exp) tm)) #t)
        ((and;;;Not a list, single element boolean. Variable or Literal
          (not (list? exp))
          (or
           (equal? exp 'true)
           (equal? exp 'false)
           (and;;;Variable?
            (map-contains tm exp)
            (equal? (typemap-type-of tm exp) 'boolean)))) #t)
        (else #f)))

#| INTEGER EXP? |#
(define (integer-exp? exp tm)
  (cond ((and;;;Arity = 2 and Operators are integer
          (list? exp)
          (or
           (equal? (car exp) '+)
           (equal? (car exp) '-)
           (equal? (car exp) '*)
           (equal? (car exp) '/)
           (equal? (car exp) '@)
           (equal? (car exp) '?))
          (integer-exp? (cadr exp) tm)
          (integer-exp? (caddr exp) tm)) #t)
        ((and;;;Not a list, single element int. Variable or Literal
          (not (list? exp))
          (or
           (integer? exp)
           (and;;;Variable?
            (map-contains tm exp)
            (equal? (typemap-type-of tm exp) 'int)))) #t)
        (else #f)))

#| VALID STATEMENT? |#
(define (valid-statement? stmt tm)
  (let ((kind (car stmt)))
    (cond ((equal? kind 'block)
           (valid-block? (cdr stmt) tm))
          ((equal? kind ':=)
           (valid-assignment? stmt tm))
          ((equal? kind 'if)
           (valid-if? (cdr stmt) tm))
          ((equal? kind 'sprint)
           (valid-sprint? stmt tm))
          ((equal? kind 'while)
           (valid-while? stmt tm))
          (else #f))))

#| VALID SPRINT? |#
(define (valid-sprint? spr tm)
  (or
   (and
    (= (length spr) 3)
    (string? (cadr spr))
    (valid-expression? (caddr spr) tm))
   (and
    (= (length spr) 2)
    (string? (cadr spr)))))

#| VALID WHILE? |#
(define (valid-while? whl tm)
  (and (boolean-exp? (cadr whl) tm)
       (valid-statement? (caddr whl) tm)))
#|----------------------------------------------------------------------------------------------|
 |                              Start of code for State "Class"                                 |
 |----------------------------------------------------------------------------------------------|#
#|
   The code define below if very similar to the Map and State "Classes" predefined, but they
   do not handle scope. The scope variable is simply an int that that will increment the deeper into
   blocks the pgm delves. For the main block scope = 0 and it will increment by 1 for every nested block.
   This allows the interpretter to simply remove the highest scope variables after completing a nested
   block.
|#
#|
   NEW STATE
   Input :
   Output: '()
|#
(define (new-state)
  '())

#|
   STATE INSERT
   Input : A state, var, val, scope
   Output: (cons '(var, val, scope) state)
|#
(define (state-insert state var scope)
  (cons (list var 'undefined scope) state))

#|
   STATE CONTAINS
   Input : A state, var
   Output: #t if var is in state
           #f if var isn't in state
|#
(define (state-contains state var)
  (cond ((null? state) #f)
        ((equal? (car (car state)) var) #t)
        (else (state-contains (cdr state) var))))

#|
   STATE REPLACE
   Input : A state, var, val
   Output: State, but the previous val
           associated with var is replaced
           with val
|#
(define (state-replace state var val)
  (cond ((null? state) 'error)
        ((equal? (car (car state)) var)
         (cons (list var val (caddr(car state))) (cdr state)))
        (else
         (cons (car state) (state-replace (cdr state) var val)))))

#|
   REMOVE HIGHEST SCOPE
   Input : A state
   Output: The state with the highest scope variables removed
|#
(define (remove-highest-scope state)
  (remove-scope state (large (map caddr state))))

#|
   GET HIGHEST SCOPE
   Input : A state
   Output: The highest scope value
|#
(define (large state)
     (if (null? (cdr state)) 
         (car state) 
         (if (< (car state) (large (cdr state)))  
             (large (cdr state)) 
             (car state))))

(define (get-highest-scope state)
  (cond ((null? state) '(0))
        (else (large (map caddr state)))))

#|
   REMOVE SCOPE
   Input : A state, scope
   Output: The state with all vars
           where the scope doesn't
           equal scope
|#
(define (remove-scope state scope)
  (cond ((null? state) '())
        ((= (caddar state) scope)
         (remove-scope (cdr state) scope))
        (else
         (cons (car state) (remove-scope (cdr state) scope)))))

#|
   REMOVE VAR
   Input : A state, var
   Output: The state after removing
           the element that equals?
           var
|#
(define (remove-var state var)
  (cond ((null? state) 'error)
        ((equal? (caar state) var)
         (cdr state))
        (else
         (cons (car state) (remove-var (cdr state) var)))))

#|
   TYPE OF VAR
   Input : A var
   Output: The type of var
|#
(define (type-of-var state var)
  (type-of (state-get-value state var)))

#|----------------------------------------------------------------------------------------------|
 |                               Start of code for "interpret"                                  |
 |----------------------------------------------------------------------------------------------|#

#| This is the INTERPRETER class
   An INTERPRETER is simply a collection of functions that
   operates on TYPES, STATES, BINDING, SCOPES and PROGRAMS
|#

(define (interpret-program pgm)
  (interpret (cadr pgm) '()))

#|(define pgm '(program 
              (block
               (declare int n)
               (declare boolean error)
               (declare int result)   
               (:= error false)
               (:= result 1)
               (block 
                (declare int local)
                (:= n 5)
                (:= local n)
                (while (> local 0)
                       (block
                        (:= result (* result local))
                        (:= local (- local 1)))))
              (sprint "result: " result)
              (if (! error) (sprint "a") (sprint "b")))))

(interpret-program pgm)|#
#|
   INTERPRET
   Input : A statement, state
   Output: The state that results from
           executing STATEMENT in STATE
|#
(define (interpret stmt state)
  (display stmt) (newline) (display state) (newline)
  (let ((kind (car stmt)))
    (cond ((equal? kind 'block) (interpret-block (cdr stmt) state 0))
          ((equal? kind 'declare) (interpret-declaration stmt state))
          ((equal? kind ':=) (interpret-assignment stmt state))
          ((equal? kind 'if) (interpret-if stmt state))
          ((equal? kind 'sprint) (interpret-sprint stmt state))
          ((equal? kind 'while) (interpret-while stmt state))       
          (else (error (string-append "statement expected but saw (" (exp->string stmt) "...) instead."))))))

#|----------------------------------------------------------------------------------------------|
 |                           Start of code for "Interpret Block"                                |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET BLOCK
   Input : A block, state
   Output: The state that results from
           executing block in state
|#
(define (interpret-block block state scope)
  (display state) (newline)
  (cond
    ((and
      (null? block)
      (= scope 0)) state)
    ((null? block) (remove-scope state scope))
    (else (let ((kind (caar block)))
            (cond ((equal? kind 'declare)
                   (interpret-block (cdr block) (interpret-declaration (car block) state  scope) scope))
                  ((equal? kind ':=)
                   (interpret-block (cdr block) (interpret-assignment (car block) state) scope))
                  ((equal? kind 'if)
                   (interpret-block (cdr block) (interpret-if (car block) state) scope))
                  ((equal? kind 'sprint)
                   (interpret-sprint (car block) state) (interpret-block (cdr block) state scope))
                  ((equal? kind 'while)
                   (interpret-block (cdr block) (interpret-while (car block) state) scope))
                  ((equal? kind 'block)
                   (interpret-block (cdr block) (interpret-block (cdar block) state (+ (get-highest-scope state) 1)) scope)))))))

#|----------------------------------------------------------------------------------------------|
 |                        Start of code for "Interpret Declaration"                             |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET DECLARATION
   Input : A declaration, state
   Output: The state that results from
           executing declaration in state
|#
(define (interpret-declaration declaration state scope)
  (cond ((valid-declaration-state? declaration state)
         (state-insert state
                       (caddr declaration)
                       scope))
        (else (error "Invalid Declaration"))))

#| VALID DECLARATION STATE |#
(define (valid-declaration-state? dclr state)
  (cond ((state-contains state (caddr dclr)) #f)
        ((not (is-alpha (string-upcase (substring (symbol->string (caddr dclr)) 0 1)))) #f)
        (else
         (not
          (or
           (equal? (caddr dclr) 'true)
           (equal? (caddr dclr) 'false)
           (equal? (caddr dclr) 'while)
           (equal? (caddr dclr) 'int)
           (equal? (caddr dclr) 'boolean)
           (equal? (caddr dclr) 'if)
           (equal? (caddr dclr) 'sprint)
           (equal? (caddr dclr) 'declare)
           (equal? (caddr dclr) 'block))))))

#| IS ALPHA |#
(define (is-alpha-state? str)
  (or
   (string=? str "A")
   (string=? str "B")
   (string=? str "C")
   (string=? str "D")
   (string=? str "E")
   (string=? str "F")
   (string=? str "G")
   (string=? str "H")
   (string=? str "I")
   (string=? str "J")
   (string=? str "K")
   (string=? str "L")
   (string=? str "M")
   (string=? str "N")
   (string=? str "O")
   (string=? str "P")
   (string=? str "Q")
   (string=? str "R")
   (string=? str "S")
   (string=? str "T")
   (string=? str "U")
   (string=? str "V")
   (string=? str "W")
   (string=? str "X")
   (string=? str "Y")
   (string=? str "Z")))

#|----------------------------------------------------------------------------------------------|
 |                         Start of code for "Interpret Assignment"                             |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET ASSIGNMENT
   Input : A assignment, state
   Output: The state that results from
           executing assignment in state
|#
(define (interpret-assignment assignment state)
  (cond ((valid-assignment-state? assignment state)
         (state-replace state (cadr assignment) (evaluate (caddr assignment) state)))
        (else (error "Invalid Assignment"))))
#|
   EVALUATE
   Input : A expression and state
   Output: The value resulting from
           evaluating the expression
|#
(define (evaluate expr state)
  (cond ((boolean-expression? expr)
         (bool->symbol (boolean-evaluate expr state)))
        ((integer-expression? expr)
         (integer-evaluate expr state))
        ((and (not (list? expr))
              (state-contains state expr))
         (state-get-value state expr))))
#|
   EVALUATE BOOLEAN
   Input : A expression
   Output: The boolean resulting from
           evaluating the expression
|#
(define (boolean-evaluate expr state)
  (cond ((not (list? expr))
         (cond ((state-contains state expr)
                (to-boolean (state-get-value state expr)))
               (else (to-boolean expr))))
        (else
         (cond ((equal? (car expr) '<)
                (< 
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '>)
                (>
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '=)
                (=
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '<=)
                (<=
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '>=)
                (>=
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '&)
                (&
                 (boolean-evaluate (cadr expr) state)
                 (boolean-evaluate (caddr expr) state)))
               ((equal? (car expr) '%)
                (%
                 (boolean-evaluate (cadr expr) state)
                 (boolean-evaluate (caddr expr) state)))
               ((equal? (car expr) '~)
                (~
                 (boolean-evaluate (cadr expr) state)))))))

#|
   EVALUATE INTEGER
   Input : A expression
   Output: The integer resulting from
           evaluating the expression
|#
(define (integer-evaluate expr state)
  (cond ((not (list? expr))
         (cond ((state-contains state expr)
                (state-get-value state expr))
               (else expr)))
        ((= (length expr) 3)
         (cond ((equal? (car expr) '+)
                (+
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '-)
                (-
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '*)
                (*
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '/)
                (/
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '@)
                (@
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               ((equal? (car expr) '?)
                (?
                 (integer-evaluate (cadr expr) state)
                 (integer-evaluate (caddr expr) state)))
               (else (error "Invalid binary arithmetic expression"))))
        ((= (length expr) 2)
         (cond ((equal? (car expr) '-)
                (-
                 (integer-evaluate (cadr expr))))
               (else (error "Invalid unary arithmetic expression"))))
        (else ("Invalid arithmetic expression"))))

#| TO BOOLEAN |#
(define (to-boolean sym)
  (cond ((equal? sym 'true) #t)
        ((equal? sym 'false) #f)))

#| BOOLEAN EXPRESSION |#
(define (boolean-expression? op)
  (or
   (and
    (not (list? op))
    (or
     (equal? op 'true)
     (equal? op 'false)))
   (and
    (list? op)
    (or
     (equal? (car op) '<)
     (equal? (car op) '>)
     (equal? (car op) '=)
     (equal? (car op) '<=)
     (equal? (car op) '>=)
     (equal? (car op) '&)
     (equal? (car op) '%)
     (equal? (car op) '~)))))

#| INTEGER EXPRESSION |#
(define (integer-expression? op)
  (or
   (and
    (not (list? op))
    (integer? op))
   (and
    (list? op)
    (or
     (equal? (car op) '+)
     (equal? (car op) '-)
     (equal? (car op) '*)
     (equal? (car op) '/)
     (equal? (car op) '@)
     (equal? (car op) '?)))))


#| VALID ASSIGNMENT? |#
(define (valid-assignment-state? stmt state)
  (or
   (and
    (state-contains state (assignment-get-var stmt))
    (valid-expression-state? (assignment-get-exp stmt) state)
    (equal? (type-of-var state (assignment-get-var stmt))
            (get-type-of-state (caddr stmt) state)))
   (and
    (state-contains state (assignment-get-var stmt))
    (valid-expression-state? (assignment-get-exp stmt) state)
    (equal? (state-get-value state (cadr stmt)) 'undefined))
   ))

#| GET TYPE OF |#
(define (get-type-of-state exp state)
  (cond ((boolean-exp-state? exp state) 'boolean)
        ((integer-exp-state? exp state) 'int)))

#| VALID EXPRESSION? |#
(define (valid-expression-state? state exp)
  (cond ((or (boolean-exp-state? state exp)
             (integer-exp-state? state exp)) #t)
        (else #f)))


#| BOOLEAN-EXP? |#
(define (boolean-exp-state? exp state)
  (cond ((and;;;Arity = 2 and Operators are integer
          (list? exp)
          (= (length exp) 3)
          (or
           (equal? (car exp) '<)
           (equal? (car exp) '>)
           (equal? (car exp) '=)
           (equal? (car exp) '<=)
           (equal? (car exp) '>=))
          (integer-exp-state? (cadr exp) state)
          (integer-exp-state? (caddr exp) state)) #t)
        ((and;;;Arity = 2 and Operators are boolean
          (list? exp)
          (= (length exp) 3)
          (or
           (equal? (car exp) '&)
           (equal? (car exp) '%))
          (boolean-exp-state? (cadr exp) state)
          (boolean-exp-state? (caddr exp) state)) #t)
        ((and;;;Arity = 1 and Operator is boolean
          (list? exp)
          (= (length exp) 2)
          (equal? (car exp) '~)
          (boolean-exp-state? (cadr exp) state)) #t)
        ((and;;;Not a list, single element boolean. Variable or Literal
          (not (list? exp))
          (or
           (equal? exp 'true)
           (equal? exp 'false)
           (and;;;Variable?
            (map-contains state exp)
            (equal? (type-of-var state exp) 'boolean)))) #t)
        (else #f)))

#| INTEGER EXP? |#
(define (integer-exp-state? exp state)
  (cond ((and;;;Arity = 2 and Operators are integer
          (list? exp)
          (or
           (equal? (car exp) '+)
           (equal? (car exp) '-)
           (equal? (car exp) '*)
           (equal? (car exp) '/)
           (equal? (car exp) '@)
           (equal? (car exp) '?))
          (integer-exp-state? (cadr exp) state)
          (integer-exp-state? (caddr exp) state)) #t)
        ((and;;;Not a list, single element int. Variable or Literal
          (not (list? exp))
          (or
           (integer? exp)
           (and;;;Variable?
            (map-contains state exp)
            (equal? (type-of-var state exp) 'int)))) #t)
        (else #f)))

#|----------------------------------------------------------------------------------------------|
 |                            Start of code for interpret "if"                                  |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET IF
   Input : A if, state
   Output: The state that results from
           executing if in state
|#
(define (interpret-if if state)
  (cond ((boolean-exp-state? (cadr if) state)
         (cond ((boolean-evaluate (cadr if) state)
                (interpret-statement (caddr if) state))
               ((= (length if) 4)
                (interpret-statement (cadddr if) state))
               (else state)))
        (else (error "Invalid Boolean"))))

         


#|
   INTERPRET STATEMENT
   Input : A statement, state
   Output: The state that results from
           executing a statement
           in the given state
|#
(define (interpret-statement stmt state)
  (cond
    ((null? stmt) state)
    (else (let ((kind (car stmt)))
            (cond ((equal? kind 'declare)
                   (interpret-statement (cdr stmt) (interpret-declaration stmt state)))
                  ((equal? kind ':=)
                   (interpret-statement (cdr stmt) (interpret-assignment stmt state)))
                  ((equal? kind 'if)
                   (interpret-statement (cdr stmt) (interpret-if stmt state)))
                  ((equal? kind 'sprint)
                   (interpret-statement (cdr stmt) (interpret-sprint stmt state)))
                  ((equal? kind 'while)
                   (interpret-statement (cdr stmt) (interpret-while stmt state)))
                  ((equal? kind 'block)
                   (interpret-statement (cdr stmt) (interpret-block stmt state (get-highest-scope state)))))))))


#|----------------------------------------------------------------------------------------------|
 |                          Start of code for interpret "sprint"                                |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET SPRINT
   Input : A sprint, state
   Output: The state that results from
           executing sprint in state
|#
(define (interpret-sprint sprint state)
  (cond ((= (length sprint) 3)
         (display (cadr sprint))
         (display (state-get-value state (caddr sprint)))
         (newline)
         state)
        (else
         (display (cadr sprint))
         (display state)
         (newline)
         state)))


#|----------------------------------------------------------------------------------------------|
 |                           Start of code for interpret "while"                                |
 |----------------------------------------------------------------------------------------------|#
#|
   INTERPRET WHILE
   Input : A while, state
   Output: The state that results from
           executing whle in state
|#
(define (interpret-while while state)
  (cond ((boolean-exp-state? (cadr while) state)
         (cond ((boolean-evaluate (cadr while) state)
                (interpret-while while (interpret-block (cdaddr while) state (+ (get-highest-scope state) 1))))
               (else state)))
        (else (error "Invalid boolean expression"))))


#|----------------------------------------------------------------------------------------------|
 |                     Start of code for operators used in "interpret"                          |
 |----------------------------------------------------------------------------------------------|#


#|
   EXPONENTIATION @
   Input : base, power
   Output: base^power
|#
(define (@ base power)
  (cond ((= power 0) 1)
        (else (* base (@ base (- power 1))))))

#|
   REMAINDER ?
   Input : numerator, denominator
   Output: numerator % denominator
|#
(define (? numerator denominator)
  (remainder numerator denominator))

#|
   LOGICAL AND &
   Input : bool1, bool2
   Output: bool1 LOGICAL AND bool2
|#
(define (& bool1 bool2)
  (and bool1 bool2))

#|
   LOGICAL OR %
   Input : bool1, bool2
   Output: bool1 LOGICAL OR bool2
|#
(define (% bool1 bool2)
  (or bool1 bool2))

#|
   LOGICAL NEGATION ~
   Input : bool
   Output: !bool
|#
(define (~ bool)
  (not bool))
