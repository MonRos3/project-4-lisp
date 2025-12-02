;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;;
;; Examples:
;;  (boolean-eval '(and t nil)) => nil
;;  (boolean-eval '(and t (or nil t)) => t

(defun boolean-eval (exp)
  (cond
    ;; Literal T or NIL
    ((or (equal exp t) (equal exp nil))
     exp)

    ;; Single-element list: evaluate its only element
    ((and (not (equal exp nil))
          (equal (length exp) 1))
     (boolean-eval (first exp)))

    ;; NOT
    ((equal (first exp) 'not)
     (not (boolean-eval (second exp))))

    ;; Binary operators: AND, OR, xor, implies, iff
    ((or
      (equal (first exp) 'AND)
      (equal (first exp) 'OR)
      (equal (first exp) 'xor)
      (equal (first exp) 'implies)
      (equal (first exp) 'iff))
     (let* ((a (boolean-eval (second exp)))  ;; first subexpression
            (b (boolean-eval (third  exp)))  ;; second subexpression
           )
       (cond
         ;; AND
         ((equal (first exp) 'AND)     (and a b))
         ;; OR
         ((equal (first exp) 'OR)      (or a b))
         ;; XOR (calls your function)
         ((equal (first exp) 'xor)     (boolean-xor a b))
         ;; IMPLIES (calls your function)
         ((equal (first exp) 'implies) (boolean-implies a b))
         ;; IFF (calls your function)
         ((equal (first exp) 'iff)     (boolean-iff a b)))))

    ;; Fallback for invalid expressions
    (t nil)))

;; ----------------------------------------------------------------------
;; Tests for boolean-eval
;; ----------------------------------------------------------------------

(format t "Testing boolean-eval:~%~%")

;; ------------------------------------------------------------
;; Literal tests
;; ------------------------------------------------------------
(format t "Literal tests:~%")
(format t "(boolean-eval t) => ~a  (expected: T)~%"
        (boolean-eval t))
(format t "(boolean-eval nil) => ~a  (expected: NIL)~%"
        (boolean-eval nil))
(format t "~%")

;; ------------------------------------------------------------
;; NOT tests
;; ------------------------------------------------------------
(format t "NOT tests:~%")
(format t "(boolean-eval '(not t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(not t)))
(format t "(boolean-eval '(not nil)) => ~a  (expected: T)~%"
        (boolean-eval '(not nil)))
(format t "(boolean-eval '(not (not t))) => ~a  (expected: T)~%"
        (boolean-eval '(not (not t))))
(format t "(boolean-eval '(not (not nil))) => ~a  (expected: NIL)~%"
        (boolean-eval '(not (not nil))))
(format t "~%")

;; ------------------------------------------------------------
;; AND tests
;; ------------------------------------------------------------
(format t "AND tests:~%")
(format t "(boolean-eval '(AND t t)) => ~a  (expected: T)~%"
        (boolean-eval '(AND t t)))
(format t "(boolean-eval '(AND t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND t nil)))
(format t "(boolean-eval '(AND nil t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND nil t)))
(format t "(boolean-eval '(AND nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; OR tests
;; ------------------------------------------------------------
(format t "OR tests:~%")
(format t "(boolean-eval '(OR t t)) => ~a  (expected: T)~%"
        (boolean-eval '(OR t t)))
(format t "(boolean-eval '(OR t nil)) => ~a  (expected: T)~%"
        (boolean-eval '(OR t nil)))
(format t "(boolean-eval '(OR nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(OR nil t)))
(format t "(boolean-eval '(OR nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(OR nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; XOR tests (integration with boolean-xor)
;; ------------------------------------------------------------
(format t "XOR tests (via boolean-xor):~%")
(format t "(boolean-eval '(xor t t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(xor t t)))
(format t "(boolean-eval '(xor t nil)) => ~a  (expected: T)~%"
        (boolean-eval '(xor t nil)))
(format t "(boolean-eval '(xor nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(xor nil t)))
(format t "(boolean-eval '(xor nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(xor nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; IMPLIES tests (integration with boolean-implies)
;; ------------------------------------------------------------
(format t "IMPLIES tests (via boolean-implies):~%")
(format t "(boolean-eval '(implies t t)) => ~a  (expected: T)~%"
        (boolean-eval '(implies t t)))
(format t "(boolean-eval '(implies t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(implies t nil)))
(format t "(boolean-eval '(implies nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(implies nil t)))
(format t "(boolean-eval '(implies nil nil)) => ~a  (expected: T)~%"
        (boolean-eval '(implies nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; IFF tests (integration with boolean-iff)
;; ------------------------------------------------------------
(format t "IFF tests (via boolean-iff):~%")
(format t "(boolean-eval '(iff t t)) => ~a  (expected: T)~%"
        (boolean-eval '(iff t t)))
(format t "(boolean-eval '(iff t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(iff t nil)))
(format t "(boolean-eval '(iff nil t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(iff nil t)))
(format t "(boolean-eval '(iff nil nil)) => ~a  (expected: T)~%"
        (boolean-eval '(iff nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; Nested expression tests
;; ------------------------------------------------------------
(format t "Nested expression tests:~%")
(format t "(boolean-eval '(AND (OR t nil) (not nil))) => ~a  (expected: T)~%"
        (boolean-eval '(AND (OR t nil) (not nil))))
(format t "(boolean-eval '(OR (AND t nil) (implies t nil))) => ~a  (expected: NIL)~%"
        (boolean-eval '(OR (AND t nil) (implies t nil))))
(format t "(boolean-eval '(iff (xor t nil) (not nil))) => ~a  (expected: T)~%"
        (boolean-eval '(iff (xor t nil) (not nil))))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
