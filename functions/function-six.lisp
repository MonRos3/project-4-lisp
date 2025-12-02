;; Return the implication of a and b
;;
;; Examples:
;;  (boolean-implies t nil) => nil
;;  (boolean-implies nil nil) => t

(defun boolean-implies (a b)
  (or (not a) b))
  
;; ----------------------------------------------------------------------
;; Tests for boolean-implies
;; ----------------------------------------------------------------------

(format t "Testing boolean-implies:~%~%")

(format t "Basic truth table tests:~%")
(format t "(boolean-implies t t) => ~a  (expected: T)~%"
        (boolean-implies t t))
(format t "(boolean-implies t nil) => ~a  (expected: NIL)~%"
        (boolean-implies t nil))
(format t "(boolean-implies nil t) => ~a  (expected: T)~%"
        (boolean-implies nil t))
(format t "(boolean-implies nil nil) => ~a  (expected: T)~%"
        (boolean-implies nil nil))
(format t "~%")

(format t "Repeated consistency tests:~%")
(format t "(boolean-implies nil nil) => ~a  (expected: T)~%"
        (boolean-implies nil nil))
(format t "(boolean-implies t nil) => ~a  (expected: NIL)~%"
        (boolean-implies t nil))
(format t "~%")

(format t "Mixed expression tests:~%")
(format t "(boolean-implies (not nil) nil) => ~a  (expected: NIL)~%"
        (boolean-implies (not nil) nil))
(format t "(boolean-implies nil (not nil)) => ~a  (expected: T)~%"
        (boolean-implies nil (not nil)))
(format t "(boolean-implies (and t t) (or nil nil)) => ~a  (expected: NIL)~%"
        (boolean-implies (and t t) (or nil nil)))
(format t "(boolean-implies (and t nil) (or t nil)) => ~a  (expected: T)~%"
        (boolean-implies (and t nil) (or t nil)))
(format t "~%")

(format t "Non-boolean truthy/falsy tests:~%")
(format t "(boolean-implies 'foo nil) => ~a  (expected: NIL)~%"
        (boolean-implies 'foo nil))
(format t "(boolean-implies nil 'foo) => ~a  (expected: T)~%"
        (boolean-implies nil 'foo))
(format t "(boolean-implies 'foo 'bar) => ~a  (expected: T)~%"
        (boolean-implies 'foo 'bar))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
