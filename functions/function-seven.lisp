;; Return the bi-implication (if and only if) of a and b
;;
;; Examples:
;;  (boolean-iff t nil) => nil
;;  (boolean-iff nil nil) => t

(defun boolean-iff (a b)
  (cond
    ((and a b) t)
    ((and (not a) (not b)) t)
    (t nil)))
    
;; ----------------------------------------------------------------------
;; Tests for boolean-iff
;; ----------------------------------------------------------------------

(format t "Testing boolean-iff:~%~%")

(format t "Basic truth table tests:~%")
(format t "(boolean-iff t t) => ~a  (expected: T)~%"
        (boolean-iff t t))
(format t "(boolean-iff t nil) => ~a  (expected: NIL)~%"
        (boolean-iff t nil))
(format t "(boolean-iff nil t) => ~a  (expected: NIL)~%"
        (boolean-iff nil t))
(format t "(boolean-iff nil nil) => ~a  (expected: T)~%"
        (boolean-iff nil nil))
(format t "~%")

(format t "Repeated consistency tests:~%")
(format t "(boolean-iff t t) => ~a  (expected: T)~%"
        (boolean-iff t t))
(format t "(boolean-iff nil nil) => ~a  (expected: T)~%"
        (boolean-iff nil nil))
(format t "(boolean-iff t nil) => ~a  (expected: NIL)~%"
        (boolean-iff t nil))
(format t "~%")

(format t "Mixed expression tests:~%")
(format t "(boolean-iff (not nil) (and t t)) => ~a  (expected: T)~%"
        (boolean-iff (not nil) (and t t)))
(format t "(boolean-iff (not nil) nil) => ~a  (expected: NIL)~%"
        (boolean-iff (not nil) nil))
(format t "(boolean-iff (and t nil) (or nil nil)) => ~a  (expected: T)~%"
        (boolean-iff (and t nil) (or nil nil)))
(format t "(boolean-iff (and t nil) (or t nil)) => ~a  (expected: NIL)~%"
        (boolean-iff (and t nil) (or t nil)))
(format t "~%")

(format t "Non-boolean truthy/falsy tests:~%")
(format t "(boolean-iff 'foo 'bar) => ~a  (expected: T)~%"
        (boolean-iff 'foo 'bar))
(format t "(boolean-iff 'foo nil) => ~a  (expected: NIL)~%"
        (boolean-iff 'foo nil))
(format t "(boolean-iff nil 'foo) => ~a  (expected: NIL)~%"
        (boolean-iff nil 'foo))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
