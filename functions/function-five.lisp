;; Return the exclusive or of a and b
;;
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil

(defun boolean-xor (a b)
  (cond
    ((and a (not b)) t)
    ((and (not a) b) t)
    (t nil)))

;; ----------------------------------------------------------------------
;; Tests for boolean-xor
;; ----------------------------------------------------------------------

(format t "Testing boolean-xor:~%~%")

(format t "Basic truth table tests:~%")
(format t "(boolean-xor t t) => ~a  (expected: NIL)~%"
        (boolean-xor t t))
(format t "(boolean-xor t nil) => ~a  (expected: T)~%"
        (boolean-xor t nil))
(format t "(boolean-xor nil t) => ~a  (expected: T)~%"
        (boolean-xor nil t))
(format t "(boolean-xor nil nil) => ~a  (expected: NIL)~%"
        (boolean-xor nil nil))
(format t "~%")

(format t "Redundant / repeated tests:~%")
(format t "(boolean-xor t t) => ~a  (expected: NIL)~%"
        (boolean-xor t t))
(format t "(boolean-xor nil nil) => ~a  (expected: NIL)~%"
        (boolean-xor nil nil))
(format t "~%")

(format t "Mixed expression tests:~%")
(format t "(boolean-xor (not nil) nil) => ~a  (expected: T)~%"
        (boolean-xor (not nil) nil))
(format t "(boolean-xor nil (not nil)) => ~a  (expected: T)~%"
        (boolean-xor nil (not nil)))
(format t "(boolean-xor (and t t) (or nil nil)) => ~a  (expected: T)~%"
        (boolean-xor (and t t) (or nil nil)))
(format t "(boolean-xor (and t nil) (or t nil)) => ~a  (expected: T)~%"
        (boolean-xor (and t nil) (or t nil)))
(format t "~%")

(format t "Non-boolean edge-like values:~%")
(format t "(boolean-xor 'foo nil) => ~a  (expected: T)~%"
        (boolean-xor 'foo nil))
(format t "(boolean-xor nil 'foo) => ~a  (expected: T)~%"
        (boolean-xor nil 'foo))
(format t "(boolean-xor 'foo 'bar) => ~a  (expected: NIL)~%"
        (boolean-xor 'foo 'bar))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
