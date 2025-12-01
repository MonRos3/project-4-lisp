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

(format t "Testing boolean-xor:~%")
(format t "(boolean-xor t t) => ~a (expected: NIL)~%" (boolean-xor t t))
(format t "(boolean-xor t nil) => ~a (expected: T)~%" (boolean-xor t nil))
(format t "(boolean-xor nil t) => ~a (expected: T)~%" (boolean-xor nil t))
(format t "(boolean-xor nil nil) => ~a (expected: NIL)~%" (boolean-xor nil nil))
(format t "~%")
