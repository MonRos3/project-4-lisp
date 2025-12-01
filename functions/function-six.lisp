
;; Return the implication of a and b

;;

;; Examples:

;;  (boolean-implies t nil) => nil

;;  (boolean-implies nil nil) => t

(defun boolean-implies (a b)
  (or (not a) b))
  
(format t "Testing boolean-implies:~%")
(format t "(boolean-implies t t) => ~a (expected: T)~%"   (boolean-implies t t))
(format t "(boolean-implies t nil) => ~a (expected: NIL)~%" (boolean-implies t nil))
(format t "(boolean-implies nil t) => ~a (expected: T)~%" (boolean-implies nil t))
(format t "(boolean-implies nil nil) => ~a (expected: T)~%" (boolean-implies nil nil))
(format t "~%")
