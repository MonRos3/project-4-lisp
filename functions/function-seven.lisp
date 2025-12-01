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
    
(format t "Testing boolean-iff:~%")
(format t "(boolean-iff t t) => ~a (expected: T)~%"   (boolean-iff t t))
(format t "(boolean-iff t nil) => ~a (expected: NIL)~%" (boolean-iff t nil))
(format t "(boolean-iff nil t) => ~a (expected: NIL)~%" (boolean-iff nil t))
(format t "(boolean-iff nil nil) => ~a (expected: T)~%" (boolean-iff nil nil))
(format t "~%")
