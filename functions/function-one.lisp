;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-member '(1 2) 3) =>  NIL

(defun set-member (set item)
  (cond
    ((equal set nil) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))

;; ----------------------------------------------------------------------
;; Tests for set-member
;; ----------------------------------------------------------------------

(format t "Testing belongingness (set-member):~%~%")

(format t "Basic tests:~%")
(format t "(set-member '(1 2) 1) => ~a  (expected: T)~%"
        (set-member '(1 2) 1))
(format t "(set-member '(1 2) 3) => ~a  (expected: NIL)~%"
        (set-member '(1 2) 3))
(format t "(set-member '(1 2 3 4) 4) => ~a  (expected: T)~%"
        (set-member '(1 2 3 4) 4))
(format t "(set-member '() 1) => ~a  (expected: NIL)~%"
        (set-member '() 1))
(format t "~%")

(format t "Symbol tests:~%")
(format t "(set-member '(a b c) 'b) => ~a  (expected: T)~%"
        (set-member '(a b c) 'b))
(format t "(set-member '(a b c) 'z) => ~a  (expected: NIL)~%"
        (set-member '(a b c) 'z))
(format t "(set-member '(1 a 2 b) 'a) => ~a  (expected: T)~%"
        (set-member '(1 a 2 b) 'a))
(format t "(set-member '(1 a 2 b) 3) => ~a  (expected: NIL)~%"
        (set-member '(1 a 2 b) 3))
(format t "~%")

(format t "Nested list tests:~%")
(format t "(set-member '((1 2) (3 4)) '(1 2)) => ~a  (expected: T)~%"
        (set-member '((1 2) (3 4)) '(1 2)))
(format t "(set-member '((1 2) (3 4)) '(2 3)) => ~a  (expected: NIL)~%"
        (set-member '((1 2) (3 4)) '(2 3)))
(format t "~%")

(format t "Long list / edge tests:~%")
(format t "(set-member '(1 2 3 4 5 6 7 8 9 10) 10) => ~a  (expected: T)~%"
        (set-member '(1 2 3 4 5 6 7 8 9 10) 10))
(format t "(set-member '(1 2 3 4 5 6 7 8 9 10) 11) => ~a  (expected: NIL)~%"
        (set-member '(1 2 3 4 5 6 7 8 9 10) 11))
(format t "(set-member '(1 2 2 3) 2) => ~a  (expected: T)~%"
        (set-member '(1 2 2 3) 2))
(format t "~%")

(format t "String tests:~%")
(format t "(set-member '(\"a\" \"b\" \"c\") \"b\") => ~a  (expected: T)~%"
        (set-member '("a" "b" "c") "b"))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

