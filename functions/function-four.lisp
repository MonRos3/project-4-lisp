;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;;
;; Examples:
;;   (set-diff '(1 2) '(2 4)) => '(1)

(defun set-diff (set-1 set-2)
  (cond
    ((equal set-1 nil) nil)

    ((not (set-member set-2 (car set-1)))
        (cons (car set-1)
            (set-diff (cdr set-1) set-2)))
        (t (set-diff (cdr set-1) set-2))))
        
;; ----------------------------------------------------------------------
;; Tests for set-diff
;; ----------------------------------------------------------------------

(format t "Testing set-diff:~%~%")

(format t "Basic numeric tests:~%")
(format t "(set-diff '(1 2) '(2 4)) => ~a  (expected: (1))~%"
        (set-diff '(1 2) '(2 4)))
(format t "(set-diff '(1 2 3) '(3 4 5)) => ~a  (expected: (1 2))~%"
        (set-diff '(1 2 3) '(3 4 5)))
(format t "(set-diff '(1 2 3) '(1 2 3)) => ~a  (expected: NIL)~%"
        (set-diff '(1 2 3) '(1 2 3)))
(format t "(set-diff '(1 2 3 4) '(2 4)) => ~a  (expected: (1 3))~%"
        (set-diff '(1 2 3 4) '(2 4)))
(format t "~%")

(format t "Empty set tests:~%")
(format t "(set-diff '() '(1 2 3)) => ~a  (expected: NIL)~%"
        (set-diff '() '(1 2 3)))
(format t "(set-diff '(1 2 3) '()) => ~a  (expected: (1 2 3))~%"
        (set-diff '(1 2 3) '()))
(format t "(set-diff '() '()) => ~a  (expected: NIL)~%"
        (set-diff '() '()))
(format t "~%")

(format t "Symbol tests:~%")
(format t "(set-diff '(a b c) '(b c d)) => ~a  (expected: (a))~%"
        (set-diff '(a b c) '(b c d)))
(format t "(set-diff '(a b c) '(c b a)) => ~a  (expected: NIL)~%"
        (set-diff '(a b c) '(c b a)))
(format t "(set-diff '(x y z) '(a b c)) => ~a  (expected: (x y z))~%"
        (set-diff '(x y z) '(a b c)))
(format t "~%")

(format t "Order / subset tests:~%")
(format t "(set-diff '(1 2 3 4 5) '(2 3)) => ~a  (expected: (1 4 5))~%"
        (set-diff '(1 2 3 4 5) '(2 3)))
(format t "(set-diff '(2 4 6 8) '(1 2 3 4)) => ~a  (expected: (6 8))~%"
        (set-diff '(2 4 6 8) '(1 2 3 4)))
(format t "~%")

(format t "Nested list tests:~%")
(format t "(set-diff '((1 2) (3 4)) '((3 4) (5 6))) => ~a  (expected: ((1 2)))~%"
        (set-diff '((1 2) (3 4)) '((3 4) (5 6))))
(format t "(set-diff '((1 2) (3 4) (5 6)) '((5 6) (1 2))) => ~a  (expected: ((3 4)))~%"
        (set-diff '((1 2) (3 4) (5 6)) '((5 6) (1 2))))
(format t "~%")

(format t "String tests:~%")
(format t "(set-diff '(\"a\" \"b\" \"c\") '(\"b\" \"d\")) => ~a  (expected: (\"a\" \"c\"))~%"
        (set-diff '("a" "b" "c") '("b" "d")))
(format t "(set-diff '(\"a\" \"b\") '(\"a\" \"b\")) => ~a  (expected: NIL)~%"
        (set-diff '("a" "b") '("a" "b")))
(format t "~%")
