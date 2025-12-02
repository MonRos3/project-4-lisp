;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)

(defun set-intersection (set-1 set-2)
    (cond
        ((equal set-1 nil) nil)
        ((set-member set-2 (car set-1))
            (cons (car set-1)
                (set-intersection (cdr set-1) set-2)))
    (t (set-intersection (cdr set-1) set-2))))

;; ----------------------------------------------------------------------
;; Tests for set-intersection
;; ----------------------------------------------------------------------

(format t "Testing set-intersection:~%~%")

(format t "Basic numeric tests:~%")
(format t "(set-intersection '(1 2) '(2 4)) => ~a  (expected: (2))~%"
        (set-intersection '(1 2) '(2 4)))
(format t "(set-intersection '(1 2 3) '(3 4 5)) => ~a  (expected: (3))~%"
        (set-intersection '(1 2 3) '(3 4 5)))
(format t "(set-intersection '(1 2 3) '(1 2 3)) => ~a  (expected: (1 2 3))~%"
        (set-intersection '(1 2 3) '(1 2 3)))
(format t "(set-intersection '(1 2 3) '(4 5 6)) => ~a  (expected: NIL)~%"
        (set-intersection '(1 2 3) '(4 5 6)))
(format t "~%")

(format t "Empty set tests:~%")
(format t "(set-intersection '() '(1 2 3)) => ~a  (expected: NIL)~%"
        (set-intersection '() '(1 2 3)))
(format t "(set-intersection '(1 2 3) '()) => ~a  (expected: NIL)~%"
        (set-intersection '(1 2 3) '()))
(format t "(set-intersection '() '()) => ~a  (expected: NIL)~%"
        (set-intersection '() '()))
(format t "~%")

(format t "Symbol tests:~%")
(format t "(set-intersection '(a b c) '(b c d)) => ~a  (expected: (b c))~%"
        (set-intersection '(a b c) '(b c d)))
(format t "(set-intersection '(a b c) '(c b a)) => ~a  (expected: (a b c))~%"
        (set-intersection '(a b c) '(c b a)))
(format t "(set-intersection '(x y z) '(a b c)) => ~a  (expected: NIL)~%"
        (set-intersection '(x y z) '(a b c)))
(format t "~%")

(format t "Order / subset tests:~%")
(format t "(set-intersection '(1 2 3 4) '(2 4 6 8)) => ~a  (expected: (2 4))~%"
        (set-intersection '(1 2 3 4) '(2 4 6 8)))
(format t "(set-intersection '(2 4 6 8) '(1 2 3 4)) => ~a  (expected: (2 4))~%"
        (set-intersection '(2 4 6 8) '(1 2 3 4)))
(format t "(set-intersection '(1 2 3 4 5) '(2 3)) => ~a  (expected: (2 3))~%"
        (set-intersection '(1 2 3 4 5) '(2 3)))
(format t "~%")

(format t "Nested list tests:~%")
(format t "(set-intersection '((1 2) (3 4)) '((3 4) (5 6))) => ~a  (expected: ((3 4)))~%"
        (set-intersection '((1 2) (3 4)) '((3 4) (5 6))))
(format t "(set-intersection '((1 2) (3 4) (5 6)) '((5 6) (1 2))) => ~a  (expected: ((1 2) (5 6)))~%"
        (set-intersection '((1 2) (3 4) (5 6)) '((5 6) (1 2))))
(format t "~%")

(format t "String tests:~%")
(format t "(set-intersection '(\"a\" \"b\" \"c\") '(\"b\" \"d\")) => ~a  (expected: (\"b\"))~%"
        (set-intersection '("a" "b" "c") '("b" "d")))
(format t "(set-intersection '(\"a\" \"b\") '(\"c\" \"d\")) => ~a  (expected: NIL)~%"
        (set-intersection '("a" "b") '("c" "d")))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
