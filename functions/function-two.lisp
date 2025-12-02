;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Examples:
;;   (set-union '(1 2) '(2 4)) => '(1 2 4)

(defun set-union (set-1 set-2)
  (cond
    ((equal set-1 nil) set-2)
    ((set-member set-2 (car set-1))
      (set-union (cdr set-1) set-2))
    (t (cons (car set-1)
          (set-union (cdr set-1) set-2)))))
          
;; ----------------------------------------------------------------------
;; Tests for set-union
;; ----------------------------------------------------------------------

(format t "Testing set-union:~%~%")

(format t "Basic numeric tests:~%")
(format t "(set-union '(1 2) '(2 4)) => ~a  (expected: (1 2 4))~%"
        (set-union '(1 2) '(2 4)))
(format t "(set-union '(1 2 3) '(3 4 5)) => ~a  (expected: (1 2 3 4 5))~%"
        (set-union '(1 2 3) '(3 4 5)))
(format t "(set-union '(1 2) '(3 4)) => ~a  (expected: (1 2 3 4))~%"
        (set-union '(1 2) '(3 4)))
(format t "~%")

(format t "Empty set tests:~%")
(format t "(set-union '() '(1 2)) => ~a  (expected: (1 2))~%"
        (set-union '() '(1 2)))
(format t "(set-union '(1 2) '()) => ~a  (expected: (1 2))~%"
        (set-union '(1 2) '()))
(format t "(set-union '() '()) => ~a  (expected: NIL)~%"
        (set-union '() '()))
(format t "~%")

(format t "Symbol tests:~%")
(format t "(set-union '(a b) '(b c)) => ~a  (expected: (a b c))~%"
        (set-union '(a b) '(b c)))
(format t "(set-union '(a b c) '(a b c)) => ~a  (expected: (a b c))~%"
        (set-union '(a b c) '(a b c)))
(format t "~%")

(format t "Nested list tests:~%")
(format t "(set-union '((1 2) (3 4)) '((3 4) (5 6))) => ~a  (expected: ((1 2) (3 4) (5 6)))~%"
        (set-union '((1 2) (3 4)) '((3 4) (5 6))))
(format t "~%")

(format t "String tests:~%")
(format t "(set-union '(\"a\" \"b\") '(\"b\" \"c\")) => ~a  (expected: (\"a\" \"b\" \"c\"))~%"
        (set-union '("a" "b") '("b" "c")))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

