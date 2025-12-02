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

;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;;
;; Examples:
;;  (boolean-eval '(and t nil)) => nil
;;  (boolean-eval '(and t (or nil t)) => t

(defun boolean-eval (exp)
  (cond
    ;; Literal T or NIL
    ((or (equal exp t) (equal exp nil))
     exp)

    ;; Single-element list: evaluate its only element
    ((and (not (equal exp nil))
          (equal (length exp) 1))
     (boolean-eval (first exp)))

    ;; NOT
    ((equal (first exp) 'not)
     (not (boolean-eval (second exp))))

    ;; Binary operators: AND, OR, xor, implies, iff
    ((or
      (equal (first exp) 'AND)
      (equal (first exp) 'OR)
      (equal (first exp) 'xor)
      (equal (first exp) 'implies)
      (equal (first exp) 'iff))
     (let* ((a (boolean-eval (second exp)))  ;; first subexpression
            (b (boolean-eval (third  exp)))  ;; second subexpression
           )
       (cond
         ;; AND
         ((equal (first exp) 'AND)     (and a b))
         ;; OR
         ((equal (first exp) 'OR)      (or a b))
         ;; XOR (calls your function)
         ((equal (first exp) 'xor)     (boolean-xor a b))
         ;; IMPLIES (calls your function)
         ((equal (first exp) 'implies) (boolean-implies a b))
         ;; IFF (calls your function)
         ((equal (first exp) 'iff)     (boolean-iff a b)))))

    ;; Fallback for invalid expressions
    (t nil)))

;; ----------------------------------------------------------------------
;; Tests for boolean-eval
;; ----------------------------------------------------------------------

(format t "Testing boolean-eval:~%~%")

;; ------------------------------------------------------------
;; Literal tests
;; ------------------------------------------------------------
(format t "Literal tests:~%")
(format t "(boolean-eval t) => ~a  (expected: T)~%"
        (boolean-eval t))
(format t "(boolean-eval nil) => ~a  (expected: NIL)~%"
        (boolean-eval nil))
(format t "~%")

;; ------------------------------------------------------------
;; NOT tests
;; ------------------------------------------------------------
(format t "NOT tests:~%")
(format t "(boolean-eval '(not t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(not t)))
(format t "(boolean-eval '(not nil)) => ~a  (expected: T)~%"
        (boolean-eval '(not nil)))
(format t "(boolean-eval '(not (not t))) => ~a  (expected: T)~%"
        (boolean-eval '(not (not t))))
(format t "(boolean-eval '(not (not nil))) => ~a  (expected: NIL)~%"
        (boolean-eval '(not (not nil))))
(format t "~%")

;; ------------------------------------------------------------
;; AND tests
;; ------------------------------------------------------------
(format t "AND tests:~%")
(format t "(boolean-eval '(AND t t)) => ~a  (expected: T)~%"
        (boolean-eval '(AND t t)))
(format t "(boolean-eval '(AND t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND t nil)))
(format t "(boolean-eval '(AND nil t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND nil t)))
(format t "(boolean-eval '(AND nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(AND nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; OR tests
;; ------------------------------------------------------------
(format t "OR tests:~%")
(format t "(boolean-eval '(OR t t)) => ~a  (expected: T)~%"
        (boolean-eval '(OR t t)))
(format t "(boolean-eval '(OR t nil)) => ~a  (expected: T)~%"
        (boolean-eval '(OR t nil)))
(format t "(boolean-eval '(OR nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(OR nil t)))
(format t "(boolean-eval '(OR nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(OR nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; XOR tests (integration with boolean-xor)
;; ------------------------------------------------------------
(format t "XOR tests (via boolean-xor):~%")
(format t "(boolean-eval '(xor t t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(xor t t)))
(format t "(boolean-eval '(xor t nil)) => ~a  (expected: T)~%"
        (boolean-eval '(xor t nil)))
(format t "(boolean-eval '(xor nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(xor nil t)))
(format t "(boolean-eval '(xor nil nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(xor nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; IMPLIES tests (integration with boolean-implies)
;; ------------------------------------------------------------
(format t "IMPLIES tests (via boolean-implies):~%")
(format t "(boolean-eval '(implies t t)) => ~a  (expected: T)~%"
        (boolean-eval '(implies t t)))
(format t "(boolean-eval '(implies t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(implies t nil)))
(format t "(boolean-eval '(implies nil t)) => ~a  (expected: T)~%"
        (boolean-eval '(implies nil t)))
(format t "(boolean-eval '(implies nil nil)) => ~a  (expected: T)~%"
        (boolean-eval '(implies nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; IFF tests (integration with boolean-iff)
;; ------------------------------------------------------------
(format t "IFF tests (via boolean-iff):~%")
(format t "(boolean-eval '(iff t t)) => ~a  (expected: T)~%"
        (boolean-eval '(iff t t)))
(format t "(boolean-eval '(iff t nil)) => ~a  (expected: NIL)~%"
        (boolean-eval '(iff t nil)))
(format t "(boolean-eval '(iff nil t)) => ~a  (expected: NIL)~%"
        (boolean-eval '(iff nil t)))
(format t "(boolean-eval '(iff nil nil)) => ~a  (expected: T)~%"
        (boolean-eval '(iff nil nil)))
(format t "~%")

;; ------------------------------------------------------------
;; Nested expression tests
;; ------------------------------------------------------------
(format t "Nested expression tests:~%")
(format t "(boolean-eval '(AND (OR t nil) (not nil))) => ~a  (expected: T)~%"
        (boolean-eval '(AND (OR t nil) (not nil))))
(format t "(boolean-eval '(OR (AND t nil) (implies t nil))) => ~a  (expected: NIL)~%"
        (boolean-eval '(OR (AND t nil) (implies t nil))))
(format t "(boolean-eval '(iff (xor t nil) (not nil))) => ~a  (expected: T)~%"
        (boolean-eval '(iff (xor t nil) (not nil))))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform merge sort on the lists.
;; Parameters:
;;   list: The list to sort
;;   predicate: A function to compare elements of the list
;;
;; Examples:
;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)
;;     (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0)

(defun merge-sort (list predicate)
  (labels
      ((half-length (n)
         (if (<= n 1)
             0
             (+ 1 (half-length (- n 2))))))
    (let* ((len (length list))
           (halfL (half-length len)))
      (cond
        ;; empty or single element: already sorted
        ((or (= len 0) (= len 1)) list)
        (t
         (merge-x
          (merge-sort (firsthalf list halfL) predicate)
          (merge-sort (secondhalf list halfL) predicate)
          predicate))))))
          
(defun firsthalf (a len)
  (if (<= len 0)
      nil
      (cons (first a)
            (firsthalf (cdr a) (- len 1)))))

(defun secondhalf (a len)
  (if (<= len 0)
      a
      (secondhalf (cdr a) (- len 1))))

(defun merge-x (a b pred)
  (cond
    ;; if one side is empty, return the other
    ((equal a nil) b)
    ((equal b nil) a)
    ;; choose smallest (or largest)
    ((funcall pred (first a) (first b))
     (cons (first a) (merge-x (cdr a) b pred)))
    (t
     (cons (first b) (merge-x a (cdr b) pred)))))
     
;; ----------------------------------------------------------------------
;; Tests for merge-sort
;; ----------------------------------------------------------------------

(format t "Testing merge-sort:~%~%")

;; ------------------------------------------------------------
;; Basic numeric ascending tests
;; ------------------------------------------------------------
(format t "Numeric ascending (#'<) tests:~%")
(format t "(merge-sort '(3 1 4 2) #'<) => ~a  (expected: (1 2 3 4))~%"
        (merge-sort '(3 1 4 2) #'<))
(format t "(merge-sort '(5 5 3 1 4) #'<) => ~a  (expected: (1 3 4 5 5))~%"
        (merge-sort '(5 5 3 1 4) #'<))
(format t "(merge-sort '() #'<) => ~a  (expected: NIL)~%"
        (merge-sort '() #'<))
(format t "(merge-sort '(42) #'<) => ~a  (expected: (42))~%"
        (merge-sort '(42) #'<))
(format t "(merge-sort '(1 2 3 4) #'<) => ~a  (expected: (1 2 3 4))~%"
        (merge-sort '(1 2 3 4) #'<))
(format t "~%")

;; ------------------------------------------------------------
;; Numeric descending tests
;; ------------------------------------------------------------
(format t "Numeric descending (#'>) tests:~%")
(format t "(merge-sort '(3 1 4 2) #'>) => ~a  (expected: (4 3 2 1))~%"
        (merge-sort '(3 1 4 2) #'>))
(format t "(merge-sort '(5 5 3 1 4) #'>) => ~a  (expected: (5 5 4 3 1))~%"
        (merge-sort '(5 5 3 1 4) #'>))
(format t "~%")

;; ------------------------------------------------------------
;; Long list / stress tests
;; ------------------------------------------------------------
(format t "Long list stress tests:~%")
(format t "(merge-sort <reversed 0-99> #'<) => ~a~%  (expected: 0-99)~%"
        (merge-sort
         '(99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80
           79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60
           59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40
           39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20
           19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
         #'<))
(format t "expected: (0 1 2 ... 99)~%~%")

(format t "(merge-sort 0-99 #'>) => ~a~%  (expected: 99-0)~%"
        (merge-sort
         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
           24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43
           44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
           64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83
           84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)
         #'>))
(format t "expected: (99 98 ... 0)~%~%")

(format t "(merge-sort <random-100> #'<) => ~a~%  (expected: 0-99)~%"
        (merge-sort
         '(92 99 59 38 64 61 51 35 4 49 29 26 2 83 23 6 48 45 8 11
           32 5 28 52 79 43 19 21 12 88 98 82 87 50 36 39 81 1 40 56
           96 94 54 9 73 22 60 67 31 71 14 91 25 33 3 58 65 78 47 46
           30 37 27 20 70 16 17 86 93 44 41 97 80 62 0 57 10 53 95 68
           42 84 24 85 72 55 75 76 89 63 18 15 74 66 7 34 13 90 69 77)
         #'<))
(format t "expected: (0 1 2 ... 99)~%~%")

(format t "(merge-sort <random-100> #'>) => ~a~%  (expected: 99-0)~%"
        (merge-sort
         '(92 99 59 38 64 61 51 35 4 49 29 26 2 83 23 6 48 45 8 11
           32 5 28 52 79 43 19 21 12 88 98 82 87 50 36 39 81 1 40 56
           96 94 54 9 73 22 60 67 31 71 14 91 25 33 3 58 65 78 47 46
           30 37 27 20 70 16 17 86 93 44 41 97 80 62 0 57 10 53 95 68
           42 84 24 85 72 55 75 76 89 63 18 15 74 66 7 34 13 90 69 77)
         #'>))
(format t "expected: (99 98 ... 0)~%~%")
(format t "~%")

;; ------------------------------------------------------------
;; Symbol / string tests
;; ------------------------------------------------------------
(format t "Symbol / string tests:~%")
(format t "(merge-sort '(c a d b) #'char<) => ~a  (expected: (a b c d))~%"
        (merge-sort '(#\c #\a #\d #\b) #'char<))
(format t "(merge-sort '(\"pear\" \"apple\" \"banana\") 'string<) => ~a  (expected: (\"apple\" \"banana\" \"pear\"))~%"
        (merge-sort '(\"pear\" \"apple\" \"banana\") 'string<))
(format t "~%")

;; ------------------------------------------------------------
;; Already sorted vs reverse sorted
;; ------------------------------------------------------------
(format t "Already-sorted / reverse-sorted checks:~%")
(format t "(merge-sort '(1 2 3 4 5 6) #'<) => ~a  (expected: (1 2 3 4 5 6))~%"
        (merge-sort '(1 2 3 4 5 6) #'<))
(format t "(merge-sort '(6 5 4 3 2 1) #'<) => ~a  (expected: (1 2 3 4 5 6))~%"
        (merge-sort '(6 5 4 3 2 1) #'<))
(format t "~%")
(format t "(merge-sort '(6 5 4 3 2 1) #'<) => ~a  (expected: (1 2 3 4 5 6))~%"
        (merge-sort '(6 5 4 3 2 1) #'<))
(format t "~%")
