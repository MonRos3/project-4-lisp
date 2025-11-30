# Project 4 Part 1: Functional programming

The purpose of the project is to familiarize you with functional programming and the Common Lisp development environment.  
Please finish the functions below and submit your lisp source code file on Canvas.

> **Note:** Use only the following standard Lisp functions, macros, operators, and constants in your definitions, along with any previously completed functions in this project:
>
>     - T
>     - NIL
>     - IF
>     - WHEN
>     - COND
>     - NOT
>     - AND
>     - OR
>     - EQUAL
>     - CONS
>     - LIST
>     - CAR
>     - CDR
>     - FIRST
>     - SECOND
>     - THIRD
>     - LENGTH
>     - DEFUN
>     - LABELS
>     - LET
>     - LET*
>     - FUNCALL
>     - QUOTE
>     - any arithmetic operator or relation (+, -, *, /, <, <=, >, >=, =)
>     - any numerical constant

---

## Function One: Set Belongingness

- Return T if item is a member of set.

- Return NIL if item is not a member of set.

- The type of set is list.

- Examples:

  - (set-member '(1 2) 1) => T

  - (set-member '(1 2) 3) => NIL

```lisp
(defun set-member (set item)
  (cond
    ((equal set nil) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))
```

```lisp
;; Test cases for belongingness
(format t "Testing belongingness:~%")
(format t "(set-member '(1 2) 1) => ~a (expected: T)~%" (set-member '(1 2) 1))
(format t "(set-member '(1 2) 3) => ~a (expected: NIL)~%" (set-member '(1 2) 3))
(format t "(set-member '(1 2 3 4) 4) => ~a (expected: T)~%" (set-member '(1 2 3 4) 4))
(format t "(set-member '() 1) => ~a (expected: NIL)~%~%" (set-member '() 1))
```

```lisp
;; Tests for belongingness function
(defun test-set-member ()
  (let ((passed 0) (failed 0))
    ;; Test 1: Item is in set
    (if (equal t (set-member '(1 2) 1))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-member '(1 2) 1~%") (setq failed (+ failed 1))))

    ;; Test 2: Item is not in set
    (if (equal nil (set-member '(1 2) 3))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-member '(1 2) 3~%") (setq failed (+ failed 1))))

    ;; Test 3: Item at end of set
    (if (equal t (set-member '(1 2 3 4) 4))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-member '(1 2 3 4) 4~%") (setq failed (+ failed 1))))

    ;; Test 4: Empty set
    (if (equal nil (set-member '() 1))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-member '() 1~%") (setq failed (+ failed 1))))

    (format t "~%belongingness tests - Passed: ~a, Failed: ~a~%" passed failed)))

(test-set-member)
```

---

## Function Two: Set Union

- Return the union of set-1 and set-2.

- The result should contain no duplicates.

- Assume set-1 contains no duplicates and set-2 contains no duplicates.

- Examples:

  - (set-union '(1 2) '(2 4)) => '(1 2 4)

```lisp
(defun set-union (set-1 set-2)
  (cond
    ((equal set-1 nil) set-2)
    ((set-member set-2 (car set-1))
      (set-union (cdr set-1) set-2))
    (t (cons (car set-1)
          (set-union (cdr set-1) set-2)))))
```

```lisp
;; Test cases for set union
(format t "Testing set-union:~%")
(format t "(set-union '(1 2) '(2 4)) => ~a (expected: (1 2 4))~%" (set-union '(1 2) '(2 4)))
(format t "(set-union '(1 2 3) '(3 4 5)) => ~a (expected: (1 2 3 4 5))~%" (set-union '(1 2 3) '(3 4 5)))
(format t "(set-union '() '(1 2)) => ~a (expected: (1 2))~%" (set-union '() '(1 2)))
(format t "(set-union '(1 2) '()) => ~a (expected: (1 2))~%" (set-union '(1 2) '()))
```

```lisp
;; Tests for set union function
(defun test-set-union ()
  (let ((passed 0) (failed 0))
    ;; Test 1: Basic union with overlap
    (if (equal '(1 2 4) (set-union '(1 2) '(2 4)))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-union '(1 2) '(2 4)~%") (setq failed (+ failed 1))))

    ;; Test 2: Multiple overlaps
    (if (equal '(1 2 3 4 5) (set-union '(1 2 3) '(3 4 5)))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-union '(1 2 3) '(3 4 5)~%") (setq failed (+ failed 1))))

    ;; Test 3: First set empty
    (if (equal '(1 2) (set-union '() '(1 2)))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-union '() '(1 2)~%") (setq failed (+ failed 1))))

    ;; Test 4: Second set empty
    (if (equal '(1 2) (set-union '(1 2) '()))
        (setq passed (+ passed 1))
        (progn (format t "FAIL: set-union '(1 2) '()~%") (setq failed (+ failed 1))))

    (format t "~%set-union tests - Passed: ~a, Failed: ~a~%" passed failed)))

(test-set-union)
```

---

## Function Three: Set Intersection

- Return the intersection of set-1 and set-2.

- The result should contain no duplicates.

- Assume set-1 contains no duplicates and set-2 contains no duplicates.

- Examples:

  - (set-intersection '(1 2) '(2 4)) => '(2)

```lisp
(defun set-intersection (set-1 set-2)

  ;;Your implementation go here

)
```

---

## Function Four: Set Difference

- Return the difference of set-1 and set-2.

- The result should contain no duplicates.

- Assume set-1 contains no duplicates and set-2 contains no duplicates.

- Examples:

  - (set-diff '(1 2) '(2 4)) => '(1)

```lisp
(defun set-diff (set-1 set-2)

  ;;Your implementation go here

)
```

---

## Function Five: XOR

- Return the exclusive or of a and b

- Examples:

  - (boolean-xor t nil) => t

  - (boolean-xor nil nil) => nil

```lisp
(defun boolean-xor (a b)

  ;;Your implementation go here

)
```

---

## Function Six: Boolean Implication

- Return the implication of a and b

- Examples:

  - (boolean-implies t nil) => nil

  - (boolean-implies nil nil) => t

```lisp
(defun boolean-implies (a b)

;;<Your implementation go here >

)
```

---

## Function Seven: Boolean Bijection

- Return the bi-implication (if and only if) of a and b

- Examples:

  - (boolean-iff t nil) => nil

  - (boolean-iff nil nil) => t

```lisp
(defun boolean-iff (a b)

;;<Your implementation go here >

)
```

---

## Function Eight: Boolean Expression

- Evaluate a boolean expression.

- Handle NOT, AND, OR, XOR, IMPLIES, and IFF.

- Examples:

  - (boolean-eval '(and t nil)) => nil

  - (boolean-eval '(and t (or nil t)) => t

```lisp
(defun boolean-eval (exp)

;;<Your implementation go here >

)
```

---

## Function Nine: Merge Sort

- Perform merge sort on the lists.

- Parameters:

  - list: The list to sort

  - predicate: A function to compare elements of the list

- Examples:

  - (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)

  - (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0)

```lisp
(defun merge-sort (list predicate)

;;<Your implementation go here >

)
```
