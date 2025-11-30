# Function One: Belongingness

This is a place for the working/unfinished function to exist, so we can see what's going on, collaborate, and test it before the final submission.

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
