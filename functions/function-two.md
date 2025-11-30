# Function Two: Set Union

This is a place for the working/unfinished function to exist, so we can see what's going on, collaborate, and test it before the final submission.

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
