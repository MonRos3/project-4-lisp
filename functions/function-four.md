# Function Four: Set Difference

This is a place for the working/unfinished function to exist, so we can see what's going on, collaborate, and test it before the final submission.

- Return the difference of set-1 and set-2.

- The result should contain no duplicates.

- Assume set-1 contains no duplicates and set-2 contains no duplicates.

- Examples:

  - (set-diff '(1 2) '(2 4)) => '(1)

```lisp
(defun set-diff (set-1 set-2)
  (cond
    ((equal set-1 nil) nil)

    ((not (set-member set-2 (car set-1)))
        (cons (car set-1)
            (set-diff (cdr set-1) set-2)))
        (t (set-diff (cdr set-1) set-2))))
```
