# Function Three: Set Intersection

This is a place for the working/unfinished function to exist, so we can see what's going on, collaborate, and test it before the final submission.

- Return the intersection of set-1 and set-2.

- The result should contain no duplicates.

- Assume set-1 contains no duplicates and set-2 contains no duplicates.

- Examples:

  - (set-intersection '(1 2) '(2 4)) => '(2)

```lisp
(defun set-intersection (set-1 set-2)
    (cond
        ((equal set-1 nil) nil)
        ((set-member set-2 (car set-1))
            (cons (car set-1)
                (set-intersection (cdr set-1) set-2)))
    (t (set-intersection (cdr set-1) set-2)))
)
```
