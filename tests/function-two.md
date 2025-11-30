# Function Two Tests: Set Union

These tests check whether the belongingness function works as expected; requires the function code to be defined above it/before running.

```lisp
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
