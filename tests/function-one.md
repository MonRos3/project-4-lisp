# Function One Tests: Belongingness

These tests check whether the belongingness function works as expected; requires the function code to be defined above it/before running.

```lisp
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
