; 1. set-member
(defun set-member (set item)
  (cond
    ((equal set nil) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))

; 2. set-union
(defun set-union (set-1 set-2)
  (cond
    ((equal set-1 nil) set-2)
    ((set-member set-2 (car set-1))
      (set-union (cdr set-1) set-2))
    (t (cons (car set-1)
          (set-union (cdr set-1) set-2)))))

; 3. set-intersection
(defun set-intersection (set-1 set-2)
    (cond
        ((equal set-1 nil) nil)
        ((set-member set-2 (car set-1))
            (cons (car set-1)
                (set-intersection (cdr set-1) set-2)))
    (t (set-intersection (cdr set-1) set-2)))
)

; 4. set-difference
(defun set-diff (set-1 set-2)
  (cond
    ((equal set-1 nil) nil)

    ((not (set-member set-2 (car set-1)))
        (cons (car set-1)
            (set-diff (cdr set-1) set-2)))
        (t (set-diff (cdr set-1) set-2))))

; 5. boolean-xor
(defun boolean-xor (a b)
  (cond
    ((and a (not b)) t)
    ((and (not a) b) t)
    (t nil)))


; 6. boolean-implies
(defun boolean-implies (a b)
  (or (not a) b))

; 7. boolean-iff
(defun boolean-iff (a b)
  (cond
    ((and a b) t)
    ((and (not a) (not b)) t)
    (t nil)))

; 8. boolean-eval
(defun boolean-eval (exp)
    (let
        (in (list exp))
        ;(print in)
        ;(print exp)
        (cond
            (
                (or
                    (equal exp nil)
                    (equal exp t)
                )
                (cond
                    ((equal exp t) t)
                    ((equal exp nil) nil)
                )
            )
            (
                (equal (length exp) 1)
                (boolean-eval (first exp))
            )
            (
                (equal (first exp) `not)
                (not (boolean-eval (cdr exp)))
            )
            (
                (or
                    (equal (first exp) `AND)
                    (equal (first exp) `OR)
                    (equal (first exp) `xor)
                    (equal (first exp) `implies)
                    (equal (first exp) `iff)
                ) ;; Operators: {and, or, xor, implies, iff}
                (let
                    (
                        (a ;; Assign a to the first sub expression
                            (boolean-eval (car (cdr exp)))
                        )
                        (b ;; Assign b to the second sub expression
                            (boolean-eval (cdr (cdr exp)))
                        )
                    )
                    (cond
                        ;; AND
                        (
                            (equal (car exp) `AND) (and a b)
                        )
                        ;; OR
                        ((equal (car exp) `OR) (or a b))
                        ;; XOR
                        ((equal (first exp) `xor) (boolean-xor a b))
                        ;; IMPLIES
                        ((equal (first exp) `implies) (boolean-implies a b))
                        ;; IFF
                        ((equal (first exp) `iff) (boolean-iff a b))
                        ((t) (nil))
                    )
                )
            )
        )
    )
)

; 9. merge-sort
(defun merge-sort (list predicate)
  (labels
      ;; compute half the length using only +, -, <=
      ((half-length (n)
         (if (<= n 1)
             0
             (+ 1 (half-length (- n 2))))))
    (let* ((len (length list))
           (halfL (half-length len)))
      (cond
        ;; empty list
        ((= len 0) nil)
        ;; single element
        ((= len 1) list)
        ;; general case: split, sort halves, then merge
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
    ;; use the given predicate to choose the next element
    ((funcall pred (first a) (first b))
     (cons (first a) (merge-x (cdr a) b pred)))
    (t
     (cons (first b) (merge-x a (cdr b) pred)))))

(defun test-merge-sort (in pred target)
    (print in)
    (print pred)
    (print '_)
    (let ((out (merge-sort in pred)))
        (print out)
        (print (equal out target))
        (if (not (equal out target))
            (print target)
        )
    )
    (terpri)(terpri)
)

(test-merge-sort '(2 1 5 0) #'< '(0 1 2 5))
(test-merge-sort '(2 1 5 0) #'> '(5 2 1 0))
(test-merge-sort '(99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
    #'< '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99))
(test-merge-sort '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99)
    #'> '(99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))

(test-merge-sort '(92 99 59 38 64 61 51 35 4 49 29 26 2 83 23 6 48 45 8 11 32 5 28 52 79 43 19 21 12 88 98 82 87 50 36 39 81 1 40 56 96 94 54 9 73 22 60 67 31 71 14 91 25 33 3 58 65 78 47 46 30 37 27 20 70 16 17 86 93 44 41 97 80 62 0 57 10 53 95 68 42 84 24 85 72 55 75 76 89 63 18 15 74 66 7 34 13 90 69 77)
    #'< '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99))
(test-merge-sort '(92 99 59 38 64 61 51 35 4 49 29 26 2 83 23 6 48 45 8 11 32 5 28 52 79 43 19 21 12 88 98 82 87 50 36 39 81 1 40 56 96 94 54 9 73 22 60 67 31 71 14 91 25 33 3 58 65 78 47 46 30 37 27 20 70 16 17 86 93 44 41 97 80 62 0 57 10 53 95 68 42 84 24 85 72 55 75 76 89 63 18 15 74 66 7 34 13 90 69 77)
    #'> '(99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80 79 78 77 76 75 74 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))