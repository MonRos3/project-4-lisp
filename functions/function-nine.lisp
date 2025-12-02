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
