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
;; Symbol
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
