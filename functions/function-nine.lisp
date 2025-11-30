#|
Goal: Implement the merge sorting algorithm on an input list.
Assumption: All inputs will be a list, that may contain as few as zero values.
Complication, how do I recognize and react to the < and > characters in the function definition?

Psuedo:
def merge (list pred)
    if list length 0 or 1
        return list


    L = sorted first half
    R = sorted second half
    out = empty list
    filter is pred


|#

;; Perform merge sort on the lists.
;; Parameters:
;;   list: The list to sort
;;   predicate: A function to compare elements of the list
;;
;; Examples:
;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)
;;     (merge-sort '(2 1 5 0) #'>) => '(5 2 1 0)

#|
 Tools:
 Predicate Test:
 (equal predicate #'<)

 List halves:
 ?

 |#

(defun merge-sort (list predicate)
    (let ((halfL (/ (length list) 2 )))
        (cond
            (
                (= 0 (length list)) (list nil)
            )
            (
                (= 1 (length list)) list
            )
            (
                (> halfL 1)
                ( merge-x
                    (merge-sort (list ( firsthalf list halfL ) ) predicate)
                    (merge-sort ( secondhalf list halfL ) predicate)
                    predicate
                )
            )
            (
                (= halfL 1)
                ( merge-x (car list) (cdr list) predicate )
            )
        )
    )
)


(defun firsthalf (a len)
    (if
        (<= len 0)
        ()
        (cons (first a) (firsthalf (cdr a) (- len 1)) )
    )
)

(defun secondhalf (a len)
    (if
        (<= len 0)
        (secondhalf (cdr a) (- len 1))
        (list a)
    )
)

(defun merge-x (a b pred)
    (print (first a))
    (terpri)
    (if
        (equal pred #'<)
        (if
            (<= (first a) (first b))
            (cons (first a) (merge-x (cdr a) b pred))
            (cons (first b) (merge-x (cdr b) a pred))
        )
    )
    (if
        (>= (first a) (first b))
        (cons (first a) (merge-x (cdr a) b pred))
        (cons (first b) (merge-x (cdr b) a pred))
    )
)



(defun test-merge-sort (in pred target)
    (print in)
    (print pred)
    (print '_)
    (let ((out (merge-sort in pred)))
        (print out)
        (print (equal out target))
    )
)

#|
(defun halftests (in)
    (print (firsthalf in (/ (length in) 2)))
    (print (secondhalf in (/ (length in) 2)))
)
|#

(test-merge-sort '(2 1 5 0) #'< (list 0 1 2 5))
(test-merge-sort '(2 1 5 0) #'> (list 5 2 1 0))

;(halftests '(2 1 5 0))