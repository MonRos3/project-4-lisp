#|
Armed with list definitions, and assuming a maximum of two elements supplied
to any operator, what sort of psuedo algorithm can I come up with:

function boolparse(list) -> bool
    if list[0] == not
        if length list[1] > 1
            return boolparse(list[1])
        else
            return not list[1]
    else // operator from {and, or, xor, implies, iff}
        a = (if length list[1] == 1 ( list[1] ) else boolparse(list[1]))
        b = (if length list[2] == 1 ( list[2] ) else boolparse(list[2]))
        if list[1] == and
            return and a b
        if list[1] == or
            return or a b
        if list[1] == xor
            xor(a b)
        if list[1] == implies
            implication(a b)
        if list[1] == iff
            biimplication(a b)
        else return nil

Maybe helpful:
https://www.youtube.com/watch?v=cKK-Y1-jAHM

|#

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
                        ;;((equal (first exp) `xor) (boolean-xor a b))
                        ;; IMPLIES
                        ;;((equal (first exp) `implies) (boolean-implies a b))
                        ;; IFF
                        ;;((equal (first exp) `iff) (boolean-iff a b))
                        ;((t) (nil))
                    )
                )
            )
        )
    )
)

#|
(defun test_be (in)
    (print in)
    (if (boolean-eval in)
        (format t ": TRUE~%")
        (format t ": FALSE~%")
    )
)

(test_be `t)
(test_be `nil)
(test_be `(and t t))
(test_be `(and t nil))
(test_be `(and nil t))
(test_be `(and nil nil))
(test_be `(or t t))
(test_be `(or t nil))
(test_be `(or nil t))
(test_be `(or nil nil))
(test_be `(and t (or nil t)))
(test_be `(and nil (or nil t)))
(test_be `(and t (or nil nil)))
(test_be `(and (or nil (or t nil)) (or (and t nil) nil)))
|#