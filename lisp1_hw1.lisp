; a) Write a function to eliminate the n-th element of a linear list.
( defun elemN (l n pos)
    (cond
        ((null l) ())
        ((= n pos) (elemN (cdr l) n (+ pos 1)))
        (t (cons (car l) (elemN (cdr l) n (+ pos 1))))
    )
)

( defun mainElem (l n)
    (elemN l n 1)
)

(print "a)")
(print(mainElem '(1 2 3 4) 2))

; b) Write a function to determine the successor of a number represented digit by digit as a list, without 
;  transforming the representation of the number from list to number. Example: (1 9 3 5 9 9) --> (1 9 3 6 0 0)
( defun sumSuc (l cf)
    (cond
        ( (and (null l) (/= cf 0)) (list cf) )
        ( (and (null l) (= cf 0)) () )
        ( (= cf 1) (cons (mod (+ (car l) 1) 10) (sumSuc (cdr l) (floor (+ (car l) 1) 10))) )
        ( t l )
    )
)

( defun mainSuc (l)
    (setq revL (reverse l))
    (setq fElem (car revL))
    (reverse (cons (mod (+ fElem 1) 10) (sumSuc (cdr revL) (floor (+ fElem 1) 10))))
)
(print "b)")
(print(mainSuc '(1 9 2 9)))
(print(mainSuc '(1 9 9)))
(print(mainSuc '(9 9)))
(print(mainSuc '(2 3 4)))

; c) Write a function to return the set of all the atoms of a list.
;  Exemplu: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)
( defun retAt (l)
    (cond
        ( (null l) nil) 
        ( (listp (first l)) (append (retAt (first l)) (retAt (rest l))))
        ( t (append (list (first l)) (retAt (rest l))) )
    )
)

( defun inside (l e)
    (cond
        ((null l) nil)
        ((equal (first l) e) t)
        (t (inside (rest l) e))
    )
)

( defun makeSet (l c)
    (cond
        ( (null l) c )
        ( (inside c (first l)) (makeSet (rest l) c) )
        ( t (makeSet (rest l) (append c (list(first l)))))
    )
)

( defun mainC (l)
    (makeSet (retAt l) ())
)

(print "c)")
(print(mainC '(1 (2 (1 3 (2 a) 3) 1) (1 4 a))))

; d) Write a function to test whether a linear list is a set.

( defun equalLists (l1 l2)
    (cond
        ((and (null l1) (null l2)) t)
        ((equal (first l1) (first l2)) (equalLists (rest l1) (rest l2)))
        (t nil)
    )
)

( defun mainD (l)
    (setq setFromList (makeSet (retAt l) ()))
    (equalLists setFromList l)
)
(print "d)")
(print(mainD '(1 2 3 4 5 1)))
(print(mainD '(1 2 3 4)))
