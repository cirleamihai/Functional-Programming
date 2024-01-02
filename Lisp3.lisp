;11. Write a function to determine the depth of a list.
; (1 (2 3) (4 (5 (6(7))))) has depth 5

(defun depthL (l level)
    (cond
        ((not (listp l)) level) ; if the element is not a list we return the level
        ((null l) (+ 1 level)) ; if the element is an empty list, we return level + 1
        (t (apply #'max (mapcar (lambda (subL) (depthL subL (+ level 1))) l))) ; get the maximum for each of the remaining elements
    )
)

(print(depthL '(1 (2 (3 (5 (6 ()))) 3) (4 (5 (6 (7))))) 0))
