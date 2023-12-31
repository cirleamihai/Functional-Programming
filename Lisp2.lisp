; Return the level (depth) of a node in a tree of type (1). The level of the root element is 0.

(defun goleft (l nv ne)
    (cond
        ((null l) nil)
        ((= nv (+ ne 1)) nil)
        (t (cons (car l) (cons (cadr l) (goleft (cddr l) (+ nv 1) (+ ne (cadr l))))))
    )
)

(defun left (l)
    (goleft (cddr l) 0 0)
)

(defun right (l)
    (setq leftTree (left l))
    (removeLeft leftTree (cddr l))
)

(defun removeLeft (leftTree tree)
    (cond
        ((null leftTree) tree)
        ((equal (car leftTree) (car tree)) (removeLeft (cdr leftTree) (cdr tree)))
        (t nil)
    )
)

(defun findDepth (tree node depth)
    (cond
        ((null tree) nil)
        ((equal (car tree) node) depth)
        (t (or (findDepth (left tree) node (+ depth 1)) 
               (findDepth (right tree) node (+ depth 1))))
    )
)

(print (left '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0)))
(print (right '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0)))
(print (findDepth '(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) 'h 0))
