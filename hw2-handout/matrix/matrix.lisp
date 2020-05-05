(defun add_recur_one (m1 m2 result)
    (
        cond(
            (and (not (null (car m1))) (not (null (car m2))))
            (add_recur_one (cdr m1) (cdr m2) (append result (add_recur_two (car m1) (car m2) '())))
        )
        (
            (and (null (car m1)) (null (car m2)))
            (car (list result))
        )
    )
)

(defun add_recur_two (m1 m2 result)
    (
        cond(
            (and (not (null (car m1))) (not (null (car m2))))
            
            (let
                ((result (append result (list (+ (car m1) (car m2))))))
                (add_recur_two (cdr m1) (cdr m2) result)
            )

        )
        (
            (and (null (car m1)) (null (car m2)))
            (list result)
        )
    )
)

(defun matrix-add (m1 m2)
   (add_recur_one m1 m2 '())
)

(defun mul_recur_one (a b result)
    (
        cond(
            (not (null (car a)))
            (mul_recur_one (cdr a) b (append result (list (mul_recur_two (car a) (matrix-transpose b) '()))))
        )
        (
            (null (car a))
            (car (list result))
        )
    )
)

(defun mul_recur_two(a b result)
    (
        cond(
            (not (null (car b)))
            (mul_recur_two a (cdr b) (append result (list (mul_recur_three a (car b) 0))))
        )
        (
            (null (car b))
            (car (list result))
        )
    )
)

(defun mul_recur_three (a b result)
    (
        cond(
            (not (null (car b)))
            (mul_recur_three (cdr a) (cdr b) (+ result (* (car a) (car b))))
        )
        (
            (null (car b))
            (car (list result))
        )
    )
)

(defun matrix-multiply (a b)
    (mul_recur_one a b '())
)

(defun transpose_recur_one (m result)
    (
        cond(
            (not (null (car m)))
            (transpose_recur_one (matrix_reduce m '()) (append result (list (transpose_recur_two m '()))))
        )
        (
            (null (car m))
            (car (list result))
        )
    )
)

(defun transpose_recur_two (m result)
    (
        cond(
            (not (null (car m)))
            (transpose_recur_two (cdr m) (append result (list (car (car m)))))
        )
        (
            (null (car m))
            (car (list result))
        )
    )
)

(defun matrix_reduce (m result)
    (
        if (not (null (car m)))
            (
                if(not (null (car m)))
                    (matrix_reduce (cdr m) (append result (list (cdr (car m)))))
                    (list result)
            )
            (car (list result))
    )
)

(defun matrix-transpose (m)
    (transpose_recur_one m '())
)
