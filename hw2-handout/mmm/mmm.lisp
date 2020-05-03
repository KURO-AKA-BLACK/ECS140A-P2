(defun min-mean-max (xs)
    (let
        (
            (min (car xs))
            (mean (car xs))
            (cnt 1)
            (max (car xs))
        )
        (
            append (min_recur min (cdr xs)) (mean_recur mean (cdr xs) cnt) (max_recur max (cdr xs))
        )
    )
)

(defun min_recur (min xs)
    (if (car xs)
        (    ; then
            cond( 
                    (> min (car xs)) ; if (min > car xs)
                    (let
                        ((min (car xs))) 
                        (min_recur min (cdr xs))
                    )
                ) ; then {min = car xs; min_recur(cdr xs)}
                (
                    (<= min (car xs))
                    (min_recur min (cdr xs))
                ); else {min_recur(cdr xs)}
        )
        (
            list min
        )
    )
)

(defun mean_recur (mean xs cnt)
    (
        cond( 
                (car xs)
                (let 
                    ((mean (+ mean (car xs))) (cnt (+ cnt 1)))
                    (mean_recur mean (cdr xs) cnt)
                )
            )
            (
                (not (car xs))
                (list (/ mean cnt))
            )
    )
)

(defun max_recur (max xs)
    (if (car xs)
        (
            cond( 
                    (< max (car xs))
                    (let
                        ((max (car xs))) 
                        (max_recur max (cdr xs))
                    )
                )
                (
                    (>= max (car xs))
                    (max_recur max (cdr xs))
                )
        )
        (
            list max
        )
    )
)