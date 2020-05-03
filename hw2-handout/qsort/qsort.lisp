(defun pivot_recur (lhs rhs xs n)
  (if (car xs)
    (
      cond( 
          (> n (car xs))
          (pivot_recur (append lhs (list (car xs))) rhs (cdr xs) n)
        )
        (
          (<= n (car xs))
          (pivot_recur lhs (append rhs (list (car xs))) (cdr xs) n)
        )
    )
    (
      list lhs rhs
    )
  )
)


(defun pivot (n xs)
  (let
    ( (lhs '()) (rhs '()) )
    (pivot_recur lhs rhs xs n)
  )
)

(defun len (xs n)
  (
    cond(
        (not (null (car (list xs))))
        (len (cdr xs) (+ n 1))
    )
    (
        (null (car (list xs)))
        (eval n)
    )
  )
)

(defun median (xs n cnt)
  (
    cond(
        (< cnt n)
        (if (cdr xs)
          (median (cdr xs) n (+ cnt 1))
          (car xs)
        )
    )
    (
        (>= cnt n)
        (car xs)
    )
  )
)

(defun pivot_recur_two (lhs rhs xs n jump)
  (if (car xs)
    (
      cond( 
          (> n (car xs))
          (if (null jump)
            (pivot_recur_two (append lhs (list (car xs))) rhs (cdr xs) n nil)
            (pivot_recur_two (append lhs (list (car xs))) rhs (cdr xs) n T)
          )
        )
        (
          (< n (car xs))
          (if (null jump)
            (pivot_recur_two lhs (append rhs (list (car xs))) (cdr xs) n nil)
            (pivot_recur_two lhs (append rhs (list (car xs))) (cdr xs) n T)
          )
        )
        (
          (= n (car xs))
          (if (null jump)
            (pivot_recur_two lhs rhs (cdr xs) n T)
            (pivot_recur_two lhs (append rhs (list (car xs))) (cdr xs) n T)
          )
        )
    )
    (
      list lhs rhs
    )
  )
)


(defun pivot_two (n xs)
  (let
    ( (lhs '()) (rhs '()) )
    (pivot_recur_two lhs rhs xs n nil)
  )
)

(defun quicksort (xs)
  (
    cond(
        (equal (len xs 0) 1)
        (car (list xs))
    )  
    (
        (equal (len xs 0) 0)
        (car (list xs))
    )
    (
        (> (len xs 0) 1)
        (let
            ( (p (median xs (/ (len xs 0) 2) 1) ) )
            (let 
              ( (lhs (car (pivot_two p xs))) (rhs (cdr (pivot_two p xs))) )
              (
                cond(
                    (and (null (car (pivot_two p xs))) (null (car rhs)))
                    (cons p)
                )
                (
                    (null (car (pivot_two p xs)))
                    (append (list p) (quicksort (car rhs)))
                )
                (
                    (null (car rhs))
                    (append (quicksort (car (pivot_two p xs))) (list p))
                )
                (
                    (and (not (null (car (pivot_two p xs)))) (not (null (car rhs))))
                    (append (quicksort (car (pivot_two p xs))) (list p) (quicksort (car rhs)))
                )
              )
            )
        )
    )
  )
)
