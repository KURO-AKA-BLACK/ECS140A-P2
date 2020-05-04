(defun foo (state input)
    (cond
    ((and (equal state 0) (null input)) (list 0))
    ((and (equal state 1) (null input)) (list 1))
    ((and (equal state 2) (null input)) (list 2))
    ((and (equal state 3) (null input)) (list 3))
    ((and (equal state 0) (eq input 'A ))  (list 1 2))
    ((and (equal state 0) (eq input 'B ))  (list 2))
    ((and (equal state 1) (eq input 'B ))  (list 3))
    ((and (equal state 2) (eq input 'c ))  (list 3))
    )
)

(defun exp (state input)
    (cond
    ((and (eq state 0) (null input)) (list 0))
    ((and (eq state 1) (null input)) (list 1))
    ((and (eq state 2) (null input)) (list 2))
    ((and (eq state 0) (eq input 'A ))  (list 1 2))
    ((and (eq state 0) (eq input 'B ))  (list 2))
    ((and (eq state 1) (eq input 'B ))  (list 0))
    )
)

(defun lang (state input)
  (cond
    ((and (eq state 0) (null input)) (list 0))
    ((and (eq state 1) (null input)) (list 1))
    ((and (eq state 0) (eq input 'A ))  (list 0))
    ((and (eq state 0) (eq input 'B ))  (list 1))
    ((and (eq state 1) (eq input 'A ))  (list 1))
    ((and (eq state 1) (eq input 'B ))  (list 0))
    )
)

(defun recur_foo (prev curr input)
    (
        cond(
            (not (null (car (list prev))))
            (recur_foo (cdr prev) (append curr (foo (car (car (list prev))) (car (list input)))) input) 
        )
        (
            (null (car (list prev)))
            (car (list curr))
        )
    )
)

(defun recur_foo_two (prev input)
    (
        cond(
            (not (null (car input)))
            (recur_foo_two (recur_foo prev '() (car input)) (cdr input))
        )
        (
            (null (car input))
            (car (list prev))
        )
    )
)

(defun foo_check (result desire)
    (
        if (not (null (car result)))
            (
                cond(
                    (equal (car result) desire)
                    (eval T)
                )
                (
                    (not (equal (car result) desire))
                    (foo_check (cdr result) desire)
                )
            )
            (
                eval nil
            )
    )
)

(defun recur_exp (prev curr input)
    (
        cond(
            (not (null (car (list prev))))
            (recur_exp (cdr prev) (append curr (exp (car (car (list prev))) (car (list input)))) input) 
        )
        (
            (null (car (list prev)))
            (car (list curr))
        )
    )
)

(defun recur_exp_two (prev input)
    (
        cond(
            (not (null (car input)))
            (recur_exp_two (recur_exp prev '() (car input)) (cdr input))
        )
        (
            (null (car input))
            (car (list prev))
        )
    )
)

(defun exp_check (result desire)
    (
        if (not (null (car result)))
            (
                cond(
                    (equal (car result) desire)
                    (eval T)
                )
                (
                    (not (equal (car result) desire))
                    (exp_check (cdr result) desire)
                )
            )
            (
                eval nil
            )
    )
)


(defun recur_lang (prev curr input)
    (
        cond(
            (not (null (car (list prev))))
            (recur_lang (cdr prev) (append curr (lang (car (car (list prev))) (car (list input)))) input) 
        )
        (
            (null (car (list prev)))
            (car (list curr))
        )
    )
)

(defun recur_lang_two (prev input)
    (
        cond(
            (not (null (car input)))
            (recur_lang_two (recur_lang prev '() (car input)) (cdr input))
        )
        (
            (null (car input))
            (car (list prev))
        )
    )
)

(defun lang_check (result desire)
    (
        if (not (null (car result)))
            (
                cond(
                    (equal (car result) desire)
                    (eval T)
                )
                (
                    (not (equal (car result) desire))
                    (lang_check (cdr result) desire)
                )
            )
            (
                eval nil
            )
    )
)

(defun reachable (transition start final input)
    (
        let
            ( (prev (list start)) )
            (
                cond(
                    (equal (car (list transition)) 'FOOTRANSITIONS)
                    (foo_check (recur_foo_two prev input) final)
                )
                (
                    (equal (car (list transition)) 'expTransitions)
                    (exp_check (recur_exp_two prev input) final)
                )
                (
                    (equal (car (list transition)) 'langTransitions)
                    (lang_check (recur_lang_two prev input) final)
                )
            )
    )
)
