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

(defun question_exist (pattern exist)
   (
      if (car pattern)
         (
            if (equal (car pattern) '?)
               (eval T)
               (question_exist (cdr pattern) nil)
         )
         (
            eval exist
         )
   )
)

(defun exclamation_exist (pattern exist)
   (
      if (car pattern)
         (
            if (equal (car pattern) '!)
               (eval T)
               (exclamation_exist (cdr pattern) nil)
         )
         (
            eval exist
         )
   )
)

(defun complete_comparision (pattern assertion)
   (
      if (not (equal (len pattern 0) (len assertion 0)))
         (eval nil)
         (
            if (not (null (car pattern)))
               (
                  cond(
                     (equal (car pattern) (car assertion))
                     (complete_comparision (cdr pattern) (cdr assertion))
                  )
                  (
                     (not (equal (car pattern) (car assertion)))
                     (eval nil)
                  )
               )
               (
                  eval T
               )
         )
   )
)

(defun question_comparision (pattern assertion)
   (
      if (not (equal (len pattern 0) (len assertion 0)))
         (eval nil)
         (
            if (not (null (car pattern)))
               (
                  cond(
                     (equal (car pattern) (car assertion))
                     (question_comparision (cdr pattern) (cdr assertion))
                  )
                  (
                     (and (equal (car pattern) '?) (not (null (car assertion))))
                     (question_comparision (cdr pattern) (cdr assertion))
                  )
                  (
                     (not (equal (car pattern) (car assertion)))
                     (eval nil)
                  )
               )
               (
                  eval T
               )
         )
   )
)

(defun my_search (letter pattern)
   (if (not (null (car pattern)))
      (
         cond (
            (equal (car pattern) letter)
            (car (list pattern))
         )
         (
            (not (equal (car pattern) letter))
            (my_search letter (cdr pattern))
         )
      )
      (
         eval nil
      )
   )
)


(defun exclamation_comparision (pattern assertion)
   (
      cond (
         (> (len pattern 0) (len pattern 0))
         (eval nil)
      )
      (
         (and (equal (car pattern) '!) (equal (len (cdr pattern) 0) 0))
         (eval T)
      )
      (
         (equal (car pattern) '!)
         (
            if (not (null (my_search (car (cdr pattern)) (cdr assertion))))
               (exclamation_comparision (cdr pattern) (my_search (car (cdr pattern)) (cdr assertion)))
               (eval nil)
         )
      )
      (
         (equal (car pattern) (car assertion))
         (
            cond( 
               (and (null (cdr pattern)) (null (cdr assertion)))
               (eval T)
            )
            (
               (or (null (cdr pattern)) (null (cdr assertion)))
               (eval nil)
            )
            (
               (and (not (null (cdr pattern))) (not (null (cdr pattern))))
               (exclamation_comparision (cdr pattern) (cdr assertion))
            )
         )
      )
   )
)

(defun count_question (pattern cnt)
   (
      if (equal (car pattern) '?)
         (+ cnt 1)
         (list cnt)
   )
)

(defun adjust (l cnt max)
   (
      cond(
         (<= cnt max)
         (adjust (cdr l) (+ cnt 1) max)
      )
      (
         (> cnt max)
         (car (list l))
      )
   )
)

(defun complex_comparison (pattern assertion)
   (
      cond(
         (and (null (car pattern)) (null (car assertion)))
         (eval T)
      )
      (
         (or (null (car (list pattern))) (null (car (list assertion))))
         (eval nil)
      )
      (
         (and (not (equal (car pattern) '!)) (not (equal (car pattern) '?)))
         (
            if (not (equal (car pattern) (car assertion)))
               (eval nil)
               (complex_comparison (cdr pattern) (cdr assertion))
         )
      )
      (
         (equal (car pattern) '?)
         (complex_comparison (cdr pattern) (cdr assertion))
      )
      (
         (equal (car pattern) '!)
         (
            cond(
               (equal (car (cdr pattern)) '?)
               (complex_comparison (adjust pattern 0 (count_question (cdr pattern) 0)) (adjust assertion 0 (count_question (cdr pattern) 0)))
            )
            (
               (not (equal (car (cdr pattern)) '?))
               (complex_comparison (cdr pattern) (my_search (car (cdr pattern)) (cdr assertion)))
            )
         )
      )
   )
)



(defun match (pattern assertion)
   (
      cond(
         (and (null (exclamation_exist pattern nil)) (null (question_exist pattern nil)))
         (complete_comparision pattern assertion)
      )
      (
         (and (null (exclamation_exist pattern nil)) (not (null (question_exist pattern nil)))) ; ?
         (question_comparision pattern assertion)
      )
      (
         (and (not (null (exclamation_exist pattern nil))) (null (question_exist pattern nil))) ; !
         (exclamation_comparision pattern assertion)
      )
      (
         (and (not (null (exclamation_exist pattern nil))) (not (null (question_exist pattern nil)))) ; !?
         (
            if (> (len pattern 0) (len assertion 0))
               (eval nil)
               (complex_comparison pattern assertion)
         )
      )
   )
)
