(defun partition-into-pairs (lst)
  (if (null lst)
      nil
      (cons (if (null (cdr lst)) (car lst) (list (car lst) (cadr lst)))
            (partition-into-pairs (cddr lst)))))

(defun my-merge (lst1 lst2)
  (cond
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((< (car lst1) (car lst2)) (cons (car lst1) (my-merge (cdr lst1) lst2)))
    (t (cons (car lst2) (my-merge lst1 (cdr lst2))))))

(defun merge-pairs (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) lst)
    (t (cons (my-merge (car lst) (cadr lst))
            (merge-pairs (cddr lst))))))

(defun bottom-up-mergesort (lst)
  (let ((pairs (partition-into-pairs lst)))
    (loop until (= (length pairs) 1)
          do (setq pairs (merge-pairs pairs))
          finally (return (car pairs)))))