;; Step 1: Partition function
(defun partition (lst)
  (let ((mid (floor (length lst) 2)))
    (values (subseq lst 0 mid) (subseq lst mid))))  ;; Partition into two sublists

;; Step 2: Merge function
(defun my-merge (list1 list2)
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((< (car list1) (car list2))
     (cons (car list1) (my-merge (cdr list1) list2)))
    (t
     (cons (car list2) (my-merge list1 (cdr list2))))))

;; Step 3: Mergesort function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))  ; Base case
      lst
      (multiple-value-bind (half1 half2) (partition lst)  ; Partition the list into two parts
        (my-merge (mergesort half1) (mergesort half2)))))  ; Recursively sort and merge

;; Testing the mergesort function
(print (mergesort '(3 1 4 1 5 9 2 6 5 3 5)))  ;; Expected output: (1 1 2 3 3 4 5 5 5 6 9)

