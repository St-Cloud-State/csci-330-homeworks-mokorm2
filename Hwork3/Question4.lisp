; Function to insert an item into the sorted list
(defun insert-into-sorted (item sorted)
  (cond
   ((null sorted) (list item))  ; If the sorted list is empty, return the item as a new list
   ((<= item (car sorted)) (cons item sorted))  ; Insert the item at the front if it is smaller
   (t (cons (car sorted) (insert-into-sorted item (cdr sorted))))))  ; Otherwise, recurse to find the right spot

; Insertion sort function
(defun insertion-sort (unsorted sorted)
  (cond
   ((null unsorted) sorted)  ; If the unsorted list is empty, return the sorted list
   (t (insertion-sort (cdr unsorted) (insert-into-sorted (car unsorted) sorted)))))  ; Otherwise, sort the rest

; Wrapper function for convenience (starts with empty sorted list)
(defun sort-list (unsorted)
  (insertion-sort unsorted nil))


(sort-list '(5 3 8 4 2))      ;; Expected output: (2 3 4 5 8)