;; Function to handle the non-terminal EE (EE corresponds to oG EE | ε)
(defun EEfn (str)
  (print '( in EEfn)) (print str)
  (cond
    ((and (eql (car str) 'o) (member (cadr str) '(x y z w)))  ;; oG EE
     (Afn (cdr (cdr str))))  ;; Process G and continue with EE
    (t (cdr str))))  ;; Empty case (ε), just return the rest of the string
	
;; Function to handle the non-terminal E (Afn corresponds to E)
(defun Afn (str)
  (print '( in Afn)) (print str)
  (cond
    ((member (car str) '(x y z w)) (EEfn (cdr str)))  ;; G EE
    (t (append (cdr str) (list 'err)))))
	
;; Function to handle the non-terminal I (Sfn corresponds to I)
(defun Sfn (str)
  (print '( in Sfn)) (print str)
  (cond
    ((eql (car str) 'i) (Afn (cdr str)))  ;; Matches iE I'
    (t (append (cdr str) (list 'err)))))
	
;; Function to handle the non-terminal G (corresponds to x | y | z | w)
(defun Gfn (str)
  (print '( in Gfn)) (print str)
  (cond
    ((member (car str) '(x y z w)) (cdr str))  ;; Match x, y, z, or w
    (t (append (cdr str) (list 'err)))))  ;; Error if no match
	
;; Function to handle the non-terminal S (Bfn corresponds to S)
(defun Bfn (str)
  (print '( in Bfn)) (print str)
  (cond
    ((eql (car str) 's) (cdr str))  ;; Match s
    ((and (eql (car str) 'd) (eql (cadr str) 'L))  ;; Match dLb
     (Lfn (cdr (cdr str))))  ;; Process L
    (t (append (cdr str) (list 'err)))))  ;; Error if no match
	
;; Function to handle the non-terminal L (Lfn corresponds to L)
(defun Lfn (str)
  (print '( in Lfn)) (print str)
  (cond
    ((eql (car str) 's) (cdr str))  ;; Match s
    ((member (car str) '(x y z w)) (LLfn str))  ;; Check LL for recursion
    (t (append (cdr str) (list 'err)))))  ;; Error if no match

;; Function to handle the non-terminal LL (LLfn corresponds to sLL | ε)
(defun LLfn (str)
  (print '( in LLfn)) (print str)
  (cond
    ((eql (car str) 's) (Lfn (cdr str)))  ;; Match s and recurse
    (t str)))  ;; ε case, return the rest of the string