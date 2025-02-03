(in-package :maxima)

;; Detect some (not all!) bugs in `great`. 
(defvar *bad* nil)
(let ((old-great (symbol-function 'great)))
  (defun great (a b)
    (let ((ans1) (ans2))
       (setq ans1 (funcall old-great a b))
       (setq ans2 (funcall old-great b a))
       (if (and (eq ans1 ans2) (not (alike1 a b)))
        (push (ftake 'mlist a b) *bad*))
       ans1)))

#| 
(defun random-shuffle-list (lst)
  "Shuffles LST by sorting it with a random comparator."
  (sort lst (lambda (a b) (< (random 2) 1))))


(defvar *simplus* nil)
  (let ((old-simplus (symbol-function 'simplus)))
    (defun simplus (x w z)
      (let* ((ans) (arg1) (arg2))
            (setq ans (funcall old-simplus x w z))
            (setq arg1 (if (mplusp ans) (cdr ans) (list ans)))
            (setq arg2 (sort arg1 #'great))
            (when (and (not (has-float x)) (not (alike1 (fapply 'mlist arg1) (fapply 'mlist arg2))))
              (push x *simplus*))
           ans)))

(defvar *simptimes* nil)
  (let ((old-simptimes (symbol-function 'simptimes)))
    (defun simptimes (x w z)
       (let ((ans) (arg1) (arg2))
             (setq ans (funcall old-simptimes x w z))
             (setq arg1 (copy-list (if (mtimesp ans) (cdr ans) (list ans))))
             (setq arg2 (sort arg1 #'great))
             (when (not (alike1 (fapply 'mlist arg1) (fapply 'mlist arg2)))
              (push x *simptimes*))
         ans)))
|#
