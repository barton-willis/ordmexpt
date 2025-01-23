(in-package :maxima)

(defvar *bad* nil)
(let ((old-great (symbol-function 'great)))
  (defun great (a b)
    (let ((ans1) (ans2))
       (setq ans1 (funcall old-great a b))
       (setq ans2 (funcall old-great b a))
       (if (and (eq ans1 ans2) (not (alike1 a b)))
        (push (ftake 'mlist a b) *bad*))
       ans1)))

