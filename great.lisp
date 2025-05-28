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

;(defun random-shuffle-list (lst)
;  "Shuffles LST by sorting it with a random comparator."
 ; (sort lst #'(lambda (a b) (< (random 2) 1))))


(defvar *simplus* nil)
  (let ((old-simplus (symbol-function 'simplus)))
    (defun simplus (x w z)
      (let* ((ans1) ($inflag t))
            (setq ans1 (funcall old-simplus x w z))
            (when (and (mplusp ans1) (not (alike (cdr ans1) (sort (cdr ans1) #'great))))
              (mtell "x = ~M ; ans1 = ~M ~%" x ans1)
              (push (ftake 'mlist x ans1) *simplus*))
           ans1)))

;(defvar *simptimes* nil)
;  (let ((old-simptimes (symbol-function 'simptimes)))
 ;   (defun simptimes (x w z)
 ;;      (let ((ans1) (ans2))
   ;          (setq ans1 (funcall old-simptimes x w z))
             ;(setq ans2 ($expand ans1 0 0))
             ;(when (not (approx-alike ans1 ans2))
             ; (mtell "x = ~M ; ans1 = ~M ; ans2 = ~M ~%" x ans1 ans2)
             ; (push (ftake x ans1 ans2) *simptimes*))
    ;     ans1)))