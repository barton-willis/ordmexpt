(in-package :maxima)

(defun gather-terms (e fn) 
   "Return a CL list of the unique terms of the expression `e` that satisfy the 
    predicate `fn`. The function 'alike1' determines uniqueness. Example:
      (gather-terms XXX #'$mapatom) returns a CL list of the mapatoms in XXX"
   (let ((ans nil))
    (cond 
      (($mapatom e) 
        (if (funcall fn e) (list e) nil))
      (t 
         (setq ans (reduce #'append (mapcar #'(lambda (q) (gather-terms q fn)) (cdr e))))
         (when (funcall fn e)
           (push e ans))
         (remove-duplicates ans :test #'alike1)))))

(defun constant-subexpression-subs (e x)
"Find possible expressions of `e` involving a variable `x` that are piecewise
 constant. Return a CL list of two Maxima lists that can be used to make
 these substitutions and to revert them. The constant subexpressions have
 the form:

 (a) log(X^Y) - Y log(X), where either X or Y depends on `x` and the term log(X^Y) is
     a term in the expression. The forward substitution (fsub) is log(X^Y) = Y log(X) + C,
     where `C` is a gensym. The backward substitution is C = log(X^Y) - Y log(X).
   
(b) (X^Y)^Z / X^(Y Z), where either X, Y, or Z depends on `x` and the term (X^Y)^Z
    is a term in the expression `e`. The forward substitution (fsub) is (X^Y)^Z = C X^(Y Z),
     where `C` is a gensym. The backward substitution is C = (X^Y)^Z / X^(Y Z)."
 (let ((xargs (gather-terms e #'(lambda (q) (and (mlogp q) (not (freeof x q))))))
         (qq) (c) (fsub nil) (rsub nil))
     ;; gather substitutions of the form log(X^Y) = Y log(X) + cnst
     (dolist (q xargs)
        (setq qq (let (($domain '$real)) ($expand q 0 0)))
        (when (not (alike1 q qq))
          (setq c (gensym))
          (push (ftake 'mequal q (add c qq)) fsub)
          (push (ftake 'mequal c (sub q qq)) rsub)))
    ;; gather substitutions of the form (X^Y)^(p/q) = cnst X^(p Y/q), where
    ;; p and q are integers.
     (setq xargs (gather-terms e 
        #'(lambda (q) (and (mexptp q) (mexptp (cadr q)) (not (freeof x q))))))
     (dolist (q xargs)
          (setq qq (let (($domain '$real)) ($expand q 0 0)))
          (when (not (alike1 q qq))
            (setq c (gensym))
            (push (ftake 'mequal q (mul c qq)) fsub)
            (push (ftake 'mequal c (div q qq)) rsub)))
    ;; return a list (fsub rsub), where fsub and rsub are Maxima lists.        
    (list (fapply 'mlist fsub)  (fapply 'mlist rsub))))

;; Alternative top-level integrate function that replaces constant subexpressions by
;; a generated constant, integrates, then replaces the generated constant by its
;; original expression. 
(defmfun $integrate (expr x &optional lo hi)
  (declare (special *in-risch-p*))
  (setq expr ($ratdisrep expr))
  (let (($ratfac nil) (cntx ($supcontext)) (ee) (subs))
    (unwind-protect
       (progn
        ($activate cntx)      
        (cond (hi ($defint expr x lo hi))
              (t
                 (setq subs (constant-subexpression-subs expr x))
                 (setq ee ($substitute (first subs) expr))
                 ;(setq ee (sratsimp ee))
                 ;(mtell "ee = ~M ; expr = ~M ~%" ee expr)
                
                 (cond ((member '%risch *nounl*) 
                          (if *in-risch-p*
                             ($funmake '%integrate (ftake 'mlist expr x)) ;give up.
                             ($substitute (second subs) (rischint ee x))))
                        (t ($substitute (second subs) (sinint ee x)))))))
      ($killcontext cntx))))