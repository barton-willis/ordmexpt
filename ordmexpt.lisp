(defun my-constantp (e)
  (cond (($mapatom e)
           (or (mnump e)
               (eq e '$%i)
               (member e *builtin-numeric-constants*)))
        (t (every #'my-constantp (margs e)))))

;; Return great(x,y), where x is an mexpt expression and y is any Maxima
;; expression.
(defun ordmexpt (x y)
   ;(let ((*standard-output* *debug-io*)) 
   ;  (mtell "x = ~M ; y = ~M ~%" x y))
  (let ((base-x (second x))
        (exp-x (third x))
        (base-y (if (mexptp y) (second y) y))
        (exp-y (if (mexptp y) (third y) 1)))
    (cond
      ;; Bases are alike, compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; great(exponential, mapatom) = t
      (($mapatom y) t)
      (t
       (let ((x-const (my-constantp x))
             (y-const (my-constantp y)))
         (cond
           ;; great(non-constant, constant) = t
           ((and (not x-const) y-const) t)
           ;; great(constant, non-constant) = nil
           ((and (not y-const) x-const) nil)
           ;; Special case for base %e 
           ((eq base-x '$%e) t)
           ((eq base-y '$%e) nil)
           ;; Compare the bases if none of the above cases match
           (t (great base-x base-y))))))))