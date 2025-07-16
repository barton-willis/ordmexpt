#| The hacked approx-alike function attempts to allow some purely syntactic test failures to 
   pass. This version of approx-alike is only intended to help with development of this code,
   it is not intended to be a replacement for approx-alike.

|#
;($load "approx-alike.lisp")

;; This function is no longer used.
(defun my-constantp (e &optional (constants *builtin-numeric-constants*))
 "Return t iff every leaf of Maxima expression `e` is either a number, 
  a declared system constant, or in the CL list `constants`."
  (if ($mapatom e)
      (or (mnump e)
          (and (symbolp e) (get e 'sysconst)) ;catches nil, true, and $%i for example
          (member e constants :test #'eq))
      (every #'my-constantp (margs e))))

;; Return great(x,y), where x is an `mexpt` expression and y is any Maxima expression. 
(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be a `mexpt` expression; `y` may 
  or may not be a `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
  expression."
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be a mexpt expression, but y need not be a mexpt expression.
  (let ((base-x (second x)) (exp-x (third x)))
     (multiple-value-bind (base-y exp-y)
        (if (mexptp y)
            (values (second y) (third y))
            (values y 1))
    (cond 
      ;; bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; default: comparison between bases
      (t (great base-x base-y))))))

;; Arguably, this version of `ordlist` is more tidy than is the standard version.
;; And I have evidence that it is more efficient than the standard version.

;; Using 'great', compare the CL lists a and b element wise in reverse order. 
;; For unequal list lengths, the arguments ida and idb give default values
;; for comparison. When ida or idb is 'mplus', compare to zero, otherwise
;; compare to one.
(defun ordlist (a b ida idb)
  "Subroutine to function 'great'. Using 'great', compare two lists of expressions
  `a` and `b` in reverse order, using `ida` and `idb` as default values for missing 
   elements."
  ;; Reverse lists a and b
  (setq a (reverse a)
        b (reverse b))
  ;; Set ida and idb based on their initial values
  (setq ida (if (eq ida 'mplus) 0 1)
        idb (if (eq idb 'mplus) 0 1))

  ;; Iterate through lists a and b
  (catch 'terminate
    (while (or a b)
      (cond 
       ;; Case when a is null
       ((null a) 
         (throw 'terminate (great ida (if (null b) idb (car b)))))
       ;; Case when b is null
       ((null b) 
        (throw 'terminate (great (car a) idb)))
       ;; Case when heads of a and b are alike
       ((alike1 (car a) (car b)) 
        (setq a (cdr a)
              b (cdr b)))
       ;; Default case: compare heads of a and b
       (t (throw 'terminate (great (car a) (car b))))))))
   
(defun ordfna (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
        ((and (not (member (caar e) '(mplus mtimes mexpt)))
              (constant a))
	 (not (member (caar e) '(rat bigfloat))))
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)
	((eq (caar e) 'mexpt) (ordmexpt e a))
	((member (caar e) '(mplus mtimes))
	 (let ((u (car (last e))))
	   (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
	((eq (caar e) '%del))
	((prog2 (setq e (car (margs e)))	; use first arg of e
	     (and (not (atom e)) (member (caar e) '(mplus mtimes))))
	 (let ((u (car (last e))))		; and compare using 
	   (cond ((eq u a) (not (ordhack e)))	; same procedure as above
		 (t (great u a)))))
	((eq e a))
	(t (great e a))))
