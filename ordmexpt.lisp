#| Testing with SBCL and the hacked approx-alike function that attempts to allow some
   syntactic failures to pass. Some of the 13 unexpected successes are likely due to
   the hacked approx-alike that allows a test to pass even when Maxima returns an
   unsimplified result. |#

 
;; I now have additional fixes in 'sin.lisp', 'limit.lisp', and 'tlimit.lisp'.
;; Let's load these files:
;($load "sin.lisp")
;($load "limit.lisp")
;($load "tlimit.lisp")

;; This function is no longer used.
(defun my-constantp (e &optional (constants *builtin-numeric-constants*))
 "Return t iff every leaf of Maxima expression `e` is either a number, 
  a declared system constant, or in the CL list `constants`."
  (if ($mapatom e)
      (or (mnump e)
          (and (symbolp e) (get e 'sysconst)) ;catches nil, true, and $%i for example
          (member e constants :test #'eq))
      (every #'my-constantp (margs e))))

;; Return great(x,y), where x is an `mexpt` expression and y is any Maxima
;; expression. 
(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be a `mexpt` expression; `y` may 
  or may not be a `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
  expression."
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be an mexpt expression, but y need not be an mexpt expression.
  (let ((base-x (second x)) (exp-x (third x)) (base-y) (exp-y))
    (if (mexptp y)
      (setq base-y (second y)
            exp-y (third y))
       (setq base-y y
            exp-y 1))
    (cond
      ;; experimental code that makes %e^X > a^Y when a =/= %e.
      ((and (eq base-x '$%e) (not (eq base-y '$%e))) t)
      ((and (eq base-y '$%e) (not (eq base-x '$%e))) nil)

      ;; bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; default: comparison between bases
      (t (great base-x base-y)))))

;; Arguably, this version of `ordlist` is more tidy than is the standard version.
;; But this version (i) fixes no bugs (ii) is no more efficient. Thus, I'm not
;; proposing that this code replace the current `ordlist`.

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
        (throw 'terminate (great ida (car b))))
       ;; Case when b is null
       ((null b) 
        (throw 'terminate (great (car a) idb)))
       ;; Case when heads of a and b are alike
       ((alike1 (car a) (car b)) 
        (setq a (cdr a)
              b (cdr b)))
       ;; Default case: compare heads of a and b
       (t (throw 'terminate (great (car a) (car b))))))))

(defun ordfna-xxx (e a)			; A is an atom
  (cond; ((numberp a)
	 ;(or (not (eq (caar e) 'rat))
	 ;    (> (cadr e) (* (caddr e) a))))
      
      ((mnump a)
         (if (mnump e) (eq t (mgrp e a)) t))

      ((and (my-constantp a)
           (consp e)
           (not (member (caar e) '(mplus mtimes mexpt) :test #'eq)))
           (not (member (caar e) '(rat bigfloat))))
       
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)

  ((eq (caar e) 'mexpt) (ordmexpt e a))

	((member (caar e) '(mplus mtimes) :test #'eq)
        (ordlist (cdr e) (list a) (caar e) (caar e)))

	((eq (caar e) '%del) t)
      
  (t
    (setq e (first (margs e)))
    (if (alike1 e a) t (great e a)))))

(defvar *ouch* nil)
(defun ordfna (e a)			; A is an atom
  "Predicate subroutine to function 'great'. Requires `e` to be any Maxima expression and
  `a` to be an atom."
  (when (not (atom a))
    (push a *ouch*))
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
        ((and (constant a)
              (not (member (caar e) '(mplus mtimes mexpt))))
	 (not (member (caar e) '(rat bigfloat))))
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)

  ((mexptp e) (ordmexpt e a))
	;((eq (caar e) 'mexpt)
	; (cond ((and (maxima-constantp (cadr e))
	;	     (or (not (constant a)) (not (maxima-constantp (caddr e)))))
	;	(or (not (free (caddr e) a)) (great (caddr e) a)))
	;       ((eq (cadr e) a) (great (caddr e) 1))
	;       (t (great (cadr e) a))))
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


(defun tlimit-taylor (e x pt n &optional (d 0))
	(let ((ee) 
	      (silent-taylor-flag t) 
	      ($taylordepth 8)
		  ($radexpand nil)
		  ($taylor_logexpand t)
		  ($logexpand t))
	    (setq ee 
		   (ratdisrep (catch 'taylor-catch
		                  (if (eq pt '$inf)      
				              ($taylor e (ftake 'mlist x pt n '$asym))
				              ($taylor e x pt n)))))
		
		(cond ((and ee (not (alike1 ee 0))) ee)
			  ;; Retry if taylor returns zero and depth is less than 16
              ((and ee (< d 16))
			    (tlimit-taylor e x pt (* 4 (max 1 n)) (1+ d)))
			  (t nil))))

(defmfun $integrate (expr x &optional lo hi)
  (declare (special *in-risch-p*))
  (let (($ratfac) (ans) (cntx ($supcontext)))
    (unwind-protect 
      (progn
         (cond (hi ($defint expr x lo hi))
               ((member '%risch *nounl*)
                  (if *in-risch-p*
                 ;; Give up; we're being called from RISCHINT by some path.
                  ($funmake '%integrate (ftake 'mlist expr x))
                  (rischint expr x)))
               (t
                 (setq ans (errcatch (sinint expr x)))
                 (if (eq ans nil) 
                     ($funmake '%integrate (ftake 'mlist expr x))
                     (car ans)))))
    ($killcontext cntx))))