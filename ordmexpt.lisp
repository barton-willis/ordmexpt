#| Testing with SBCL and the hacked approx-alike function that attempts to allow some
   syntactic failures to pass. Some (likely most) of the ten unexpected successes are 
   due to the hacked approx-alike that allows a test to pass even when Maxima returns an
   unsimplified result. 
   
Error(s) found:
  rtest16.mac problem:    (21)
  rtest3.mac problems:    (67 68 69 74)
  rtest_gamma.mac problems:    (384 390)
  rtest_integrate.mac problems:    (176 177 178 179 360 362 372 374 441 456 525 526 
    527 528 529 530 534 535  537 538)
  rtest_trace.mac problems:    (87 88)
  rtest_to_poly_solve.mac problems:    (166 216)

Tests that were expected to fail but passed:
  rtest1.mac problem:    (183)
  rtest16.mac problems:    (525 526)
  rtestsum.mac problem:    (95)
  rtest_limit_extra.mac problem:    (259)
  rtest_hg.mac problem:    (87)
  rtest_fourier_elim.mac problem:    (149)
  rtest_romberg.mac problem:    (18)
  rtest_to_poly_solve.mac problem:    (322)
  rtest_raddenest.mac problem:    (123)

31 tests failed out of 18,957 total tests. |#

 
;; I now have additional fixes in 'sin.lisp', 'limit.lisp', and 'tlimit.lisp'.
;; Let's load these files:
;($load "sin.lisp")
;($load "limit.lisp")
;($load "tlimit.lisp")
($load "constant_subexpressions.lisp")
($load "approx-alike.lisp")
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
      ;; make %e^X > a^Y when a =/= %e.
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

(defvar *iii* 0)
(defun ordfna (e a)			; A is an atom
  "Predicate subroutine to function 'great'. Requires `e` to be a Maxima expression and
  `a` to be an atom."

  (incf *iii* 1)
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))

  ((and (constant a) (not (member (caar e) '(mplus mtimes mexpt))))
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

(defun ordfna (e a)  ; A is an atom
  "Predicate subroutine to function 'great'. Requires `e` to be a Maxima expression and `a` to be an atom."
  (incf *iii* 1)
  (cond
   ((numberp a)
    (or (not (eq (caar e) 'rat))
        (> (cadr e) (* (caddr e) a))))
   
   ((and (constant a) (not (member (caar e) '(mplus mtimes mexpt))))
    (not (member (caar e) '(rat bigfloat))))
   
   ((eq (caar e) 'mrat))  ; All MRATs succeed all atoms
   
   ((null (margs e)) nil)
   
   ((mexptp e) (ordmexpt e a))
   
   ((member (caar e) '(mplus mtimes))
    (let ((u (car (last e))))
      (cond ((eq u a) (not (ordhack e)))
            (t (great u a)))))
   
   ((eq (caar e) '%del))
   
   ((prog2 (setq e (car (margs e)))  ; Use first arg of e
           (and (not (atom e)) (member (caar e) '(mplus mtimes))))
    (let ((u (car (last e))))  ; Compare using the same procedure as above
      (cond ((eq u a) (not (ordhack e)))
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
