#| Testing with SBCL and the hacked approx-alike function that attempts to allow some
   syntactic failures to pass. Some of the 13 unexpected successes are likely due to
   the hacked approx-alike that allows a test to pass even when Maxima returns an
   unsimplified result.

Error summary:
Error(s) found:
   rtest7.mac problems:    (27 28 29)
   rtest9a.mac problem:    (12)
   rtest15.mac problem:    (7)
   rtestode.mac problem:    (86)
   rtest_gamma.mac problems:    (384 390)
   rtest_integrate.mac problems:
    (47 50 53 56 59 62 76 135 136 137 138 320 322 346 348 439 550)
   rtest_limit_extra.mac problems:    (57 100 188)
   rtest_to_poly_solve.mac problems:
    (114 166 216)
    
Tests that were expected to fail but passed:
   rtest1.mac problem:    (183)
   rtest16.mac problems:    (525 526)
   rtestsum.mac problem:    (95)
   rtest_limit.mac problem:    (231)
   rtest_limit_extra.mac problems:    (94 259)
   rtest_limit_gruntz.mac problem:    (97)
   rtest_hg.mac problem:    (87)
   rtest_fourier_elim.mac problem:    (149)
  rtest_romberg.mac problem:    (18)
  rtest_to_poly_solve.mac problem:    (322)
  rtest_raddenest.mac problem:    (123)
31 tests failed out of 18,909 total tests.
Evaluation took:
  354.791 seconds of real time
  328.156250 seconds of total run time (224.750000 user, 103.406250 system)
  [ Real times consist of 13.108 seconds GC time, and 341.683 seconds non-GC time. ]
  [ Run times consist of 11.890 seconds GC time, and 316.267 seconds non-GC time. ]
  92.49% CPU
  347,665 forms interpreted
  348,016 lambdas converted
  708,231,714,625 processor cycles
  124,253,183,504 bytes consed

|#

;; I now have additional fixes in 'sin.lisp', 'limit.lisp', and 'tlimit.lisp'.
;; Let's load these files:
($load "sin.lisp")
($load "limit.lisp")
($load "tlimit.lisp")

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
  "Subroutine to function 'great'. Requires `x` to be and `mexpt` expression; `y` may 
  or may not be an `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
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
      ;; bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; default: comparison between bases
      (t (great base-x base-y)))))

;; Arguably, this version of `ordlist` is more tidy than is the standard version.
;; But this version (i) fixes no bugs (ii) is no more efficient. Thus, I'm not
;; proposing that this code replace the current `ordlist`.

;; Using 'great', compare the CL lists a and b elementwise in reverse order. 
;; For unequal list lengths, the arguments ida and idb give default values
;; for comparison. When ida or idb is 'mplus', compare to zero, otherwise
;; compare to one.
(defun ordlist (a b ida idb)
  "Subroutine to function 'great'. Using 'great', compare two lists of expressions `a` and `b` in reverse order, 
   using `ida` and `idb` as default values for missing elements."
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

(defun ordfna (e a)			; A is an atom
  (cond; ((numberp a)
	 ;(or (not (eq (caar e) 'rat))
	 ;    (> (cadr e) (* (caddr e) a))))

      ((mnump a)
         (if (mnump e) (eq t (mgrp e a)) t))

      ((and nil (constant a)
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