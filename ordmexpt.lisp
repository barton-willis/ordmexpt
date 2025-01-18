#| The hacked approx-alike function attempts to allow some purely syntactic test failures to 
   pass. This version of approx-alike is only intended to help with development of this code,
   it is not intended to be a replacement.

   Testing with SBCL and the hacked approx-alike function gives 28 failures
   and ten successes. Some (likely most) of the ten unexpected successes are 
   due to the hacked approx-alike that allows a test to pass even when Maxima 
   returns an unsimplified result. 

Error(s) found:
   rtest7.mac problems:  (27 28 29)
   rtest9a.mac problem:  (12)
   rtest15.mac problem:   (7)
   rtestode.mac problem:   (86)
   rtest_gamma.mac problems:   (384 390)
   rtest_integrate.mac problems: (47 50 53 56 59 62 76 135 136 137 138 320 322 346 348 439 550)
   rtest_limit_extra.mac problem:  (116)
   rtest_to_poly_solve.mac problems: (166 216)

Tests that were expected to fail but passed:
   rtest1.mac problem:  (183)
   rtest16.mac problems:  (525 526)
   rtestsum.mac problem:  (95)
   rtest_limit_extra.mac problem:  (259)
   rtest_hg.mac problem:  (87)
    fourier_elim/rtest_fourier_elim.mac problem:  (149)
    numeric/rtest_romberg.mac problem:  (18)
    to_poly_solve/rtest_to_poly_solve.mac problem:  (322)
    raddenest/rtest_raddenest.mac problem:  (123)
28 tests failed out of 18,957 total tests.  

Without the hacked approx-alike function:
Error summary:
Error(s) found:
   rtest7.mac problems:    (27 28 29)
   rtest9.mac problem:    (54)
   rtest9a.mac problems:    (12 14)
   rtest14.mac problem    (50)
   rtest15.mac problems:    (7 247)
   rtestode.mac problem    (86)
   rtesthyp.mac problems:    (66 67 68 69 70 71 72 73 74 75 143)
   rtest_hypgeo.mac problems:    (83 86 132 136 156 158 159 162 175 176 211)
   rtestint.mac problems:    (193 197 198 199 200 201 217 304)
   rtest_gamma.mac problems:    (384 390)
   rtest_integrate.mac problems:
    (42 43 44 47 50 53 56 59 62 76 96 97 105 106 107 108 135 136 137 138 320
     322 340 346 348 351 354 399 402 439 550)
   rtest_powerseries.mac problem:   (46)
   rtest_laplace.mac problems:    (70 100)
   rtest_limit_extra.mac problem:    (116)
  rtest_to_poly_solve.mac problems:    (166 216)
  rtest_antid.mac problem:    (11)

Tests that were expected to fail but passed:
   rtest1.mac problem:    (183)
   rtest_limit_extra.mac problem:    (259)
80 tests failed out of 18,957 total tests. |#

;($load "constant_subexpressions.lisp")
;($load "approx-alike.lisp")
;($load "nrat4.lisp")
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
      ;((and (eq base-x '$%e) (not (eq base-y '$%e))) t)
      ;((and (eq base-y '$%e) (not (eq base-x '$%e))) nil)
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

(defun ordfna (e a)  ; A is an atom
  "Subroutine to function 'great'. Requires `e` to be a Maxima expression and `a` 
   to be an atom."
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
