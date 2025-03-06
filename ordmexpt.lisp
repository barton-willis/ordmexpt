#| The hacked approx-alike function attempts to allow some purely syntactic test failures to 
   pass. This version of approx-alike is only intended to help with development of this code,
   it is not intended to be a replacement.

   Testing with SBCL and the hacked approx-alike function gives 31 failures
   and ten successes. Some (likely most) of the ten unexpected successes are 
   due to the hacked approx-alike that allows a test to pass even when Maxima 
   returns an unsimplified result.  Plus, this code causes one testsuite asksign.

Result from rtest_great:
┌                                                                         ┐
│ function          time/call            calls        runtime      gctime │
│                                                                         │
│  ordfna   2.6089600879452177e-6 sec  135153227  352.609375 sec     0    │
│                                                                         │
│ ordlist   6.052684462470317e-6 sec   52190169   315.890625 sec     0    │
│                                                                         │
│ ordmexpt  3.7398180794529003e-6 sec  92430131   345.671875 sec     0    │
│                                                                         │
│  great    3.9013668574613305e-6 sec  377027316  1470.921875 sec    0    │
│                                                                         │
│  total    3.7836336181438184e-6 sec  656800843  2485.09375 sec     0    │
└                                                                         ┘

... Which was correct.
38/38 tests passed  

Without ordmexpt:

Result:
┌                                                                        ┐
│ function         time/call            calls        runtime      gctime │
│                                                                        │
│  ordfna   2.816167760010712e-6 sec  186612196   525.53125 sec     0    │
│                                                                        │
│ ordlist   8.02527012099124e-6 sec   69133187    554.8125 sec      0    │
│                                                                        │
│ ordmexpt  8.384556558806245e-6 sec  68401426   573.515625 sec     0    │
│                                                                        │
│  great    4.959178382098201e-6 sec  470967002  2335.609375 sec    0    │
│                                                                        │
│  total    5.017481390472288e-6 sec  795113811  3989.46875 sec     0    │
└                                                                        ┘
0

... Which was correct.

30/38 tests passed

The following 8 problems failed: (7 11 14 17 19 28 31 32)


Running the full testsuite with ordmexpt and approx-alike

Error summary:
Error(s) found:
   rtest1.mac problem:    (185)
   rtest6b.mac problems:    (11 12)
   rtest7.mac problems:    (13 27 28 29)
   rtest9a.mac problem:    (12)
   rtest15.mac problem:    (7)
   rtest16.mac problems:    (21 780)
   rtestode.mac problem:    (86)
   rtest3.mac problems:    (180 182)
   rexamples.mac problem:    (122)
   rtest_allnummod.mac problems:    (397 398)
   rtest_integrate.mac problems:  (47 50 53 56 59 62 76 135 136 137 138 320 322 346 348 439 550)
   rtest_limit.mac problem:    (197)
   rtest_limit_extra.mac problems:    (116 260 336 337 338 339 343 347)
   rtest_unicode_display.mac problems:    (11 13)
   contrib/diffequations/rtest_odelin.mac problem:    (22)
   fourier_elim/rtest_fourier_elim.mac problems:    (104 110)
   contrib/bitwise/rtest_bitwise.mac problem:    (42)
   to_poly_solve/rtest_to_poly_solve.mac problems:    (166 216 313)
   vector/rtest_vect.mac problem:    (19)

Tests that were expected to fail but passed:
   rtest1.mac problem:    (183)
   rtest_limit_extra.mac problems:    (259 261)
   fourier_elim/rtest_fourier_elim.mac problem:    (149)
   numeric/rtest_romberg.mac problem:    (18)
   to_poly_solve/rtest_to_poly_solve.mac problem:    (322)
   raddenest/rtest_raddenest.mac problem:    (123)
53 tests failed out of 19,027 total tests.
|#

;($load "constant_subexpressions.lisp")
;($load "approx-alike.lisp")
;($load "nrat4.lisp")

;; This function is no longer used.
#| 
(defun my-constantp (e &optional (constants *builtin-numeric-constants*))
 "Return t iff every leaf of Maxima expression `e` is either a number, 
  a declared system constant, or in the CL list `constants`."
  (if ($mapatom e)
      (or (mnump e)
          (and (symbolp e) (get e 'sysconst)) ;catches nil, true, and $%i for example
          (member e constants :test #'eq))
      (every #'my-constantp (margs e))))
|#

;; Return great(x,y), where x is an `mexpt` expression and y is any Maxima
;; expression. 
(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be a `mexpt` expression; `y` may 
  or may not be a `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
  expression."
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be a mexpt expression, but y need not be a mexpt expression.
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
      ;; make %e^X > a^Y and %e^X > a when a =/= %e.
      ;((and (eq base-x '$%e) (not (eq base-y '$%e))) t)
      ;((and (eq base-y '$%e) (not (eq base-x '$%e))) nil)

      ;; default: comparison between bases
      (t (great base-x base-y)))))

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

(defun ordfna (e a)  ; A is an atom
  "Subroutine to function 'great'. Requires `e` to be a Maxima expression and `a` 
   to be an atom."
  (cond
   ((numberp a)
    (or (not (eq (caar e) 'rat))
        (> (cadr e) (* (caddr e) a))))
   
   ;; I am not sure about the (constant a)?
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

#|  This version causes some failures running rtest_great

;; An effort to simplify the logic of ordfna
(defun ordfna (e a)  
  "Subroutine to function 'great'. Requires `e` to be a Maxima expression and `a` 
   to be an atom."
  (cond ((mexptp e) 
           (ordmexpt e a))
        ((mnump a)
           (if (mnump e) (eq t (mgrp e a)) t))
        ((member (caar e) '(mplus mtimes))
           (ordlist (cdr e) (list a) (caar e) (caar e)))
      
        (t t)))
|#
;; This fix is needed for integrate(exp(acsc(x)),x) with domain : complex. Thanks to David Scherfgen
;; for finding and fixing this bug.
(defun islinear (expr var1)
  (let ((a (let ((*islinp* t))
             (sdiff expr var1))))
    (if (freeof var1 a)
      (let ((b (no-err-sub-var 0 expr var1)))
       (if (eq b t) nil
        (cons a (maxima-substitute 0 var1 expr)))))))
 
 