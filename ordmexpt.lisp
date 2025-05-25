#| The hacked approx-alike function attempts to allow some purely syntactic test failures to 
   pass. This version of approx-alike is only intended to help with development of this code,
   it is not intended to be a replacement for approx-alike.

Error(s) found:
  rtest16.mac problems:    (21 780)
  rtest3.mac problems:    (67 68 69 74)
  rtest_integrate.mac problems:    (360 362 372 374 441 456 525 526 527 528 529 530 534 535 537 538)
  rtest_trace.mac problems:    (87 88)
  rtest_to_poly_solve.mac problems:  (166 216)
Tests that were expected to fail but passed:
  rtest1.mac problems:   (183 186)
  rtest_limit_extra.mac problem:  (259)
  rtest_fourier_elim.mac problem:  (149)
  rtest_romberg.mac problem:  (18)
  rtest_to_poly_solve.mac problem:  (322)
  rtest_raddenest.mac problem: (123)
26 tests failed out of 19,154 total tests.

All test in rtest_great pass: timings:

┌                                                                         ┐
│ function          time/call            calls        runtime      gctime │
│                                                                         │
│  ordfna   2.402409067168569e-6 sec   119730078  287.640625 sec     0    │
│                                                                         │
│ ordlist   5.563747453519275e-6 sec   52168076     290.25 sec       0    │
│                                                                         │
│ ordmexpt  2.6806849360904043e-6 sec  91930796    246.4375 sec      0    │
│                                                                         │
│  great    3.653463862676877e-6 sec   343231635  1253.984375 sec    0    │
│                                                                         │
│  total    3.423566858652172e-6 sec   607060585   2078.3125 sec     0    │
└                                                                         ┘
Standard Maxima--eight tests fail and

┌                                                                         ┐
│ function          time/call            calls        runtime      gctime │
│                                                                         │
│  ordfna   2.5787938050951397e-6 sec  186612196  481.234375 sec     0    │
│                                                                         │
│ ordlist   7.876779570425417e-6 sec   69133187   544.546875 sec     0    │
│                                                                         │
│ ordmexpt   8.37130756601478e-6 sec   68401426   572.609375 sec     0    │
│                                                                         │
│  great    4.813865653797971e-6 sec   470967002  2267.171875 sec    0    │
│                                                                         │
│  total    4.861646781280724e-6 sec   795113811   3865.5625 sec     0    │
└                                                                         ┘

Possibly, my ordlist function is a bit faster than the standard, and plus it is called about
17 million fewer times.
|#

;($load "constant_subexpressions.lisp")
($load "approx-alike.lisp")
($load "tlimit.lisp")
($load "limit.lisp")
($load "sin.lisp")
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
      ((and (eq base-x '$%e) (not (eq base-y '$%e))) t)
      ((and (eq base-y '$%e) (not (eq base-x '$%e))) nil)

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
   
#| 
;; An attempt to clarify and simplify (at least to me) the logic of ordfna. This version of
;; ordfna fails the rtest_great test.
(defun ordfna-xxx (e a)  ; A is an atom
  "Subroutine to function 'great'. Requires `e` to be a non atom and `a` to be an atom."
  (cond
   ((mnump e) ; either `e` is a rational number or a bigfloat
    (if (mnump a) 
          (eq t (mgrp e a)) ;both `e` & `a` are numbers, so compare using mgrp
          nil)) ;`e` is a number and `a` is not, return nill (numbers before symbols)

   (($subvarp e) t) ; subvarp > atom 

   ((mexptp e) (ordmexpt e a))

   ((mplusp e)
      (if (null (cdr e)) (great 0 a) (great (car (last e)) a)))

   ((mtimesp e)
      (if (null (cdr e)) (great 1 a) (great (car (last e)) a)))

   ;((null (margs e)) nil)
   
   (t
      (setq e (car (margs e))) 
      (cond ((eq e a) t)
            (t (great e a))))))


;; This fix is needed for integrate(exp(acsc(x)),x) with domain : complex. Thanks to David Scherfgen
;; for finding and fixing this bug.
(defun islinear (expr var1)
  (let ((a (let ((*islinp* t))
             (sdiff expr var1))))
    (if (freeof var1 a)
      (let ((b (no-err-sub-var 0 expr var1)))
       (if (eq b t) nil
        (cons a (maxima-substitute 0 var1 expr)))))))
 
 |#