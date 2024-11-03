#| Using the extended approx-alike: (there is an asksign that accounts for part
of the slowness of running the testsuite)

Error summary:
Error(s) found:
  C:/Users/barto/maxima-code/tests/rtest3.mac problems:
    (67 68 69 71 74)
  C:/Users/barto/maxima-code/tests/rtest_gamma.mac problems:
    (384 390)
  C:/Users/barto/maxima-code/tests/rtest_integrate.mac problems:
    (176 177 178 179 360 362 372 374 441 456 525 526 527 528 529 530 534 535
     537 538)
  C:/Users/barto/maxima-code/tests/rtest_trace.mac problems:
    (87 88)
  C:/Users/barto/maxima-code/share/to_poly_solve/rtest_to_poly_solve.mac problems:
    (166 216)
Tests that were expected to fail but passed:
  C:/Users/barto/maxima-code/tests/rtest1.mac problem:
    (183)
  C:/Users/barto/maxima-code/tests/rtestsum.mac problem:
    (95)
  C:/Users/barto/maxima-code/tests/rtest_limit_extra.mac problem:
    (259)
  C:/Users/barto/maxima-code/tests/rtest_hg.mac problem:
    (87)
  C:/Users/barto/maxima-code/share/numeric/rtest_romberg.mac problem:
    (18)
  C:/Users/barto/maxima-code/share/to_poly_solve/rtest_to_poly_solve.mac problem:
    (322)
  C:/Users/barto/maxima-code/share/raddenest/rtest_raddenest.mac problem:
    (123)
31 tests failed out of 18,817 total tests.
Evaluation took:
  705.770 seconds of real time
  403.109375 seconds of total run time (264.562500 user, 138.546875 system)
  [ Real times consist of 13.359 seconds GC time, and 692.411 seconds non-GC time. ]
  [ Run times consist of 13.203 seconds GC time, and 389.907 seconds non-GC time. ]
  57.12% CPU
  347,839 forms interpreted
  347,941 lambdas converted
  1,408,853,402,540 processor cycles
  120,653,951,936 bytes consed

  calls to ordmexpt: 6,435,472
 |#

(defun my-constantp (e &optional (constants *builtin-numeric-constants*))
 "Return t if every leaf of Maxima expression `e` is either a number, 
  the imaginary unit %i, or in `constants`."
  (if ($mapatom e)
      (or (mnump e)
          (eq e '$%i)
          (member e constants :test #'eq))
      (every #'my-constantp (margs e))))
;; Return great(x,y), where x is an mexpt expression and y is any Maxima
;; expression.

(defvar *debug-ordmexpt* nil)
(defvar *calls-to-ordmexpt* 0)
(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be in `mexpt` form; `y` may 
  or may not be an `mexpt` expression."
  
    (incf *calls-to-ordmexpt* 1)
    (when *debug-ordmexpt*   
      (let ((*standard-output* *debug-io*)) 
         (mtell "x = ~M ; y = ~M ~%" x y)))

  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be an mexpt expression, but y need not be an mexpt expression.
  (let ((base-x (second x))
        (exp-x (third x))
        (base-y (if (mexptp y) (second y) y))
        (exp-y (if (mexptp y) (third y) 1)))
    (cond
      ;; Bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; If base of x is an integer and y is atomic, return true
      ;; This case is needed rtest_rules.mac problems 207 & 208)
      ((and (integerp base-x) ($mapatom y)) t)
      (t
       (let ((x-const (my-constantp x))
             (y-const (my-constantp y)))
         (cond
           ;; non-constant x is greater than constant y (needed for rtest_limit 71 & 73)
           ((and (not x-const) y-const) t)
           ;; constant x is not greater than non-constant y
           ((and (not y-const) x-const) nil)
           ;; special case for base %e (needed for rtestode 86, rtest_limit_extra 116
           ;; and maybe more--without these cases, about 130 testsuite failures)
           ((eq base-x '$%e) t)
           ((eq base-y '$%e) nil)
           ;; default: comparison between bases
           (t (great base-x base-y))))))))