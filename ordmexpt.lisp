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
  C:/Users/barto/maxima-code/tests/rtest_unicode_display.mac problems:
    (77 90 91 92 93)
Tests that were expected to fail but passed:
  C:/Users/barto/maxima-code/tests/rtest1.mac problem:
    (183)
  C:/Users/barto/maxima-code/tests/rtestsum.mac problem:
    (95)
  C:/Users/barto/maxima-code/tests/rtest_limit_extra.mac problem:
    (259)
  C:/Users/barto/maxima-code/tests/rtest_hg.mac problem:
    (87)
34 tests failed out of 13,954 total tests.
Evaluation took:
  273.612 seconds of real time
  233.296875 seconds of total run time (159.671875 user, 73.625000 system)
  [ Real times consist of 6.971 seconds GC time, and 266.641 seconds non-GC time. ]
  [ Run times consist of 6.640 seconds GC time, and 226.657 seconds non-GC time. ]
  85.27% CPU
  9,642 forms interpreted
  12,130 lambdas converted
  546,183,137,950 processor cycles
  44,154,900,736 bytes consed

  Calls to ordmextp 2,040,189.
 |#

;; Return t iff every leaf of the Maxima expression is either a number
;; a builtin numeric constant (for exmaple %pi or %e) or %i.
(defun my-constantp (e)
  (cond (($mapatom e)
           (or (mnump e)
               (eq e '$%i)
               (member e *builtin-numeric-constants*)))
        (t (every #'my-constantp (margs e)))))

;; Return great(x,y), where x is an mexpt expression and y is any Maxima
;; expression.

(defvar *debug-ordmexpt* nil)
(defvar *calls-to-ordmexpt* 0)
(defun ordmexpt (x y)
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
      ;; Bases are alike, compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; great(exponential, mapatom) = t (needed rtest_rules.mac problems 207 & 208)
      (($mapatom y) t)
      (t
       (let ((x-const (my-constantp x))
             (y-const (my-constantp y)))
         (cond
           ;; great(non-constant, constant) = t (needed for rtest_limit 71 & 73)
           ((and (not x-const) y-const) t)
           ;; great(constant, non-constant) = nil
           ((and (not y-const) x-const) nil)
           ;; Special case for base %e (needed for rtestode 86, rtest_limit_extra 116
           ;; and maybe more--without these cases, about 130 testsuite failures)
           ((eq base-x '$%e) t)
           ((eq base-y '$%e) nil)
           ;; Compare the bases if none of the above cases match
           (t (great base-x base-y))))))))