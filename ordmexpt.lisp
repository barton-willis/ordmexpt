#|
Error summary:
Error(s) found:
       rtest_gamma.mac problems:    (384 390)
       rtest_integrate.mac problems:
    (176 177 178 179 360 362 372 374 441 456 525 526 527 528 529 530 534 535
     537 538)
       rtest_trace.mac problems:    (87 88)
  rtest_to_poly_solve.mac problems:    (166 216)
Tests that were expected to fail but passed:
       rtest1.mac problem:    (183)
       rtestsum.mac problem:    (95)
       rtest_limit_extra.mac problem:    (259)
       rtest_hg.mac problem:    (87)
 rtest_romberg.mac problem:    (18)
  to_poly_solve/rtest_to_poly_solve.mac problem:    (322)
 rtest_raddenest.mac problem:    (123)
26 tests failed out of 18,817 total tests.
Evaluation took:
  359.598 seconds of real time
  341.656250 seconds of total run time (230.265625 user, 111.390625 system)
  [ Real times consist of 11.534 seconds GC time, and 348.064 seconds non-GC time. ]
  [ Run times consist of 11.0000 seconds GC time, and 330.657 seconds non-GC time. ]
  95.01% CPU
  347,839 forms interpreted
  347,937 lambdas converted
  717,830,316,259 processor cycles
  121,741,885,152 bytes consed
 |#

(defvar *ordmexpt* nil)
(defvar *old* nil)
(defun ordmexptx (x y)
  (let ((old (ordmexpt-old x y))
        (new (ordmexpt-new x y)))
    (when (not (eq old new))
      (push (ftake 'mlist x y old new) *ordmexpt*))
    (if *old* old new)))

(defun ordmexpt-old (x y)
  (cond ((eq (caar y) 'mexpt)
	 (cond ((alike1 (cadr x) (cadr y)) (great (caddr x) (caddr y)))
	       ((maxima-constantp (cadr x))
		(if (maxima-constantp (cadr y))
		    (if (or (alike1 (caddr x) (caddr y))
			    (and (mnump (caddr x)) (mnump (caddr y))))
			(great (cadr x) (cadr y))
			(great (caddr x) (caddr y)))
		    (great x (cadr y))))
	       ((maxima-constantp (cadr y)) (great (cadr x) y))
	       ((mnump (caddr x))
		(great (cadr x) (if (mnump (caddr y)) (cadr y) y)))
	       ((mnump (caddr y)) (great x (cadr y)))
	       (t (let ((x1 (simpln1 x)) (y1 (simpln1 y)))
		    (if (alike1 x1 y1) (great (cadr x) (cadr y))
			(great x1 y1))))))
	((alike1 (cadr x) y) (great (caddr x) 1))
	((mnump (caddr x)) (great (cadr x) y))
	(t (great (simpln1 x)
		  (ftake '%log y)))))

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
(defvar *bad* nil)
(defun ordmexpt-new (x y)
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
           ;; rules for great(%e^X, (=/= %e)^Y) = t
           ((eq base-x '$%e) t)
           ((eq base-y '$%e) nil)      
           ;; default: comparison between bases
           (t (great base-x base-y))))))))

(defvar $xxx)
(defmfun $report ()
  (setq $xxx (fapply 'mlist *ordmexpt*))
  (setq $xxx ($listify ($setify $xxx)))
  (mtell "Number of old/new differences:  ~M ~%" ($length $xxx))
  (mtell "Calls to ordmexpt = ~M ~%" *calls-to-ordmexpt*)
  (mtell "Return old value = ~M ~%" *old*)
  '$done)
