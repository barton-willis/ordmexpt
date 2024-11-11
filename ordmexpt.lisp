#|
Error summary:
Error(s) found:
     /rtest14.mac problem:    (80)
     /rtestode.mac problem:    (64)
     /rtest3.mac problems:    (66 67 68 69 71 74)
     /rtest_hypgeo.mac problem:    (71)
     /rtestint.mac problems:    (197 198 199 200 201 306)
     /rtest_gamma.mac problem:    (330)
     /rtest_integrate.mac problems:
    (41 42 43 44 95 97 360 362 372 374 404 414 416 441 456 525 526 527 528 529 530 534 535 537 538)
     /rtest_powerseries.mac problems:    (45 48)
     /rtest_trace.mac problem:    (87)
     /rtest_unicode_display.mac problems:    (77 90 91 92 93)
Tests that were expected to fail but passed:
     /rtest1.mac problem:    (183)
     /rtest_limit_extra.mac problem:    (259)
49 tests failed out of 14,023 total tests.
(TESTSUITE)
took 213,331,000 microseconds (213.331000 seconds) to run.
       4,194,441 microseconds (  4.194441 seconds, 1.97%) of which was spent in GC.
During that period, and with 16 available CPU cores,
      45,468,750 microseconds ( 45.468750 seconds) were spent in user mode
      22,500,000 microseconds ( 22.500000 seconds) were spent in system mode
 46,014,970,016 bytes of memory allocated.

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

;;Notes:

;; Case II: We define odermexpt(X, subscripted variable) = t. 
;; Without this rule, rtest_rules 207 & 208 fail. Possibly,
;; pattern matching is too sensitive to changes in ordering.

;; Case III Something like this is needed for rtest_limit 71 & 73.

;; Case  IV: Without the %cos %sin check, we get an error for 
;; block([domain : complex], integrate(exp(acsc(x)),x)). I would
;; like to replace this case with something more logical.

(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be and `mexpt` expression; `y` may 
  or may not be an `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
  expression."
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be an mexpt expression, but y need not be an mexpt expression.
  (let ((base-x (second x))
        (exp-x (third x))
        (base-y (if (mexptp y) (second y) y))
        (exp-y (if (mexptp y) (third y) 1)))
    (cond
      ;; Case I: bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))
      ;; Case II: Special case when y is a subscripted variable. 
      ;(($subvarp y) t)

      ;; Cases III-V
      (t
       (let ((x-const (my-constantp x))
             (y-const (my-constantp y)))
         (cond
           ;; Case III: non-constant x is greater than constant y.
           ((and (not x-const) y-const) t)
           ((and (not y-const) x-const) nil)

           ;; Case IV: rules for great(%e^X, Y).
          ; ((and (eq base-x '$%e) 
         ;        (not (and (consp y) (member (caar y) '(%cos %sin) :test #'eq))))
        ;   t)
         ;  ((and (eq base-y '$%e)
        ;         (not (and (consp x) (member (caar x) '(%cos %sin) :test #'eq))))
        ;    nil)

           ;; Case V: default: comparison between bases
           (t (great base-x base-y))))))))

;; Arguably, this version of `ordlist` is more tidy than is the standard version.
;; But this version (i) fixes no bugs (ii) is no more efficient. Thus, I'm not
;; proposing that this code replace the current `ordlist`.

;; Using 'great', compare the CL lists a and b elementwise in reverse order. 
;; For unequal list lengths, the arguments ida and idb give default values
;; for comparision. When ida or idb is 'mplus', compare to zero, othewise
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


(defvar *worry* nil)
(defun ordfna (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
        ((and (constant a)
              (not (member (caar e) '(mplus mtimes mexpt))))
	 (not (member (caar e) '(rat bigfloat))))
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)

      ;; Change from standard--now call ordmexpt directly
      ((eq (caar e) 'mexpt) (ordmexpt e a))

      ;; should we call ordfn instead for the next three cases? And why
      ;; the special cases for '%del? Finally what does the function with 
      ;; the sketchy name (ordhack) do? But ordfn requires that nither 
      ;; input be an atom.

      ((member (caar e) '(mplus mtimes %del) :test #'eq)
       (ordfn e a))

	;((member (caar e) '(mplus mtimes))
	; (let ((u (car (last e))))
	;   (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
	;((eq (caar e) '%del))
	((prog2 (setq e (car (margs e)))	; use first arg of e
	     (and (not (atom e)) (member (caar e) '(mplus mtimes))))
	 (let ((u (car (last e))))		; and compare using 
	   (cond ((eq u a) (not (ordhack e)))	; same procedure as above
		 (t (great u a)))))
	((eq e a))
	(t (great e a))))

;; one of the exprs x or y should be one of:
;; %del, mexpt, mplus, mtimes
(defun ordfn (x y)
  (let ((cx (caar x)) (cy (if (consp y) (caar y) 'mplus)))

    (cond ((eq cx '%del) (if (eq cy '%del) (great (cadr x) (cadr y)) t))
	  ((eq cy '%del) nil)
	  ((or (eq cx 'mtimes) (eq cy 'mtimes))
	   (ordlist (factor-list x) (factor-list y) 'mtimes 'mtimes))
	  ((or (eq cx 'mplus) (eq cy 'mplus))
	   (ordlist (term-list x) (term-list y) 'mplus 'mplus))
	  ((eq cx 'mexpt) (ordmexpt x y))
	  ((eq cy 'mexpt) (not (ordmexpt y x))))))