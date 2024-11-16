#| Using SBCL & running testsuite and the share testsuite:

Error(s) found:
   rtest7.mac problems:    (27 28 29)
   rtest9a.mac problem:    (12)
   rtest15.mac problem:    (7)
   rtestode.mac problem:    (86)
   rtest_gamma.mac problems:    (384 390)
   rtest_integrate.mac problems:
    (47 50 53 56 59 62 76 135 136 137 138 320 322 346 348 439 550)
   rtest_limit.mac problems:    (71 73)
   rtest_limit_extra.mac problem:    (116)
  rtest_to_poly_solve.mac problems:     (166 216)
Tests that were expected to fail but passed:
   rtest1.mac problem:    (183)
   rtestsum.mac problem:    (95)
   rtest_limit_extra.mac problem:    (259)
   rtest_hg.mac problem:    (87)
   fourier_elim/rtest_fourier_elim.mac problem:    (149)
   rtest_romberg.mac problem:    (18)
   rtest_to_poly_solve.mac problem:    (322)
   rtest_raddenest.mac problem:   (123)

30 tests failed out of 18,886 total tests.
Evaluation took:
  356.769 seconds of real time
  328.437500 seconds of total run time (226.609375 user, 101.828125 system)
  [ Real times consist of 12.146 seconds GC time, and 344.623 seconds non-GC time. ]
  [ Run times consist of 11.437 seconds GC time, and 317.001 seconds non-GC time. ]
  92.06% CPU
  347,835 forms interpreted
  347,939 lambdas converted
  712,179,189,850 processor cycles
  124,083,954,800 bytes consed

  rtest_shame:

  The following 4 problems failed: (1 3 11 12)


rtest_great: (all tests pass)


 |#

(defun ordmexpt-999 (x y)
      (let ((old (ordmexpt-old x y))
            (new (ordmexpt-new x y)))

      ;(when (not (alike1 old new))
       ;     (mtell "x = ~M ; y = ~M ; old = ~M ; new = ~M  ~%" x y old new))
      new))

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
  a declared system constant, or in the CL list `constants`."
  (if ($mapatom e)
      (or (mnump e)
          (and (symbolp e) (get e 'sysconst)) ;catches nil, true, and $%i for example
          (member e constants :test #'eq))
      (every #'my-constantp (margs e))))

;; Return great(x,y), where x is an mexpt expression and y is any Maxima
;; expression. 

(defun ordmexpt (x y)
  "Subroutine to function 'great'. Requires `x` to be and `mexpt` expression; `y` may 
  or may not be an `mexpt` expression and `y` is *not* an `mplus` or `mtimes` 
  expression."
  ;; Decompose both x & y as x = base-x^exp-x & y = base-y^exp-y. The input x is 
  ;; required to be an mexpt expression, but y need not be an mexpt expression.
  (let ((base-x (second x)) (exp-x (third x))
        (base-y (if (mexptp y) (second y) y))
        (exp-y (if (mexptp y) (third y) 1)))
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


(defun ordfna (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
           
      ((and (constant a)
              (not (member (caar e) '(mplus mtimes mexpt))))
              
	 (not (member (caar e) '(rat bigfloat))))
       
	((eq (caar e) 'mrat)) ;; all MRATs succeed all atoms
	((null (margs e)) nil)

      ((eq (caar e) 'mexpt) (ordmexpt e a))

	((member (caar e) '(mplus mtimes) :test #'eq)
        (ordlist (cdr e) (list a) (caar e) (caar e)))

	((eq (caar e) '%del))
      
	((prog2 (setq e (car (margs e)))	; use first arg of e
	     (and (not (atom e)) (member (caar e) '(mplus mtimes))))
	 (let ((u (car (last e))))		; and compare using 
	   (cond ((eq u a) (not (ordhack e)))	; same procedure as above
		 (t (great u a)))))
	((eq e a))
	(t (great e a))))

