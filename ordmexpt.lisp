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
      ;; Case I: bases are alike; compare exponents
      ((alike1 base-x base-y)
       (great exp-x exp-y))

      ;((not (mexptp y)) nil)

      ;; Case V: default: comparison between bases
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

