;; This is version of approx-alike that applies `resimplify` and `ratsimp` to each 
;; input at the top level. This version helps find testsuite failures that are 
;; purely syntatic. It is *not* intended to be a replacment to the standard 
;; version of approx-alike.

;; If a test fails using the standard approx-alike and passes using this version, 
;; it might mean that Maxima is incorrectly returing an unsimplified result.
(defun approx-alike (f g)
    (or (approx-alike-h f g)
        (approx-alike-h (resimplify f) (resimplify g))
        (let (($keepfloat t)) 
               (approx-alike-h (sratsimp (resimplify f)) (sratsimp (resimplify g))))))

(defun approx-alike-h (f g)
  
  (cond ((floatp f) (and (floatp g) ($float_approx_equal f g)))
	
	(($bfloatp f) (and ($bfloatp g) ($bfloat_approx_equal f g)))
	
	(($taylorp g)
	 (approx-alike 0 (sub (ratdisrep f) (ratdisrep g))))
	
	((stringp f)
	 (and (stringp g) (string= f g)))

	((arrayp f)
	 (and (arrayp g)
          (equal (array-dimensions f) (array-dimensions g))
          (approx-alike ($listarray f) ($listarray g))))

	((hash-table-p f)
	 (and (hash-table-p g) (approx-alike ($listarray f) ($listarray g))))

	((atom f)
	 (and (atom g) (equal f g)))
		     
	((op-equalp f 'lambda)
	 (and (op-equalp g 'lambda)
	      (approx-alike-list (mapcar #'(lambda (s) (simplifya s nil)) (margs f))
				 (mapcar #'(lambda (s) (simplifya s nil)) (margs g)))))
	
	(($ratp f)
	 (and ($ratp g) (approx-alike (ratdisrep f) (ratdisrep g))))
	
	;; maybe we don't want this.
	((op-equalp f 'mquote)
	 (approx-alike (second f) g))
	 
	;; I'm pretty sure that (mop f) and (mop g) won't signal errors, but
	;; let's be extra careful.

	((and (consp f) (consp (car f)) (consp g) (consp (car g))
	      (or (approx-alike (mop f) (mop g)) 
		  (and (symbolp (mop f)) (symbolp (mop g))
		       (approx-alike ($nounify (mop f)) ($nounify (mop g)))))
	      (eq ($subvarp f) ($subvarp g))
	      (approx-alike-list (margs f) (margs g))))
	
	(t nil)))