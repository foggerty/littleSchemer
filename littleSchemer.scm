;;;; The Little Schemer.
;;;; Third attempt :-)

;;; ========== Chapter the first. ==========

;;; Both of these are sexps
1					; atom
'(1 2 3)				; list
'()					; list

;;; CONS adds a sexp to the front of a list.
(cons 1 '(2 3))				; (1 2 3)
(cons 1 '())				; (1)

(define (atom? a)
  (and (not (null? a))
		 (not (pair? a))))

;;; ========== Chapter the second. ==========

(define (lat? lat)
  "T if lat is a list of atoms.  No error checking, expects a
   non-empty list."
  (cond ((null? lat) #t)
		  ((atom? (car lat))
			(lat? (cdr lat)))
		  (else #f)))

(define (member? a lat)
  "T if a is a member of lat (list of atoms)."
  (cond ((null? lat) #f)
		  ((eq? a (car lat)) #t)
		  (else (member? a (cdr lat)))))

;;; ========== Chapter the third. ==========

(define (rember a lat)
  "Removes all instances of a from LAT.  No error checking, expects a
list of atoms."
  (cond ((null? lat) '())
		  ((eq? a (car lat))
			(rember a (cdr lat)))
		  (else
			(cons (car lat)
					(rember a (cdr lat))))))

(define (firsts ll)
  "Returns the first s-expression of each list in a list of lists."
  (if (null? ll)
      '()
      (cons (caar ll)
				(firsts (cdr ll)))))

(define (insertR new old lat)
  "Inserts new into lat after every occurrence of old."
  (cond ((null? lat) '())
		  ((eq? old (car lat))
			(cons (car lat)
					(cons new (insertR new old (cdr lat)))))
		  (else
			(cons (car lat)
					(insertR new old (cdr lat))))))

(define (insertL new old lat)
  "Inserts new before each occurrence of old."
  (cond ((null? lat) '())
		  ((eq? old (car lat))
			(cons new
					(cons old (insertL new old (cdr lat)))))
		  (else
			(cons (car lat)
					(insertL new old (cdr lat))))))

(define (subst new old lat)
  "Replaces every instance of old with new in lat."
  (cond ((null? lat) '())
		  ((eq? old (car lat))
			(cons new (subst new old (cdr lat))))
		  (else
			(cons (car lat)
					(subst new old (cdr lat))))))

;;; ========== Chapter the fourth ==========

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(define (add a b)
  "The result of adding a and b."
  (if (zero? b)
      a
      (add1 (add a (sub1 b)))))

(define (sub a b)
  "The result of subtracting b from a."
  (if (zero? b)
      a
      (sub1 (sub a (sub1 b)))))

;; Note that here, 'tup' means an empty list or a list of numbers.
;; Also, we're only considering +ve numbers here.

(define (addtup tup)
  "Adds all numbers in tup."
  (if (null? tup)
      0
      (+ (car tup) (addtup (cdr tup)))))

(define (mult a b)
  "Multiplies a & b."
  (if (zero? b)
      0
      (+ a (mult a (sub1 b)))))

(define (add-tup tup1 tup2)
  "Adds two tups."
  (cond ((null? tup1) tup2)
		  ((null? tup2) tup1)
		  (else
			(cons (+ (car tup1)
						(car tup2))
					(add-tup (cdr tup1) (cdr tup2))))))

(define (gt-than? a b)
  "True if b is greater than b.  No negatives, please."
  (cond ((zero? a) #f)
		  ((zero? b) #t)
		  (else
			(gt-than? (sub1 a) (sub1 b)))))

(define (le-than? a b)
  "True if a is less than b.  No negatives allowed."
  (cond ((zero? b) #f)
		  ((zero? a) #t)
		  (else
			(le-than? (sub1 a) (sub1 b)))))

(define (power a b)
  "Raises a to the power of b."
  (if (zero? b)
      1
      (mult a (power a (sub1 b)))))

(define (lt-or-eq? a b)
  "T if a is less than or equal to b."
  (or (le-than? a b)
		(eq? a b)))

(define (divide a b)
  "Integer result of dividing b into a.  Don't even ask about error checking."
  (if (lt-or-eq? a 0)
		0
		(+ 1 (divide (sub a b) b))))

(define (length lat)
  "Returns length of a list of atoms."
  (if (null? lat)
		0
		(+ 1 (length (cdr lat)))))

(define (pick n lat)
  "Returns n'th atom in lat, index starts at 1."
  (cond ((< n 0) '())
		  ((null? lat) '())
		  ((eq? 1 n) (car lat))
		  (else
			(pick (sub n 1)
					(cdr lat)))))

(define (one? n)
  (= 1 n))

(define (rempick n lat)
  "Returns lat with the n'th element removed."
  (cond ((lt-or-eq? n 0) lat)
		  ((null? lat) '())
		  ((one? n) (cdr lat))
		  (else
			(cons (car lat)
					(rempick (- n 1)
								(cdr lat))))))

(define (no-nums lat)
  "Removes all numbers from a list of atoms."
  (cond ((null? lat) '())
		  ((number? (car lat)) (no-nums (cdr lat)))
		  (else
			(cons (car lat)
					(no-nums (cdr lat))))))

(define (all-nums lat)
  "Returns all numbers from lat."
  (cond ((null? lat) '())
		  ((number? (car lat))
			(cons (car lat) (all-nums (cdr lat))))
		  (else
			(all-nums (cdr lat)))))

(define (occur a lat)
  "The number of times that a appears in lat."
  (cond ((null? lat) 0)
		  ((eq? a (car lat))
			(+ 1 (occur a (cdr lat))))
		  (else
			(occur a (cdr lat)))))

;;; ========== Chapter the Fifth ==========

(define (rember* a lat)
  "Recursively remove all occurrences of a from lat."
  (cond ((null? lat) '())
		  ((atom? (car lat))
			(cond ((eq? a (car lat))
					 (rember* a (cdr lat)))
					(else
					 (cons (car lat)
							 (rember* a (cdr lat))))))
		  (else
			(cons (rember* a (car lat))
					(rember* a (cdr lat))))))

(define (insertR* new old lst)
  "Recursively insert new to the right of old."
  (cond ((null? lst) '())
		  ((atom? (car lst))
			(cond ((eq? old (car lst))
					 (cons old (cons new (insertR* new old (cdr lst)))))
					(else
					 (cons (car lst)
							 (insertR* new old (cdr lst))))))
		  (else
			(cons (insertR* new old (car lst))
					(insertR* new old (cdr lst))))))

(define (insertL* new old lst)
  "Recursively insert new to the left of old."
  (cond ((null? lst) '())
		  ((atom? (car lst))
			(cond ((eq? old (car lst))
					 (cons new (cons old (insertL* new old (cdr lst)))))
					(else
					 (cons (car lst)
							 (insertL* new old (cdr lst))))))
		  (else
			(cons (insertL* new old (car lst))
					(insertL* new old (cdr lst))))))

(define (occur* a lst)
  "Recursively counts the number of times that a occurs in lst."
  (cond ((null? lst) 0)
		  ((atom? (car lst))
			(cond ((eq? a (car lst))
					 (+ 1 (occur* a (cdr lst))))
					(else
					 (occur* a (cdr lst)))))
		  (else
			(+ (occur* a (car lst))
				(occur* a (cdr lst))))))

(define (subst* new old lst)
  "Recursively substitutes new for old in lst."
  (cond ((null? lst) '())
		  ((atom? (car lst))
			(cond ((eq? old (car lst))
					 (cons new (subst* new old (cdr lst))))
					(else
					 (cons (car lst) (subst* new old (cdr lst))))))
		  (else
			(cons (subst* new old (car lst))
					(subst* new old (cdr lst))))))

(define (member* a lst)
  "T if a is a member of lst."
  (cond ((null? lst) #f)
		  ((atom? (car lst))
			(or (eq? a (car lst))
				 (member* a (cdr lst))))
		  (else
			(or (member* a (car lst))
				 (member* a (cdr lst))))))

(define (leftmost lst)
  "Returns leftmost atom (if there is one) is a list of lists."
  (cond ((null? lst) '())
		  ((atom? (car lst)) (car lst))
		  (else
			(leftmost (car lst)))))

;; Note that for learning purposes, we're pretending that the built-in
;; function eq? only works with atoms.  Hence writing equal? below,
;; which does exactly the same thing (only less performant).

(define (eqlist? l1 l2)
  "Recursively determine if l1 and l2 are the equivalent."
  (cond ((and (null? l1) (null? l2)) #t)
		  ((or  (null? l1) (null? l2)) #f)
		  ((and (atom? (car l1)) (atom? (car l2)))
			(and (eq? (car l1) (car l2))
				  (eqlist? (cdr l1) (cdr l2))))
		  ((or (atom? (car l1)) (atom? (car l2))) #f)
		  (else
			(and (eqlist? (car l1) (car l2))
				  (eqlist? (cdr l1) (cdr l2))))))

(define (equal? a b)
  "T if a and b are the s-expression (i.e. an atom or a list)."
  (cond ((and (atom? a) (atom? b))
			(eq? a b))
		  ((or (atom? a) (atom? b))
			#f)
		  (else
			(eqlist? a b))))

(define (eqlist? l1 l2)
  "Rewriting, using equal?."
  (cond ((and (null? l1) (null? l2)) #t)
		  ((or  (null? l1) (null? l2)) #f)
		  (else
			(and (equal? (car l1) (car l2))
				  (eqlist? (cdr l1) (cdr l2))))))

;;; ========== Chapter the Sixth ==========

(define (operator? n)
  (or (eq? n '+)
		(eq? n 'x)
		(eq? n '^)))

(define (operator op)
  (cond ((eq? '+ op) +)
		  ((eq? 'x op) *)
		  ((eq? '^ op) power)
		  (else
			(error "Oh noes!"))))

(define (get-operator n)
  (car n))
(define (first-expression n)
  (cadr n))
(define (second-expression n)
  (caddr n))

(define (numbered? n)
  "T if n is a number, or an s-expression containing an arithmetic
expression."
  (if (atom? n)
		(number? n)
		(and (eq? 3 (length n))
			  (numbered? (first-expression n))
			  (operator? (get-operator n))
			  (numbered? (second-expression n)))))

(define (value n)
  "Does maths.  On n.  Because."
  (cond ((not (numbered? n)) (error "Not an expression!"))
		  ((atom? n) n)
		  (else (let ((a (first-expression n))
						  (b (second-expression n))
						  (opp (operator (get-operator n))))
					 (opp (value a) (value b))))))


;;; ========== Chapter the seventh ==========

(define (set? s)
  "T if s contains only unique atoms."
  (cond ((null? s) #t)
		  ((member? (car s) (cdr s)) #f)
		  (else
			(set? (cdr s)))))

(define (makeset s)
  "Returns a list of unique atoms in s."
  (cond ((null? s) '())
		  ((member? (car s) (cdr s))
			(makeset (cdr s)))
		  (else
			(cons (car s)
					(makeset (cdr s))))))

(define (subset? s1 s2)
  "T if s1 is a sub set of s2."
  (cond ((null? s1) #t)
		  ((member? (car s1) s2)
			(subset? (cdr s1) s2))
		  (else
			#f)))

(define (intersects? s1 s2)
  "T if s1 and s2 intersect."
  (cond ((null? s1) #f)
		  ((member? (car s1) s2) #t)
		  (else
			(intersects? (cdr s1) s2))))

(define (intersection s1 s2)
  "Returns the intersection of s1 & s2."
  (cond ((or (not (set? s1))
				 (not (set? s2)))
			(error "Both s1 and s2 need to be sets."))
		  ((or (null? s1) (null? s2))
			'())
		  ((member? (car s1) s2)
			(cons (car s1) (intersection (cdr s1) s2)))
		  (else
			(intersection (cdr s1) s2))))

(define (union s1 s2)
  "Guess....."
  (cond ((or (not (set? s1))
				 (not (set? s2)))
			(error "Both s1 and s2 must be sets."))
		  ((null? s1) s2)
		  ((null? s2) s1)						 ; avoid testing s1 if s2 is nil 
		  ((member? (car s1) s2)
			(union (cdr s1) s2))
		  (else
			(cons (car s1)
					(union (cdr s1) s2)))))

(define (intersect* lst)
  "Intersection of all subsets of lst."
  (if (null? (cdr lst))
		(car lst)
		(intersection (car lst)
						  (intersect* (cdr lst)))))


