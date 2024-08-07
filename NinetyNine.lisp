(asdf:load-system 'trivia)
(use-package 'trivia)

(format nil "~r" 99)
(defun succ (x)
	(1+ x))
(defun pred (x)
	(1- x))


(defun my-last (lx)
	"1. Example: * (my-last '(a b c d)) (D)"
	(match lx
		((cons x nil)
		 x)
		((cons x xs)
		 (my-last xs))))

(my-last '(a b c d))
(my-last nil)
(my-last '(a))

(defun my-but-last (lx)
	"2. Example: * (my-but-last '(a b c d)) (C D)"
	(match lx
		((cons x (cons _ nil))
		 x)
		((cons x nil)
		 x)
		((cons x (cons y xs))
		 (my-but-last (cons y xs)))))

(my-but-last nil)
(my-but-last '(a))
(my-but-last '(a b c d))


(defun element-at (lx n)
	"3. The first element in the list is number 0. Example: * (element-at '(a b c d e) 3) D"
	(match n
		(0 (when lx (car lx)))
		(_ (element-at (cdr lx) (pred n)))))

(element-at nil 5)
(element-at '(a) 5)
(element-at '(a b c d e) 3)

(defun number-of-elements (lx)
	"4. Find the number of elements of a list."
	(match lx
		((cons _ xs) (succ (number-of-elements xs)))
		(_ 0)))

(number-of-elements nil)
(number-of-elements (loop for x from 0 to 9 collect x))

(defun reverse-list (lx)
	"5. Reverse a list."
	(labels ;; /= flet
			((reverse-helper (xs acc)
				 (match xs
					 ((cons x xs) (reverse-helper xs (cons x acc)))
					 (_ acc))))
		(reverse-helper lx nil)))

(reverse-list (loop for x from 0 to 9 collect x))
(reverse-list nil)


(defun is-palindrome-p (lx)
	"6. Find out whether a list is a palindrome."
	(every #'equal lx (reverse-list lx)))

(is-palindrome-p '(x a m a x))
(is-palindrome-p '(s e x))


(defun my-flatten (lx)
	"7. Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively). Example: * (my-flatten '(a (b (c d) e))) (A B C D E)"
	(match lx
		((cons x xs)
		 (concatenate 'list (if (listp x) (my-flatten x) (cons x nil)) (my-flatten xs)))))

(my-flatten nil)
(my-flatten '(a (b (c d) e)))
(my-flatten '((a (b c d e f)) g))


(defun compress (lx)
	"8. If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. Example: * (compress '(a a a a b c c a a d e e e e)) (A B C A D E)"
	(match lx
		((list* x y ys)
		 (let
				 ((next-call (compress (cons y ys))))
			 (if (equal x y)
					 next-call
					 (cons x next-call))))
		((cons _ nil)
		 lx)))

(compress '(a a a a b c c a a d e e e e))
(compress '(a a))
(compress '(a))
(compress nil)

(defun pack (lx)
	"9. If a list contains repeated elements they should be placed in separate sublists. Example: * (pack '(a a a a b c c a a d e e e e)) ((A A A A) (B) (C C) (A A) (D) (E E E E))"
	(labels
			((pack-helper (lx acc)
				 (match lx
					 ((list* x nil)
						(cons (cons x acc) nil))
					 ((list* x y xs)
						(if (equal x y)
								(pack-helper (cons y xs) (cons x acc))
								(cons (cons x acc) (pack-helper (cons y xs) nil)))))))
		(pack-helper lx nil)))

(pack '(a a a a b c c a a d e e e e))



(defun encode (lx)
	"10. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. Example: * (encode '(a a a a b c c a a d e e e e)) ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))"
	(map 'list (lambda (x) (list (length x) (car x))) (pack lx)))

(encode '(a a a a b c c a a d e e e e))



(defun encode-modified (lx)
	"11."
	(mapcar (lambda (x) (match x
												((list* len elem nil)
												 (if (>= 1 len)
														 elem
														 x)))) (encode lx)))

(format t "~a~%" (encode-modified '(a a a a b c c a a d e e e e)))


(defun decode-modified (lx)
	"12."
	(mapcan (lambda (x)
						(match x
							((list* len elem nil)
							 (loop for i from 1 to len collect elem))
							(y (cons y nil))))
					lx))

(decode-modified '((4 A) B (2 C) (2 A) D (4 E)))


(defun encode-direct (lx)
	"13."
	(labels
			((helper (n x)
				 (if (<= n 0)
						 x
						 (list (succ n) x)
						 ))
			 (encode-direct-helper (lx accum)
				 (match lx
					 ((list* x y _)
						(if (equal x y)
								(encode-direct-helper (cdr lx) (succ accum))
								(cons (helper accum x)
											(encode-direct-helper (cdr lx) 0))))
					 ((list* x nil)
						(list (helper accum x))))))
		(encode-direct-helper lx 0)))

(encode-direct '(a a a a b c c a a d e e e e)) 


(defun dupli (lx)
	"14."
	(match lx
		((cons x xs)
		 (cons x (cons x (dupli xs))))))

(dupli '(a b c c d))



(defun repli (lx n)
	"15."
	(mapcan (lambda (x) (loop for i from 1 to n collect x)) lx))

(repli '(a b c) 3)
