#!/usr/bin/sbcl --script

(defun fib_naive (n)
  "Naive fibonacci implementation."
  (if (< n 2)
      n
      (+ (fib_naive (- n 1))
	 (fib_naive (- n 2)))))

(defun fib_case (n)
  "Attempt naive fibonacci, but using case-matching on n."
  (case n
    (0 n)
    (1 n)
    (T (+ (fib_case (- n 1)) (fib_case (- n 2))))
    ))



(defun memoize (fn)
  "Memoization utility taken from \"On Lisp\" by Paul Graham."
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))


(defun fib_memoized (n)
  "Memoized version of the naive fibonacci implementation."
  (if (< n 2)
      n
      (+ (fib_memoized (- n 1))
	 (fib_memoized (- n 2)))))

(setf (fdefinition 'fib_memoized) (memoize #'fib_memoized))


(defun main ()
  (print (fib_naive 35))
  (print (fib_case 35))
  (print (fib_memoized 35))
  (loop for i from 1 to 11
    do (print (fib_naive i))
    ))



(main)
