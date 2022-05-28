;;;;sudoku solver v4

(include "../library/sudoku-library.scm")



(define-syntax nested-loop
  (syntax-rules ()
    ((_ l1 l1-start l1-end l2 l2-start l2-end body ...)
         (for l1-start l1-end (lambda(l1)
		    (for l2-start l2-end (lambda(l2)
			       (begin
				body ... ))))))))

(define (solve)
  (let/ec return
    (nested-loop row 0 8 col 0 8
                 (if (eqv?(array-ref grid row col) 0)
                     (let num-loop ((num 1))
                       (if (not (eqv? 10 num))
                           (begin
                             (if (possible? row col num)
                                 (begin
                                   (array-set! grid num row col)
                                   (solve)
                                   (when (no-zeros-left?)
				     (begin
				       (print-grid)
				       (exit)))
                                   (array-set! grid 0 row col)))
                             (num-loop (+ 1 num)))
                           (return)))))))
(print-grid)
(solve)
