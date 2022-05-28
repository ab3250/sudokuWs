;;;;sudoku solver v3
;; unfinished

(include "../library/sudoku-library.scm")

(define (solve)
  (let loop ()
    (for 0 8 (lambda(row)
	       (for 0 8 (lambda(col)
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
                                (num-loop (+ 1 num)))(loop))))))))))
(print-grid)
(solve)
