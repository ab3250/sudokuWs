;;;;sudoku solver v1

(include "../library/sudoku-library.scm")
(define (solve)
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
                                      (when (no-zeros-left?)(print-grid))
                                      (array-set! grid 0 row col)))
                                (num-loop (+ 1 num)))
                             ))))))))
;(print-grid)
(solve)
