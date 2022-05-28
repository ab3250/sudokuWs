;;;;sudoku solver v2

(include "../library/sudoku-library.scm")

(define (solve)  
  (if (no-zeros-left?)
      (begin (print-grid)(exit)) ;solved
      (letrec ((next-zero (get-next-zero grid)) (row (car next-zero)) (col (cdr next-zero)))      
        (for 1 9 (lambda(num)                            
                   (if (possible? row col num)
                       (begin
                         (array-set! grid num row col)			 
                         (solve) 
                         (array-set! grid 0 row col))))))))
(print-grid)
(solve)


