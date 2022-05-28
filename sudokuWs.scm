;;;;sudoku solver v1
(import
  (sudokuWs)
  (chibi time)  
  (scheme base)
  (scheme red)   
  (delay)
  (srfi 179)
  (scheme vector))

(define (solve)
  (let/ec return
    (for2 0 8 (lambda(row)
      (for2 0 8 (lambda(col)
        (if (=(vector-ref grid (row_col->cell row col)) 0)
          (let num-loop ((num 1))
            (if (not (= 10 num))
              (begin
                (if (possible? row col num grid)
                  (begin
                    (vector-set! grid (row_col->cell row col) num)
                    (solve)
                    (when (no-zeros-left? grid)(begin (print-grid grid) (exit)))
                    (vector-set! grid (row_col->cell row col) 0)))
                (num-loop (+ 1 num)))
              (return)))))))))) 

(define (main args)
  (solve))