;;;;sudoku solver v1
(import
  (sudokuWs)
  (chibi time)  
  (scheme base)
  (scheme red)  
  (chibi io)
  (schemepunk json)  
  (srfi 1)
  (delay)
  (srfi 179)
  (scheme list)
  (chibi loop)
  (scheme vector))



(define (solve)
  (let/ec return
    (for2 0 8 (lambda(row)
	       (for2 0 8 (lambda(col)
                    (if (eqv?(vector-ref grid (row_col->cell row col)) 0)
                        (let num-loop ((num 1))
                          (if (not (eqv? 10 num))
                              (begin
                                (if (possible? row col num grid)
                                    (begin
                                      (vector-set! grid (row_col->cell row col) num)
                                      (solve)
                                      (when (no-zeros-left? grid)(begin (print-grid grid) (exit)))
                                      ;(print-grid)
                                      (vector-set! grid (row_col->cell row col) 0)))
                                (num-loop (+ 1 num)))
                              (return))))))))))

(define (main args)
  (solve))

; (define (solve)
;   (for2 0 8 (lambda(row)
;     (for2 0 8 (lambda(col)
;       (if (=(vector-ref grid (row_col->cell row col)) 0)
;         (let num-loop ((num 1))
;           (if (not (eqv? 10 num))
;             (begin
;               (if (possible? row col num)
;                 (begin
;                   (vector-set! grid (row_col->cell row col) num )
;                   ;(print-grid)
;                   (solve)
;                   (when (no-zeros-left? grid)(print-grid))
;                   (vector-set! grid (row_col->cell row col) 0 )))
;                 (num-loop (+ 1 num)))))))))))
