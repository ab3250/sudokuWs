;;;;sudoku solver v1
(import
  (sudokuWs)
  (chibi time)  
  (scheme base)
  (scheme red)  
  (chibi io) 
  (delay)
  (scheme vector))

(define (solve)
  (let/ec return
  (call-with-output-file2 "send" write-line 
                      (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid))) "\"}"))
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
                    (delay 150000)        
                    (call-with-output-file2 "send" write-line 
                      (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid))) "\"}"))
                    (when (no-zeros-left? grid)
                      (begin                                                
                        (print-grid grid)
                        (exit)))
                    (vector-set! grid (row_col->cell row col) 0)))
                (num-loop (+ 1 num)))
              (return))))))))))

(define (main args)
  (solve)
)