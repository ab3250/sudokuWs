;;;;sudoku solver v1
(import
  (sudokuWs)
  (chibi time)  
  (scheme base)
  (scheme red)  
  (chibi io) 
  (delay)
  (scheme vector)
  (fifoRWP))

(define pred1 (lambda (gr)  (no-zeros-left? gr)))


(define (solve grid) 
    (nested-loop-with-break row 0 8 (pred1 grid) col 0 8 (pred1 grid)
        (if (=(vector-ref grid (row_col->cell row col)) 0)
          (let num-loop ((num 1))
            (if (not (= 10 num))
              (begin            
                (if (possible? row col num grid)
                  (begin
                  ;(print-grid grid)
                    (vector-set! grid (row_col->cell row col) num)                    
                    (scheme_write_ws fifoOut 
                      (grid-string grid))
                    (solve grid)
                    (vector-set! grid (row_col->cell row col) 0)))
                (num-loop (+ 1 num))))))))

(define (main args)
  (let loop ()
    (let ((msg (scheme_read_ws fifoIn)))          
      (cond 
        ((string=? msg "button1")(lock (solve (vector-copy grid1))))
        ((string=? msg "button2")(lock (solve (vector-copy grid2))))
        ((string=? msg "button3")(scheme_write_ws fifoOut (grid-string grid1)))
        ((string=? msg "button4")(scheme_write_ws fifoOut (grid-string grid2)))) 
  (loop))))


