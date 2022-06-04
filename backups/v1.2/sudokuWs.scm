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

(define (solve grid)
 (let loop2 ()
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
                    (delay-seconds 1)        
                    (scheme_write_ws fifoOut 
                      (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid))) "\"}"))
                    (loop2)
                    (when (no-zeros-left? grid)
                      (begin                                                
                        ;(print-grid grid)
                        (return))) ;unwinds recursion ? yuck
                    (vector-set! grid (row_col->cell row col) 0)))
                (num-loop (+ 1 num)))
              (return)))))))))))

(define (main args)
(gwinit)
  (let loop ()
     (let ((msg (scheme_read_ws fifoIn)))    
      (flush-output-port (current-output-port))
      (display (list 'debug msg))
      (cond 
        ((string=? msg "button1")(solve (vector-copy grid1)))
        ((string=? msg "button2")(solve (vector-copy grid2)))
        ((string=? msg "button3")(scheme_write_ws fifoOut 
                     (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid1))) "\"}")))
        ((string=? msg "button4")(scheme_write_ws fifoOut
                     (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid2))) "\"}")))) 
    (loop))))