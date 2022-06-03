;;;;sudoku solver v1
(import
  (sudokuWs)
  (chibi time)  
  (scheme base)
  (scheme red)  
  (chibi io) 
  (delay)
  (scheme vector)
  (fifoRWP)
)

(define (solve grid)
(let ((grid-copy (vector-copy grid)))
 (let loop2 ()
  (let/ec return
    (for2 0 8 (lambda(row)
      (for2 0 8 (lambda(col)
        (if (=(vector-ref grid-copy (row_col->cell row col)) 0)
          (let num-loop ((num 1))
            (if (not (= 10 num))
              (begin
                (if (possible? row col num grid-copy)
                  (begin
                    (vector-set! grid-copy (row_col->cell row col) num)
                   ; (delay-seconds 1)        
                    (scheme_write_ws fifoOut 
                      (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid-copy))) "\"}"))
                    (loop2)      

                    (when (no-zeros-left? grid-copy)
                      (begin                                                
                        ;(print-grid grid-copy)
                        (return))) ;unwinds recursion ? yuck
                    (vector-set! grid-copy (row_col->cell row col) 0)))
                (num-loop (+ 1 num)))
              (return))))))))))))

(define (main args)
  (let loop ()
     (let ((msg (scheme_read_ws fifoIn)))    
     ; (flush-output-port (current-output-port))
      (display (list 'debug msg))
      (cond 
        ((string=? msg "button1")(solve grid1))
        ((string=? msg "button2")(solve grid2))
        ((string=? msg "button3")(scheme_write_ws fifoOut 
                     (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid1))) "\"}")))
        ((string=? msg "button4")(scheme_write_ws fifoOut
                     (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid2))) "\"}")))) 
    (loop))))
