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
                    (scheme_write_ws fifoOut 
                      (string-append  "{\"type\":\"grid\",\"num\":\"" (apply string-append (map number->string (vector->list grid))) "\"}"))
                    (solve grid)
                    (when (no-zeros-left? grid)(return))
                    (vector-set! grid (row_col->cell row col) 0)))
                (num-loop (+ 1 num)))
              (return))))))))))

(define (main args)
(gwinit)
  (let loop ()
     (let ((msg (scheme_read_ws fifoIn)))    
      ;(flush-output-port (current-output-port))
      (display (list 'debug msg))
      (cond 
        ((string=? msg "button1")(begin (scheme_write_ws fifoOut "{\"type\":\"lock\"}")
          (solve (vector-copy grid1))
          (scheme_write_ws fifoOut "{\"type\":\"unlock\"}")))
        ((string=? msg "button2")(begin (scheme_write_ws fifoOut "{\"type\":\"lock\"}")
          (solve (vector-copy grid2))
          (scheme_write_ws fifoOut "{\"type\":\"unlock\"}")))
        ((string=? msg "button3")(scheme_write_ws fifoOut 
          (string-append  "{\"type\":\"grid\",\"num\":\"" (apply string-append (map number->string (vector->list grid1))) "\"}")))
        ((string=? msg "button4")(scheme_write_ws fifoOut
          (string-append  "{\"type\":\"grid\",\"num\":\"" (apply string-append (map number->string (vector->list grid2))) "\"}")))) 
  (loop))))


   ; (define jsonify (lambda (type data)
    ;(string-append "{\"type\": \"" type "\",\"data\":" (json->string data) "}")))


      ; ((string=? msg "button1")(begin (scheme_write_ws fifoOut "{\"lock\": \"null\"}")
      ;     (solve (vector-copy grid1))))
      ;   ((string=? msg "button2")(begin (scheme_write_ws fifoOut "{\"unlock\": \"}")(solve (vector-copy grid2))))
      ;   ((string=? msg "button3")(scheme_write_ws fifoOut 
      ;                (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid1))) "\"}")))
      ;   ((string=? msg "button4")(scheme_write_ws fifoOut
      ;                (string-append  "{\"num\": \"" (apply string-append (map number->string (vector->list grid2))) "\"}")))) 

     ; {"type": "grid","num": "000000000000000000000000000000000000000000000000000000000000000000000000000000000"}

     ;{"type": "grid","num": "534678912672195348198342567859761423426853791713924856961537284287419635340080079"}
   ;  {"type": "grid","num": "534678912672195348198342567859761423426853791713924856961537284287419635340080079"}


    ;const d = JSON.parse(evt.data).num.toString()