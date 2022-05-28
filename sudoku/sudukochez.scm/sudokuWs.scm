;;;;sudoku solver v1
(define-syntax let/ec 
  (syntax-rules ()
    ((_ return body ...)
     (call-with-current-continuation
      (lambda (return)
        body ...)))))
 (define for2 (lambda (start end func)
      (let loop ((index start))
        (if (> index end) #t
          (begin
            (func index)
            (loop (+ index 1)))))))

  (define call-with-input-file 
    (lambda (filename proc)
  	  (let ((p (open-input-file filename)))
       (let ((str (proc p)))  
        (close-input-port p)
        str))))

  (define call-with-output-file
    (lambda(filename proc str)
      (let ((p (open-output-file filename)))
        (proc str p)    
        (close-output-port p))))

  (define grid2 (list->vector
    (append
      '(5 3 0 0 7 0 0 0 0)
      '(6 0 0 1 9 5 0 0 0)
      '(0 9 8 0 0 0 0 6 0)
      '(8 0 0 0 6 0 0 0 3)
      '(4 0 0 8 0 3 0 0 1)
      '(7 0 0 0 2 0 0 0 6)
      '(0 6 0 0 0 0 2 8 0)
      '(0 0 0 4 1 9 0 0 5)
      '(0 0 0 0 8 0 0 7 9))))

  (define grid (list->vector
    (append
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0)
      '(0 0 0 0 0 0 0 0 0))))

  (define (check cell_list num) ;free to place number t/f  
    (let loop ((cell 0))      
      (if (not (= (vector-ref grid (vector-ref cell_list cell)) num))           
          (if (not (= cell 8))(loop (+ cell 1))#t)
            #f)))

  (define (no-zeros-left? grid)
    (let ((length (- (vector-length grid) 1)))
      (let loop ((count 0)) 
        (if (= (vector-ref grid count) 0)
          #f
        (if (not (= count length))
          (loop (+ count 1))
          #t)))))
 
  (define (print-grid)
    (newline)
    (for2 0 8 (lambda(row)
      (for2 0 8 (lambda(col)
        (display (vector-ref grid (row_col->cell row col)))))
        (newline))))

  (define (row_col->cell row col)
    (+ (* row 9) col))
     
  (define (possible? row col num)

    (define (get_box_number row col) 
      (cond 
        ((or (= col 0) (= col 1) (= col 2))
          (cond 
            ((or (= row 0) (= row 1) (= row 2)) 0)
            ((or (= row 3) (= row 4) (= row 5)) 1)
            ((or (= row 6) (= row 7) (= row 8)) 2)))
        ((or(= col 3) (= col 4) (= col 5))
          (cond
            ((or (= row 0) (= row 1) (= row 2)) 3)
            ((or (= row 3) (= row 4) (= row 5)) 4)
            ((or (= row 6) (= row 7) (= row 8)) 5)))      
        ((or (= col 6) (= col 7) (= col 8))
          (cond 
            ((or (= row 0) (= row 1) (= row 2)) 6)
            ((or (= row 3) (= row 4) (= row 5)) 7)
            ((or (= row 6) (= row 7) (= row 8)) 8))))) 

    (define (get_row_cells row)
      (let loop ((start (* row 9)))
        (vector start (+ start 1) (+ start 2) (+ start 3) (+ start 4) (+ start 5) (+ start 6) (+ start 7) (+ start 8))))

    (define (get_col_cells col)
      (vector col (+ col 9) (+ col 18) (+ col 27) (+ col 36) (+ col 45) (+ col 54) (+ col 63) (+ col 72)))
  
    (define (cell_list_row row)
      (vector row  (+ row 9) (+ row 18) (+ row 27) (+ row 36) (+ row 45) (+ row 54) (+ row 63) (+ row 72)))

    (define (cell_list_col col)
      (vector col (+ col 9) (+ col 18) (+ col 27) (+ col 36) (+ col 45) (+ col 54) (+ col 63) (+ col 72)))

    (define (get_box_cells row col)
        (let ((box (get_box_number row col)))
          (cond
            ((= box 0)(vector 0 1 2 9 10 11 18 19 20))
            ((= box 1)(vector 3 4 5 12 13 14 21 22 23))
            ((= box 2)(vector 6 7 8 15 16 17 24 25 26))
            ((= box 3)(vector 27 28 29 36 37 38 45 46 47))
            ((= box 4)(vector 30 31 32 39 40 41 48 49 50))
            ((= box 5)(vector 33 34 35 42 43 44 51 52 53))
            ((= box 6)(vector 54 55 56 63 64 65 72 73 74))
            ((= box 7)(vector 57 58 59 66 67 68 75 76 77))
            ((= box 8)(vector 60 61 62 69 70 71 78 79 80))))) 

    (and (check (get_row_cells row) num)
         (check (get_col_cells col) num)
         (check (get_box_cells row col) num)))
    
(define for2 (lambda (start end func)
              (let loop ((index start))
                (if (> index end) #t
                    (begin (func index) (loop (+ index 1))) ))))

(define (solve)
  (let/ec return
    (for2 0 8 (lambda(row)
	       (for2 0 8 (lambda(col)
                    (if (eqv?(vector-ref grid (row_col->cell row col)) 0)
                        (let num-loop ((num 1))
                          (if (not (eqv? 10 num))
                              (begin
                                (if (possible? row col num)
                                    (begin
                                      (vector-set! grid (row_col->cell row col) num)
                                      (solve)
                                      (when (no-zeros-left? grid)(begin (print-grid) (exit)))
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