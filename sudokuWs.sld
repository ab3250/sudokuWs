(define-library (sudokuWs)
  (export       
    grid1
    grid2
    possible?
    row_col->cell
    no-zeros-left?
    print-grid
    for
    let/ec 
    fifoOut
    fifoIn 
    lock-buttons
    unlock-buttons
    lock
    grid-string
    nested-loop
    nested-loop-with-break
    ;scheme_write_ws
    ;scheme_write_ws
    )
 (import
  (chibi time)
  (scheme base)
  (scheme red)  
  (scheme vector)
  (fifoRWP))

  ;library for sudokuWs
  (begin
  (define fifoIn "/tmp/fifoIn")
  (define fifoOut "/tmp/fifoOut")
 
;#|
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
;|#
;#| 
  (define grid1 (list->vector
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
;|#

(define-syntax nested-loop-with-break
  (syntax-rules ()
    ((_ l1 l1-start l1-end l2 l2-start l2-end pred body ...)
         (for-with-break l1-start l1-end (lambda(l1)
		    (for-with-break l2-start l2-end (lambda(l2)
			       (begin
				body ... ))))))))

(define for-with-break (lambda (start end func pred var)
      (let loop ((index start))
        (if (or (> index end) (pred var)) #t
          (begin
            (func index)
            (loop (+ index 1)))))))


(define (grid-string grid)
  (string-append  "{\"type\":\"grid\",\"num\":\"" (apply string-append (map number->string (vector->list grid))) "\"}"))

(define (lock-buttons)
  (scheme_write_ws fifoOut "{\"type\":\"lock\"}"))

(define (unlock-buttons)
  (scheme_write_ws fifoOut "{\"type\":\"unlock\"}"))

(define-syntax lock
  (syntax-rules ()
    ((_ body ...)
      (begin (lock-buttons)
        body ...
        (unlock-buttons)))))

  (define for (lambda (start end func)
      (let loop ((index start))
        (if (> index end) #t
          (begin
            (func index)
            (loop (+ index 1)))))))

(define-syntax nested-loop
  (syntax-rules ()
    ((_ l1 l1-start l1-end l2 l2-start l2-end body ...)
         (for l1-start l1-end (lambda(l1)
		    (for l2-start l2-end (lambda(l2)
			       (begin
				body ... ))))))))

(define-syntax let/ec 
  (syntax-rules ()
    ((_ return body ...)
     (call-with-current-continuation
      (lambda (return)
        body ...)))))

(define-syntax nested-loop-with-break
  (syntax-rules ()
    ((_ l1 l1-start l1-end pred1 l2 l2-start l2-end pred2 body ...)
         (for-with-break l1-start l1-end pred1 (lambda(l1)
		    (for-with-break l2-start l2-end pred2 (lambda(l2)
			       (begin
				body ... ))))))))

(define for-with-break (lambda (start end pred func) 
      (let loop ((index start))
        (if (or (> index end) pred) #t
          (begin
            (func index)
            (loop (+ index 1)))))))


; (define call-with-input-file2 
;     (lambda (filename proc)
;   	  (let ((p (open-input-file filename)))
;        (let ((str (proc p)))  
;         (close-input-port p)
;         str))))

; (define call-with-output-file2
;     (lambda(filename proc str)
;       (let ((p (open-output-file filename)))
;         (proc str p)    
;         (close-output-port p))))


(define (check cell_list num grid) ;free to place number t/f  
    (let loop ((cell 0))      
      (if (= (vector-ref grid (vector-ref cell_list cell)) num)
          #f
          (if (not (= cell 8)) (loop (+ cell 1)) #t))))


(define (no-zeros-left? grid)
    (let ((length (- (vector-length grid) 1)))
      (let loop ((count 0)) 
        (if (= (vector-ref grid count) 0)
          #f
        (if (not (= count length))
          (loop (+ count 1))
          #t)))))

(define (get_row_cells row)
      (let loop ((start (* row 9)))
        (vector start (+ start 1) (+ start 2) (+ start 3) (+ start 4) (+ start 5) (+ start 6) (+ start 7) (+ start 8))))

(define (get_col_cells col)
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

(define (get_box_number row col) 
      (cond 
        ((or (= row 0) (= row 1) (= row 2))
          (cond 
            ((or (= col 0) (= col 1) (= col 2)) 0)
            ((or (= col 3) (= col 4) (= col 5)) 1)
            ((or (= col 6) (= col 7) (= col 8)) 2)))
        ((or(= row 3) (= row 4) (= row 5))
          (cond
            ((or (= col 0) (= col 1) (= col 2)) 3)
            ((or (= col 3) (= col 4) (= col 5)) 4)
            ((or (= col 6) (= col 7) (= col 8)) 5)))      
        ((or (= row 6) (= row 7) (= row 8))
          (cond 
            ((or (= col 0) (= col 1) (= col 2)) 6)
            ((or (= col 3) (= col 4) (= col 5)) 7)
            ((or (= col 6) (= col 7) (= col 8)) 8)))))

(define (print-grid grid)
    (newline)
    (for 0 8 (lambda(row)
      (for 0 8 (lambda(col)
        (display (vector-ref grid (row_col->cell row col)))))
        (newline))))

(define (row_col->cell row col)
    (+ (* row 9) col))

(define (possible? row col num grid)
  (and (check (get_row_cells row) num grid)
       (check (get_col_cells col) num grid)
       (check (get_box_cells row col) num grid)))

;;;;;;;;;;;;;
    
));eolib

  




