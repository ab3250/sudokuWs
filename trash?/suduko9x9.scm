;library for sudoku version one and two

#|
(define grid (list->array 2 '((5 3 0 0 7 0 0 0 0)
			      (6 0 0 1 9 5 0 0 0)
			      (0 9 8 0 0 0 0 6 0)
			      (8 0 0 0 6 0 0 0 3)
			      (4 0 0 8 0 3 0 0 1)
			      (7 0 0 0 2 0 0 0 6)
			      (0 6 0 0 0 0 2 8 0)
			      (0 0 0 4 1 9 0 0 5)
			      (0 0 0 0 8 0 0 7 9))))
|#
;#|
(define grid  (list->array 2 '((0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0)
			      (0 0 0 0 0 0 0 0 0))))
;|#
(define-syntax let/ec 
  (syntax-rules ()
    ((_ return body ...)
     (call-with-current-continuation
      (lambda (return)
        body ...)))))

(define for (lambda (start end func)
              (let loop ((index start))
                (if (> index end) #t
                    (begin (func index) (loop (+ index 1))) ))))

(define (position-9x9 val li2)
  (let loop ((li li2)(count 0))
    (cond ((null? li) #f)
          ((equal? val (car li))
           (cons (truncate(/ count 9))(modulo count 9)))
          (else (loop (cdr li)(+ count 1))))))

(define (get-next-zero grid)
  (position-9x9 0 (array->list (array-contents grid))))

(define-syntax no-zeros-left?
  (syntax-rules ()
    ((_)
     (if (pair? (get-next-zero grid))
	 #f
	 #t))))
 
(define (print-grid)
  (newline)
  (for 0 8 (lambda(row)
	     (for 0 8 (lambda(col)
			(display (array-ref grid row col))))
	     (newline))))
;
;Check if number will work in cell by checking row, column and box.
;

(define (possible? row col num)
;;;
  (define (check-row row num) ;free to place number t/f
    (let loop ((cell 0))
      (if (not (eqv? 9 cell))
          (if (eqv?(array-ref grid row cell) num)
              #f
              (loop (+ cell 1)))
          #t)))
;;;
  (define (check-col col num)  ;free to place number t/f
    (let loop ((cell 0))
      (if (not (eqv? 9 cell))
          (if (eqv?(array-ref grid cell col) num)
              #f            
              (loop (+ cell 1)))
          #t)))
;;;
  (define (check-box  cur-row cur-col num)  ;free to place number t/f
    (let row-loop ((row 0))
      (if (not (eqv? 3 row))
          (let col-loop ((col 0))
            (if (not (eqv? 3 col))
                (if (eqv?(array-ref grid (+(* (truncate(/ cur-row 3))3)row) (+(* (truncate(/ cur-col 3))3)col)) num)
                    #f
                    (col-loop (+ col 1)))
                (row-loop (+ row 1))))
          #t)))
  ;;;;
  (and (check-row row num)
       (check-col col num)
       (check-box row col num)))

(define (solve)
  (let/ec return
    (for 0 8 (lambda(row)
	       (for 0 8 (lambda(col)
                    (if (eqv?(array-ref grid row col) 0)
                        (let num-loop ((num 1))
                          (if (not (eqv? 10 num))
                              (begin
                                (if (possible? row col num)
                                    (begin
                                      (array-set! grid num row col)
                                      (solve)
                                      (when (no-zeros-left?)
					(begin
					  (print-grid)
					  (exit)))
                                      (array-set! grid 0 row col)))
                                (num-loop (+ 1 num)))
                              (return))))))))))


