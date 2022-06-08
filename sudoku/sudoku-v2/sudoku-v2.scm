;;;;sudoku solver v2

;(include "../library/sudoku-library.scm")
;#|
(define grid (list->array 2 
  '((5 3 0 0 7 0 0 0 0)
	  (6 0 0 1 9 5 0 0 0)
	  (0 9 8 0 0 0 0 6 0)
	  (8 0 0 0 6 0 0 0 3)
	  (4 0 0 8 0 3 0 0 1)
	  (7 0 0 0 2 0 0 0 6)
	  (0 6 0 0 0 0 2 8 0)
	  (0 0 0 4 1 9 0 0 5)
	  (0 0 0 0 8 0 0 7 9))))
;|#
(define for (lambda (start end func)
              (let loop ((index start))
                (if (> index end) #t
                    (begin (func index) (loop (+ index 1)))))))

(define (print-grid)
  (newline)
  (for 0 8 (lambda(row)
	     (for 0 8 (lambda(col)
			(display (array-ref grid row col))))
	     (newline))))

(define (position-9x9 val li2)
  (let loop ((li li2)(count 0))
    (cond ((null? li) #f)
          ((equal? val (car li))
           (cons (truncate(/ count 9))(modulo count 9)))
          (else (loop (cdr li)(+ count 1))))))

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

(define-syntax no-zeros-left?
  (syntax-rules ()
    ((_)
     (if (pair? (get-next-zero grid))
	 #f
	 #t))))

(define (get-next-zero grid)
  (position-9x9 0 (array->list (array-contents grid))))

(define (solve)  
  (if (no-zeros-left?)
      (begin (print-grid)(exit)) ;solved
      (letrec ((next-zero (get-next-zero grid)) (row (car next-zero)) (col (cdr next-zero)))      
        (for 1 9 (lambda(num)                            
                   (if (possible? row col num)
                       (begin
                         (array-set! grid num row col)			 
                         (solve) 
                         (array-set! grid 0 row col))))))))
;(print-grid)
(solve)


