#!r6rs
(import
        (rnrs (6))
        (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6))
        (rnrs r5rs (6))
        (math array)
        
       )
;library for sudoku version one and two

;(#%require srfi/147)
;(require scheme list)
(define (list->array rank proto lst)
    (define dimensions
      (do ((shp '() (cons (length row) shp))
           (row lst (car lst))
           (rnk (- rank 1) (- rnk 1)))
        ((negative? rnk) (reverse shp))))
    (let ((nra (apply make-array proto dimensions)))
      (define (l2ra dims idxs row)
        (cond ((null? dims)
               (apply array-set! nra row (reverse idxs)))
              ((unless (eqv? (car dims) (length row))
                 (error 'list->array
                        "non-rectangular array ~a ~a"
                        dims dimensions))
               (do ((idx 0 (+ idx 1))
                    (row row (cdr row)))
                 ((>= idx (car dims)))
                 (l2ra (cdr dims) (cons idx idxs) (car row))))))
      (l2ra dimensions '() lst)
      nra))
(define grid  (make-array  (vector 2) (vector (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0)
                               (vector 0 0 0 0 0 0 0 0 0))))

(display (array? grid))

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

;(define (get-next-zero grid)
;  (position-9x9 0 (array->list (array-contents grid))))

(define (get-next-zero grid)
  (position-9x9 0 (array->list grid)))

(define-syntax no-zeros-left?
  (syntax-rules ()
    ((_)
     (if (pair? (get-next-zero grid))
	 #f
	 #t))))
 
;(define (print-grid)
;  (newline)
;  (for 0 8 (lambda(row)
;	     (for 0 8 (lambda(col)
;			(display (array-ref grid row col))))
;	     (newline))))
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
					 ; (print-grid)
					  (exit)))
                                      (array-set! grid 0 row col)))
                                (num-loop (+ 1 num)))
                              (return))))))))))
;(print-grid)
(solve)


