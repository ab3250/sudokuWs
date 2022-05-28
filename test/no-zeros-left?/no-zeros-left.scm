(import
    (scheme base)
    (scheme red)   
    (scheme vector))

    (define grid (vector 5 4 5 6 0))

(define (no-zeros-left? grid)
(let ((length (- (vector-length grid) 1)))
(let loop ((count 0)) 
  (if (= (vector-ref grid count) 0)
    #f
    (if (not (= count length))
        (loop (+ count 1))
        #t)))))


(display (no-zeros-left? grid))