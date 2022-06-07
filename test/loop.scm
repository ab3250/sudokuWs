(import
  (scheme base)
  (scheme red)    
  )

(define-syntax nested-loop-with-break
  (syntax-rules ()
    ((_ l1 l1-start l1-end pred1 l2 l2-start l2-end pred2 body ...)
         (for-with-break l1-start l1-end pred1 (lambda(l1)
		    (for-with-break l2-start l2-end pred2 (lambda(l2)
			       (begin
				body ... ))))))))

(define for-with-break (lambda (start end pred func) 
      (let loop ((index start))
        (if (or (> index end) (pred index)) #t
          (begin
            (func index)
            (loop (+ index 1)))))))

(define (pred1 i) (= i 3))

(define (show x) (display x))

(define (main args)
  (nested-loop-with-break row 0 9 pred1 col 0 9 pred1 
    (display row)(display " ")(display col)(newline)
  )
  
  )
