(module bf-lib
    *
  (import scheme chicken)
  
  (define tape-length 30000)
  (define pointer 0)
  (define tape (make-vector tape-length 0))
  (define (*pointer)
    (vector-ref tape pointer))
  (define (tape-inc! pos val)
    (vector-set! tape pos
                 (+ (vector-ref tape pos)
                    val)))

;;; +
  (define (bf-inc! #!optional (val 1))
    (tape-inc! pointer val))

;;; - not use
  (define (bf-dec! #!optional (val 1))
    (tape-inc! pointer (- val)))

;;; >
  (define (bf-fd! #!optional (val 1))
    (set! pointer (+ pointer val)))

;;; < not use
  (define (bf-bk! #!optional (val 1))
    (set! pointer (- pointer val)))


;;; ,
  (define (bf-getc)
    (vector-set! tape pointer
                 (char->integer (read-char))))


;;; .
  (define (bf-putc)
    (display (integer->char (*pointer))))

;;; [
  (define-syntax bf-while
    (syntax-rules ()
      [(_ expr ...)
       (let loop ()
         (unless (zero? (*pointer))
           expr ...
           (loop)))]))
;;; clear [-]
  (define (bf-clear)
    (vector-set! tape pointer 0))
  
;;; copy 
  (define (bf-copy pos mul)
    (tape-inc! (+ pointer pos)
               (* (*pointer) mul)))
;;; scan

)
