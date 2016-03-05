(module bf-lib
    *
  (import scheme chicken)
  (define-constant tape-length 30000)
  (define pointer 0)
  (define tape (make-vector tape-length 0))
  (define (*pointer)
    (vector-ref tape pointer))
  (define (tape-inc! pos val)
    (vector-set! tape pos
                 (fx+ (vector-ref tape pos)
                    val)))

;;; +
  (define (bf-inc! #!optional (val 1))
    (tape-inc! pointer val))
  (define (bf-inc!-off val off)
    (tape-inc! (fx+ pointer off) val))

;;; - not use
  (define (bf-dec! #!optional (val 1))
    (tape-inc! pointer (fxneg val)))
  (define (bf-dec!-off val off)
    (tape-inc! (fx+ pointer off) (fxneg val)))

;;; >
  (define (bf-fd! #!optional (val 1))
    (set! pointer (fx+ pointer val)))

;;; < not use
  (define (bf-bk! #!optional (val 1))
    (set! pointer (fx- pointer val)))

;;; ,
  (define (bf-getc)
    (vector-set! tape pointer
                 (char->integer (read-char))))
  (define (bf-getc-off off)
    (vector-set! tape (fx+ pointer off)
                 (char->integer (read-char))))


;;; .
  (define (bf-putc)
    (display (integer->char (*pointer))))
  (define (bf-putc-off off)
    (display (integer->char (vector-ref tape
                                        (fx+ pointer off)))))

;;; [
  (define-syntax bf-while
    (syntax-rules ()
      [(_ expr ...)
       (let loop ()
         (unless (fx= 0 (*pointer))
           expr ...
           (loop)))]))
  (define-syntax bf-begin
    (syntax-rules ()
      [(_) (void)]
      [(_ expr ...)
       ;; 0 loop 対策
       (unless (fx= 0 (*pointer))
         expr ...)]))
;;; clear [-]
  (define (bf-clear)
    (vector-set! tape pointer 0))
  (define (bf-clear-off off)
    (vector-set! tape (fx+ pointer off) 0))
  
;;; copy 
  (define (bf-mul pos mul)
    (tape-inc! (fx+ pointer pos)
               (fx* (*pointer) mul)))
  (define (bf-mul-off pos mul off)
    (vector-set! tape
                 (fx+ pointer (fx+ pos off))
                 (fx* (vector-ref tape (fx+ pointer off))
                      mul)))
;;; scan
  
)
