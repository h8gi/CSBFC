;;; main.scm    main source file
(define (convert-char char)
  (case char
    ;; ++(*ptr)
    [(#\+) "(bf-inc!)"]
    
    ;; --(*ptr)
    [(#\-) "(bf-dec!)"]

    ;; ++ptr
    [(#\>) "(bf-fd!)"]

    ;; --ptr
    [(#\<) "(bf-bk!)"]

    ;; *ptr = getchar()
    [(#\,) "(bf-getc)"]
    
    ;; putchar(*ptr)
    [(#\.) "(bf-putc)"]
    
    ;; while(*ptr) {
    [(#\[) "(bf-while"]
    ;; }
    [(#\]) ")"]

    ;; else
    [else ""]
    ))


(define (bf-compile)
  (let ([ch (read-char)])
    (unless (eof-object? ch)
      (display (convert-char ch))
      (bf-compile))))


