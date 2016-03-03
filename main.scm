(use (only data-structures conc))
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

(define (expand-char char count)
  (conc "("
        (case char
          [(#\+) "bf-inc!"]
          [(#\-) "bf-dec!"]
          [(#\>) "bf-fd!"]
          [(#\<) "bf-bk!"]
          [else (error "fuck up")])
        " " count ")"))

(define (compressable? ch)
  (case ch
    [(#\+ #\- #\< #\>) ch]
    [else #f]))

(define (compress)
  (let loop ([ch (read-char)]
             [run? #f]
             [count 0])
    (cond [(eof-object? ch)
           (if run?
               (display (expand-char run? count))
               (display (convert-char ch)))]
          [run?                         ; 今まで連続していた
           (cond [(char=? run? ch)      ; まだまだ連続
                  (loop (read-char) run? (add1 count))]
                 [(compressable? ch)    ; 連続の起点
                  (display (expand-char run? count))
                  (loop (read-char) ch 1)]
                 [else                  ; 普通に処理
                  (display (expand-char run? count)) ; 今までの
                  (display (convert-char ch)) ; 今の
                  (loop (read-char) #f 0)])]
          [else
           (cond [(compressable? ch)
                  (loop (read-char) ch 1)]
                 [else
                  (display (convert-char ch))
                  (loop (read-char) #f 0)])])))

(define (bf-compile)
  (let ([ch (read-char)])
    (unless (eof-object? ch)
      (display (convert-char ch))
      (bf-compile))))
