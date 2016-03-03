(use data-structures)
;;; main.scm    main source file
(define bf-debug (make-parameter #f))

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
;;; permit negative count
(define (expand-char char count)
  (conc "("
        (case char
          [(#\+ #\-) "bf-inc!"]
          [(#\> #\<) "bf-fd!"]
          [else (error "fuck up")])
        " " count ")"))

(define (compressable? ch)
  (case ch
    [(#\+ #\- #\< #\>) ch]
    [else #f]))

(define (inc-or-dec? ch)
  (case ch
    [(#\+ #\-) ch]
    [else #f]))

(define (fd-or-bk? ch)
  (case ch
    [(#\< #\>) ch]
    [else #f]))

(define (cp-char=? ch1 ch2)
  (or (and (inc-or-dec? ch1) (inc-or-dec? ch2))
      (and (fd-or-bk? ch1) (fd-or-bk? ch2))))

(define (cp-char-score ch)
  (case ch
    [(#\+ #\>) 1]
    [(#\- #\<) -1]))

(define (legal? ch)
  (case ch
    [(#\+ #\- #\< #\> #\, #\. #\[ #\]) ch]
    [else #f]))

(define (skip-read-char)
  (let ([ch (read-char)])
    (cond
     [(eof-object? ch) ch]
     [(legal? ch) ch]
     [else (skip-read-char)])))

(define (bf-compress)
  (let loop ([ch (skip-read-char)]
             [run? #f]
             [count 0])
    (cond [(eof-object? ch)
           (if run?
               (bf-display (expand-char run? count))
               (bf-display (convert-char ch)))]
          [run?                         ; 今まで連続していた
           (cond [(cp-char=? run? ch)   ; まだまだ連続
                  (loop (skip-read-char) run? (+ (cp-char-score ch) count))]
                 [(compressable? ch)    ; 連続の起点
                  (bf-display (expand-char run? count)) ; 今までの
                  (loop (skip-read-char) ch (cp-char-score ch))]
                 [else                                  ; 普通に処理
                  (bf-display (expand-char run? count)) ; 今までの
                  (bf-display (convert-char ch))        ; 今の
                  (loop (skip-read-char) #f 0)])]
          [else                         ; 連続していない
           (cond [(compressable? ch)
                  (loop (skip-read-char) ch (cp-char-score ch))]
                 [else
                  (bf-display (convert-char ch))
                  (loop (skip-read-char) #f 0)])])))

(define (bf-raw-compile)
  (let ([ch (read-char)])
    (unless (eof-object? ch)
      (bf-display (convert-char ch))
      (bf-raw-compile))))

(define (bf-compile #!optional (optimize #t))
  (if optimize
      (bf-compress)
      (bf-raw-compile)))

(define (bf-display str)
  (display str)
  (when (bf-debug) (newline)))
