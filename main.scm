(use data-structures matchable ports srfi-1)
;;; main.scm    main source file
(define bf-debug (make-parameter #f))
(define bf-optimize (make-parameter #t))

(define (convert-char char)
  (case char
    ;; ++(*ptr)
    [(#\+) "(bf-inc! 1)"]
    
    ;; --(*ptr)
    [(#\-) "(bf-inc! -1)"]

    ;; ++ptr
    [(#\>) "(bf-fd! 1)"]

    ;; --ptr
    [(#\<) "(bf-fd! -1)"]

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

(define (bf-display str)
  (display str)
  (when (bf-debug) (newline)))

;;; S式への変換 それに対する最適化
(define (->sexp str)
  (with-input-from-string str
    (lambda ()
      (let loop ([sexp (read)]
                 [acc '()])
        (if (eof-object? sexp)
            (reverse! acc)
            (loop (read) (cons sexp acc)))))))

(define (compile-sexp sexp)
  (match sexp
    [('bf-while ('bf-inc! (? negative? n))) ; [-] などを (bf-clear) に変換
     '(bf-clear)]
    [else sexp]))

;;; 標準入力から標準出力へ
(define (bf-compile)
  (if (bf-optimize)
      (let ([str (with-output-to-string bf-compress)])
        (for-each (lambda (sexp) (bf-display (compile-sexp sexp)))
                  (->sexp str)))
      (bf-raw-compile)))
