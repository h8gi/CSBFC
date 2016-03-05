(use data-structures matchable ports srfi-1 (only traversal but-last))
;;; main.scm    main source file
(define bf-debug (make-parameter #f))
(define bf-optimize (make-parameter 2))

(define (convert-char char)
  (case char
    ;; ++(*ptr)
    [(#\+) "(bf-inc! 1)"]
    
    ;; --(*ptr)
    [(#\-) "(bf-dec! 1)"]

    ;; ++ptr
    [(#\>) "(bf-fd! 1)"]

    ;; --ptr
    [(#\<) "(bf-bk! 1)"]

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
          [(#\+ #\-) (if (positive? count) "bf-inc!" "bf-dec!")]
          [(#\> #\<) (if (positive? count) "bf-fd!" "bf-bk!")]
          [else (error "fuck up")])
        " " (abs count) ")"))

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

;;; stdin -> string
;;; まずbf文字列をrun-length圧縮するとともにS式に変換
(define (bf-compress)
  (with-output-to-string
      (lambda () (let loop ([ch (skip-read-char)]
                        [run? #f]
                        [count 0])
               (cond [(eof-object? ch)
                      (if run?
                          (display (expand-char run? count))
                          (display (convert-char ch)))]
                     [run?              ; 今まで連続していた
                      (cond [(cp-char=? run? ch) ; まだまだ連続
                             (loop (skip-read-char) run? (+ (cp-char-score ch) count))]
                            [(compressable? ch) ; 連続の起点
                             (display (expand-char run? count)) ; 今までの
                             (loop (skip-read-char) ch (cp-char-score ch))]
                            [else       ; 普通に処理
                             (display (expand-char run? count)) ; 今までの
                             (display (convert-char ch))        ; 今の
                             (loop (skip-read-char) #f 0)])]
                     [else              ; 連続していない
                      (cond [(compressable? ch)
                             (loop (skip-read-char) ch (cp-char-score ch))]
                            [else
                             (display (convert-char ch))
                             (loop (skip-read-char) #f 0)])])))))
;;; stdin -> string
(define (bf-raw-compile)
  (with-output-to-string
      (lambda () (let loop ([ch (read-char)])
               (unless (eof-object? ch)
                 (display (convert-char ch))
                 (loop (read-char)))))))



;;; S式へ変換 して 最適化 ------------------------------------------------------------

;;; 変換
(define (->sexp-seq str)
  (with-input-from-string str
    (lambda ()
      (let loop ([sexp (read)]
                 [acc '()])
        (if (eof-object? sexp)
            (reverse! acc)
            (loop (read) (cons sexp acc)))))))

(define (fd? x)
  (eq? 'bf-fd! (car x)))
(define (bk? x)
  (eq? 'bf-bk! (car x)))
(define (inc? x)
  (eq? 'bf-inc! (car x)))
(define (dec? x)
  (eq? 'bf-dec! (car x)))
(define (while? x)
  (eq? 'bf-while (car x)))
(define (val x)
  (cadr x))
(define (once-dec? x)
  (and (dec? x) (= 1 (val x))))
(define (clear? x)
  (eq? 'bf-clear (car x)))

;;; loop の 最適化
;;; 頭かおしりで - しているかどうか
(define (once-dec-loop? while-body)
  (cond [(once-dec? (car while-body))
         (cdr while-body)]
        [(once-dec? (last while-body))
         (but-last while-body)]
        [else #f]))

(define (scan-copy-loop while-body)
  (call/cc
   (lambda (k)
     (let loop ([lst while-body]
                [fdcount 0]
                [bkcount 0]
                [wait? #f]
                [acc '()])
       (if (null? lst)
           (k #f)
           (let ([x (car lst)])
             (cond
     
              [(and (inc? x) wait?)
               (loop (cdr lst)
                     fdcount
                     bkcount
                     #f
                     (cons `(bf-copy ,(- fdcount bkcount) ,(val x))
                           acc))]
              [(and (dec? x) wait?)
               (loop (cdr lst)
                     fdcount
                     bkcount
                     #f
                     (cons `(bf-copy ,(- fdcount bkcount) ,(- (val x)))
                           acc))]
              
              [(and (bk? x) (null? (cdr lst)) (= (+ (val x) bkcount) fdcount))
               (reverse! (cons '(bf-clear) acc))]         
              [(and (fd? x) (null? (cdr lst)) (= (+ (val x) fdcount) bkcount))
               (reverse! (cons '(bf-clear) acc))]
              
              [(fd? x)
               (loop (cdr lst)
                     (+ fdcount (val x))
                     bkcount
                     #t
                     acc)]
              [(bk? x)     
               (loop (cdr lst)
                     fdcount
                     (+ bkcount (val x))
                     #t
                     acc)]
              [(clear? x)
               (loop (cdr lst)
                     fdcount
                     bkcount
                     #f
                     (cons `(bf-clear-off ,(- fdcount bkcount))
                           acc))]
              [else (k #f)])))))))


(define (compile-while sexp)
  (match sexp
    ;; [-] などを (bf-clear) に変換
    ;; [+]もどうせ無限ループなのでclearする
    [('bf-while ((or 'bf-inc!
                     'bf-dec!) n))
     '(bf-clear)]
    ;; []ループの除去
    [('bf-while) '(void)]
    [('bf-while . while-body)
     (let ([body (once-dec-loop? while-body)])
       (if body
           (let ([scaned (scan-copy-loop body)])
             (if scaned
                 (cons 'bf-begin scaned)
                 sexp))
           sexp))]
    [else sexp]))

;;; whileに対しては再帰?
(define (compile-sexp-rec sexp)
  (cond [(and (pair? sexp) (while? sexp))
         (compile-while (map compile-sexp-rec sexp))]
        [else sexp]))

;;; 標準入力から標準出力へ
(define (bf-compile)
  ((if (bf-debug) pp display)
   (cons 'begin (case (bf-optimize)
                  [(#f 0) (->sexp-seq (bf-raw-compile))]
                  [(1)    (->sexp-seq (bf-compress))]
                  [else   (map compile-sexp-rec (->sexp-seq (bf-compress)))]))))

;;; debug 用
(define (compile-string str)
  (with-input-from-string str
    bf-compile))
