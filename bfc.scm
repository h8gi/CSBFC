#!/usr/bin/env csi
(use srfi-14 utils)
(define *pinc-c* #\>)
(define *pdec-c* #\<)
(define *vinc-c* #\+)
(define *vdec-c* #\-)
(define *put-c* #\.)
(define *get-c* #\,)
(define *left-c* #\[)
(define *right-c* #\])
(define *token-char-set*
  (list->char-set
   (list *pinc-c* *pdec-c* *vinc-c* *vdec-c*
         *put-c* *get-c*
         *left-c* *right-c*)))
;;; ファイル文字列をトークンのリストへ
(define (bf-read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (string->list
       (string-filter
        *token-char-set* (read-all in))))))

;;; トークンのリストを文字列に
(define (bf-compile tokens-list)
  (if (null? tokens-list) ""
      (let ((token (car tokens-list)))
        (string-append
         (cond ((eq? token *pinc-c*)
                "(pointer-inc!) ")
               ((eq? token *pdec-c*)
                "(pointer-dec!) ")
               ((eq? token *vinc-c*)
                "(current-value-inc!) ")
               ((eq? token *vdec-c*)
                "(current-value-dec!) ")
               ((eq? token *put-c*)
                "(emit) ")
               ((eq? token *get-c*)
                "(get-c) ")
               ((eq? token *left-c*)
                "(while (not (zero? (current-value))) ")
               ((eq? token *right-c*)
                ") ")
               (else ""))
         (bf-compile (cdr tokens-list))))))

(define (bf-compile-file filename)
  (with-output-to-file (string-append (car (string-split filename "."))
                                      ".scm")
    (lambda ()
      (display
       "(define-syntax pop!
          (syntax-rules ()
            ((_ LOC)
             (let ((a (car LOC)))
               (set! LOC (cdr LOC))
               a))))
       (define-syntax push!
         (syntax-rules ()
           ((_ X LOC)
            (set! LOC (cons X LOC)))))
       (define-syntax while
         (syntax-rules ()
           ((_ TEST BODY ...)
            (let loop ()
              (if TEST
                  (begin BODY ... (loop)))))))
        (define *cells-depth* 1000)
        (define *cells* (make-vector *cells-depth* 0))
        (define *pointer* 0)
        (define (current-value)
          (vector-ref *cells* *pointer*))
        (define (pointer-inc!)
          (if (< *pointer* (- *cells-depth* 1))
              (set! *pointer* (+ *pointer* 1))
              (error \"cells over flow\")))
        (define (pointer-dec!)
          (if (zero? *pointer*)
              (error \"cells under flow\")
              (set! *pointer* (- *pointer* 1))))
        (define (current-value-inc!)
          (vector-set! *cells* *pointer* (+ (current-value) 1)))
        (define (current-value-dec!)
          (vector-set! *cells* *pointer* (- (current-value) 1)))
        (define (emit)
          (display (integer->char (current-value))))
        (define (get-c)
          (vector-set! *cells* *pointer* (char->integer (read-char))))")
      (display (bf-compile (bf-read-file filename))))))

(let ((filename (car (command-line-arguments))))
  (bf-compile-file filename)
  (system (string-append "csc " (car (string-split filename ".")) ".scm"))
  )

