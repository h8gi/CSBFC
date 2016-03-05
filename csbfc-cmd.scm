;;; csbfc-cmd.scm
;;; command line tools
(use csbfc matchable irregex (only data-structures conc))
(define bf-outfile (make-parameter #f))


(define (bf-compile-file filename)
  (cond [(irregex-match "(.+)\.(?:bf|b)" filename) =>
         (lambda (m)
           (let* ([outfile (irregex-match-substring m 1)]
                  [scmfile (string-append (or (bf-outfile) outfile) "_bf.scm")])
             (with-output-to-file scmfile
               (lambda ()
                 (display "(use bf-lib)")
                 (newline)
                 (when (bf-debug) (display "(time ") (newline))
                 (with-input-from-file filename bf-compile)
                 (when (bf-debug) (display ") ;; end of time") (newline))))
             (system (conc "csc "
                           scmfile
                           " -o " (or (bf-outfile) outfile)))
             (unless (bf-debug) (system (conc "rm " scmfile)))))]
        [else (error "specify (.bf|.b) source file" filename)]))

(define (usage) (display
                 #<<END
csbfc - chicken scheme brainfuck compiler
Usage: csbfc <file> | <option> ...
    -d -debug           debug mode
    -h -help            display this text and exit
    -O <number>         enable certain sets of optimization options (0-3)
    -o <file>           write output to <file>

END

(current-error-port)
))

(define (main args)
  (match args
    [((or "-h" "-help") . rest) (usage)]
    [((or "-d" "-debug") . rest)
     (bf-debug #t)
     (main rest)]    
    [("-o" outfile . rest)
     (bf-outfile outfile)
     (main rest)]
    [("-O" (? string->number n) . rest)
     (bf-optimize (string->number n))
     (main rest)]
    [("-O" notnum . rest)
     (error "illegal optimize level" notnum)
     (exit 1)]
    ;; [(filename . rest)
    ;;  (main `(,@rest ,filename) (sub1 lim))]
    [(filename) (bf-compile-file filename)]
    [else (usage)]))

(main (command-line-arguments))
