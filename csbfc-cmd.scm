;;; csbfc-cmd.scm
;;; command line tools
(use csbfc matchable irregex (only data-structures conc))

(define bf-outfile (make-parameter #f))
(define (bf-compile-file filename)
  (cond [(irregex-match "(.+)\.bf" filename) =>
         (lambda (m)
           (let* ([body (irregex-match-substring m 1)]
                  [outfile (string-append (or (bf-outfile) body) "_bf.scm")])
             (with-output-to-file outfile
               (lambda ()
                 (display "(use bf-lib)")
                 (with-input-from-file filename
                   (lambda () (bf-compile)))))
             (system (conc "csc " (if (bf-debug) "-profile " "") outfile " -o " (or (bf-outfile) body)))
             ;(system (conc "rm " outfile))
             ))]
        [else (error "specify .bf source file" filename)]))

(define (usage) (display
                 #<<END
csbfc - chicken scheme brainfuck compiler
Usage: csbfc FILENAME | OPTION ...
    -d -debug           debug mode
    -h -help            display this text and exit
    -n -no-optimize     no optimize
    -o <file>       write output to <file>
END

(current-error-port)
))

(define (main args)
  (match args
    [((or "-h" "-help") . rest) (usage)]
    [((or "-d" "-debug") . rest)
     (bf-debug #t)
     (main rest)]
    [((or "-n" "-no-optimize") . rest)
     (bf-optimize #f)
     (main rest)]
    [("-o" outfile . rest)
     (bf-outfile outfile)
     (main rest)]
    [(filename) (bf-compile-file filename)]
    [else (usage)]))

(main (command-line-arguments))

