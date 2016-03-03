;;; csbfc-cmd.scm
;;; command line tools
(use csbfc matchable irregex (only data-structures conc))

(define (bf-compile-file filename)  
  (cond [(irregex-match "(.+)\.bf" filename) =>
         (lambda (m)
           (let* ([body (irregex-match-substring m 1)]
                  [outfile (string-append body "_bf.scm")])
             (with-output-to-file outfile
               (lambda ()
                 (display "(use bf-lib)")
                 (with-input-from-file filename
                   bf-compile)))
             (system (conc "csc " outfile " -o " body))
             ;(system (conc "rm " outfile))
             ))]
        [else (error "specify .bf source file" filename)]))

(define (usage) (display
                 #<<END
csbfc - chicken scheme brainfuck compiler
Usage: csbfc FILENAME | OPTION ...
    -h -help        display this text and exit

END

(current-error-port)
))

(match (command-line-arguments)
  [("-h") (usage)]
  [("-help") (usage)]
  [(filename) (bf-compile-file filename)]
  [else (usage)])

