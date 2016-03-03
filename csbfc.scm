;;; csbfc.scm
(module csbfc
    (bf-compile bf-debug bf-optimize)
  (import scheme chicken data-structures ports srfi-1 extras)
  (include "main.scm"))
