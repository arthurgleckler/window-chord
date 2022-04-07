;;;; Window Chord (Library)

;;; Copyright MMXXI-MMXXII Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

(define-library (window-chord)
  (export active-window half-screen left-half maximize right-half)
  (import (chibi process))
  (import (scheme small))
  (import (srfi 130))
  (include "window-chord.scm"))