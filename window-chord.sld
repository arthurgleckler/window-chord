;;;; Window Chord (Library)

;;; Copyright MMXXI-MMXXII Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

(define-library (window-chord)
  (export active-window bottom left-half maximize other-monitor right-half
	  short tall top)
  (import (chibi assert))
  (import (chibi process))
  (import (scheme small))
  (import (only (srfi 1) filter filter-map find find-tail fold))
  (import (only (srfi 16) case-lambda))
  (import (srfi 115))
  (import (only (srfi 130) string-contains string-join string-split))
  (include "window-chord.scm"))