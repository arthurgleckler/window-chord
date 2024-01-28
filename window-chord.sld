;;;; Window Chord (Library)

;;; Copyright MMXXI-MMXXIV Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

(define-library (window-chord)
  (export active-window
	  left
	  maximize
	  other-monitor
	  right
	  toggle-height
	  twist)
  (import (chibi assert)
	  (chibi process)
	  (scheme small)
	  (only (srfi 1) filter filter-map find find-tail fold)
	  (only (srfi 16) case-lambda)
	  (srfi 115)
	  (only (srfi 130) string-contains string-join string-split))
  (include "window-chord.scm"))