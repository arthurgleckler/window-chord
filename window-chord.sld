;;;; Window Chord (Library)

;;; Copyright MMXXI-MMXXVI Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

(define-library (window-chord)
  (export active-window
	  all-windows
	  all-windows-same-monitor
	  json-ref
	  left
	  maximize
	  monitor-names
	  reset-windows
	  right
	  switch-monitor
	  switch-monitor-all
	  toggle-height
	  twist
	  wch-act!
	  wch-invoke
	  wch-query
	  window-column
	  window-monitor)
  (import (chibi process)
	  (chibi json)
	  (only (chibi string)
		call-with-input-string)
	  (scheme small)
	  (only (srfi 1) filter-map)
	  (only (srfi 16) case-lambda)
	  (only (srfi 130) string-contains))
  (include "window-chord.scm"))