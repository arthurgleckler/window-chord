;;;; Window Chord

;;; Copyright MMXXI-MMXXVI Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

;;; This assumes that monitors are laid out horizontally.

;; A single window-chord-helper subprocess is kept running across all requests
;; so that (a) we avoid per-call fork+exec overhead and (b) we can pipeline
;; multiple requests (e.g. un-maximize, re-query extents, move) within a single
;; X11 connection.

(define-values
    (wch-pid wch-in wch-out wch-err)
  (call-with-process-io (list "window-chord-helper") values))

(define (wch-invoke json-input)
  "Send a JSON request to the persistent helper and read the response."
  (json-write json-input wch-in)
  (newline wch-in)
  (flush-output-port wch-in)
  (call-with-input-string (read-line wch-out) json-read))

(define (wch-act! operations)
  "Execute operations via window-chord-helper"
  (wch-invoke `((command . "act")
                (operations . ,operations))))

(define (wch-query query)
  "Query window-chord-helper with include specification"
  (wch-invoke `((command . "query")
                (include . ,query))))

(define (window-decimal hex-id)
  "Convert a hex window ID string with \"0x\" prefix to a decimal."
  (number->string (string->number (substring hex-id 2) 16)))

(define (window-hex decimal-id)
  "Convert a decimal window ID string to a hex string with \"0x\" prefix."
  (string-append "0x" (number->string (string->number decimal-id) 16)))

(define (json-ref obj key)
  "Get value from JSON object (alist)."
  (cdr (assoc key obj)))

(define (active-window)
  "Return a string that is the ID of the active window."
  (let ((result (wch-query '((active_window . #true)))))
    (window-decimal (json-ref result 'active_window))))

(define (all-windows*)
  (json-ref (wch-query '((all_windows . #true))) 'windows))

(define all-windows
  (case-lambda
    (()
     (vector->list
      (vector-map (lambda (w) (window-decimal (json-ref w 'id)))
                  (all-windows*))))
    ((monitor)
     (let* ((mg (json-ref monitor 'geometry))
            (mx (json-ref mg 'x))
            (mw (json-ref mg 'width)))
       (filter-map
        (lambda (w)
          (let* ((wg (json-ref w 'geometry))
                 (cx (+ (json-ref wg 'x) (/ (json-ref wg 'width) 2))))
            (and (< mx cx (+ mx mw))
                 (window-decimal (json-ref w 'id)))))
        (vector->list (all-windows*)))))))

(define (monitor-names)
  "Return a list of monitor name strings."
  (map (lambda (m) (json-ref m 'name))
       (vector->list
        (json-ref (wch-query '((monitors . #true))) 'monitors))))

(define (window-monitor window)
  "Return the monitor object containing `window'."
  (let* ((result (wch-query `((window_details . #(,(window-hex window)))
                              (monitors . #true))))
         (window-data (vector-ref (json-ref result 'windows) 0))
         (wg (json-ref window-data 'geometry))
         (cx (+ (json-ref wg 'x) (/ (json-ref wg 'width) 2)))
         (monitors (json-ref result 'monitors)))
    (vector-ref monitors (find-monitor monitors cx))))

(define (all-windows-same-monitor window)
  "Return a list of strings that are the IDs of all windows on the same monitor
as `window'."
  (all-windows (window-monitor window)))

(define (find-monitor monitors x)
  "Return the index in `monitors' of the monitor containing horizontal
position `x', defaulting to 0."
  (let loop ((i 0))
    (if (>= i (vector-length monitors))
        0
        (let* ((m (vector-ref monitors i))
               (mg (json-ref m 'geometry))
               (mx (json-ref mg 'x))
               (mw (json-ref mg 'width)))
          (if (< mx x (+ mx mw))
              i
              (loop (+ i 1)))))))

(define (metric-minimizer elements measure)
  (let next ((minimizer (car elements))
	     (minimum (measure (car elements)))
	     (remaining (cdr elements)))
    (if (null? remaining)
	minimizer
	(let ((minimum* (measure (car remaining))))
	  (if (< minimum* minimum)
	      (next (car remaining)
		    minimum*
		    (cdr remaining))
	      (next minimizer
		    minimum
		    (cdr remaining)))))))

(define (window-column window)
  (let* ((result (wch-query `((window_details . #(,(window-hex window)))
                              (monitors . #true))))
         (window-data (vector-ref (json-ref result 'windows) 0))
         (wg (json-ref window-data 'geometry))
         (monitor (vector-ref (json-ref result 'monitors) 0))
         (mg (json-ref monitor 'geometry))
         (mw (json-ref mg 'width))
         (mx (json-ref mg 'x))
         (w-center-x (+ (json-ref wg 'x) (/ (json-ref wg 'width) 2))))
    (car (metric-minimizer `((left . ,(+ mx (* mw 1/4)))
                             (maximized . ,(+ mx (/ mw 2)))
                             (right . ,(+ mx (* mw 3/4))))
                           (lambda (a) (abs (- (cdr a) w-center-x)))))))

(define (reset-window! window)
  "Reset `window' to left, maximized, or right, depending on where it is
already.  This helps when switching monitors leaves the windows misplaced."
  (case (window-column window)
    ((left) (left window))
    ((maximized) (maximize window))
    ((right) (right window))))

(define (reset-windows active)
  (for-each reset-window! (all-windows-same-monitor active)))

(define (twist-window! window)
  "Toggle `window' between 1/2-1/2 left-right configuration and 1/3-2/3
left-right configuration."
  (let* ((result (wch-query `((window_details . #(,(window-hex window)))
                              (monitors . #true))))
         (window-data (vector-ref (json-ref result 'windows) 0))
         (wg (json-ref window-data 'geometry))
         (we (json-ref window-data 'extents))
         (csd-width (+ (json-ref we 'left) (json-ref we 'right)))
         (wx (json-ref wg 'x))
         (ww (json-ref wg 'width))
         (wh (json-ref wg 'height))
         (w-center-x (+ wx (/ ww 2)))
         (monitors (json-ref result 'monitors))
         (monitor (vector-ref monitors (find-monitor monitors w-center-x)))
         (mg (json-ref monitor 'geometry))
         (mw (json-ref mg 'width))
         (mh (json-ref mg 'height))
         (mx (json-ref mg 'x))
         (h (/ wh mh))
         (column (car (metric-minimizer
		       `((left . ,(+ mx (* mw 1/4)))
                         (maximized . ,(+ mx (/ mw 2)))
                         (right . ,(+ mx (* mw 3/4))))
                       (lambda (a) (abs (- (cdr a) w-center-x))))))
         (content-width (- ww csd-width))
         (target-width
          (case column
            ((left)
             (let ((current-fraction (/ content-width mw)))
               (if (< (abs (- current-fraction 1/2)) 0.01)
                   (/ mw 3)		; Currently 1/2.  Switch to 1/3.
                   (/ mw 2))))		; Currently 1/3.  Switch to 1/2.
            ((right)
             (let ((current-fraction (/ content-width mw)))
               (if (< (abs (- current-fraction 1/2)) 0.01)
                   (* 2 (/ mw 3))	; Currently 1/2.  Switch to 2/3.
                   (/ mw 2))))		; Currently 2/3.  Switch to 1/2.
            (else ww)))
         (target-x
          (case column
            ((left) mx)
            ((right) (+ mx (- mw target-width)))
            (else wx))))
    (wch-act! (list->vector
               `(((geometry . ((x . ,target-x)
                               (y . ,(json-ref wg 'y))
                               (width . ,target-width)
                               (height . ,(* h mh))))
                  (type . "set_window_geometry")
                  (window . ,(window-hex window))))))))

(define (twist active)
  (for-each twist-window! (all-windows-same-monitor active)))

(define short-fraction 7/8)

(define (normalize n) (if (integer? n) n (floor n)))

(define (toggle-height window)
  (let* ((result (wch-query `((window_details . #(,(window-hex window)))
                              (monitors . #true))))
         (window-data (vector-ref (json-ref result 'windows) 0))
         (wg (json-ref window-data 'geometry))
         (wx (json-ref wg 'x))
         (wy (json-ref wg 'y))
         (ww (json-ref wg 'width))
         (w-center-x (+ wx (/ ww 2)))
         (monitors (json-ref result 'monitors))
         (monitor (vector-ref monitors (find-monitor monitors w-center-x)))
         (mg (json-ref monitor 'geometry))
         (mx (json-ref mg 'x))
         (mw (json-ref mg 'width))
         (mh (json-ref mg 'height))
         (height mh)
         (wh (json-ref wg 'height))
         (is-short (< (abs (- (/ wh mh) short-fraction))
                      (abs (- (/ wh mh) 1))))
         (fraction (if is-short 1 short-fraction))
         (column (car (metric-minimizer
		       `((left . ,(+ mx (* mw 1/4)))
                         (maximized . ,(+ mx (/ mw 2)))
                         (right . ,(+ mx (* mw 3/4))))
                       (lambda (a) (abs (- (cdr a) w-center-x))))))
         (x (+ mx (case column
                    ((left) 0)
                    ((maximized) (/ mw 2))
                    ((right) (/ mw 2)))))
         (y (normalize (* (- 1 fraction) height)))
         (h (normalize (* fraction height))))
    (wch-act! (list->vector
               `(((type . "set_window_geometry")
                  (window . ,(window-hex window))
                  (geometry . ((x . ,x)
                               (y . ,y)
                               (width . ,ww)
                               (height . ,h)))))))))

(define (set-window-column! column)
  (lambda (window)
    (let* ((result (wch-query `((active_window . #true)
                                (window_details . #(,(window-hex window)))
                                (monitors . #true))))
           (window-data (vector-ref (json-ref result 'windows) 0))
           (wg (json-ref window-data 'geometry))
           (wx (json-ref wg 'x))
           (ww (json-ref wg 'width))
           (w-center-x (+ wx (/ ww 2)))
           (monitors (json-ref result 'monitors))
           (monitor (vector-ref monitors (find-monitor monitors w-center-x)))
           (mg (json-ref monitor 'geometry))
           (mx (json-ref mg 'x))
           (mw (json-ref mg 'width))
           (mh (json-ref mg 'height))
           (height mh)
           (x (+ mx (case column
                      ((left) 0)
                      (else (/ mw 2)))))
           (width (/ mw 2)))
      (wch-act! (list->vector
                 `(((type . "set_window_geometry")
                    (window . ,(window-hex window))
                    (geometry . ((x . ,x)
				 (y . 0)
				 (width . ,width)
				 (height . ,height))))))))))

(define left (set-window-column! 'left))
(define right (set-window-column! 'right))

(define (maximize window)
  "Start with maximized horizontally, but switch to full-screen mode if
repeated."
  (let* ((result (wch-query `((window_details . #(,(window-hex window))))))
         (window-data (vector-ref (json-ref result 'windows) 0))
         (states (json-ref window-data 'states))
         (maximized-horizontally?
          (and states
               (let loop ((i 0))
                 (cond ((>= i (vector-length states)) #false)
                       ((string-contains (vector-ref states i)
					 "_NET_WM_STATE_MAXIMIZED_HORZ"))
                       (else (loop (+ i 1))))))))
    (cond (maximized-horizontally?
           (wch-act! (list->vector
                      `(((type . "set_window_state")
                         (window . ,(window-hex window))
                         (remove_states . #("_NET_WM_STATE_MAXIMIZED_HORZ"))
                         (add_states . #("_NET_WM_STATE_FULLSCREEN")))))))
          (else
           (wch-act! (list->vector
                      `(((type . "set_window_state")
                         (window . ,(window-hex window))
                         (remove_states . #("_NET_WM_STATE_FULLSCREEN"))
                         (add_states . #("_NET_WM_STATE_MAXIMIZED_HORZ"))))))))))

(define switch-monitor
  (case-lambda
    (() (switch-monitor (active-window)))
    ((window)
     (let* ((result (wch-query `((window_details . #(,(window-hex window)))
                                 (monitors . #true))))
            (window-data (vector-ref (json-ref result 'windows) 0))
            (wg (json-ref window-data 'geometry))
            (wx (json-ref wg 'x))
            (ww (json-ref wg 'width))
            (w-center-x (+ wx (/ ww 2)))
            (monitors (json-ref result 'monitors))
            (i (find-monitor monitors w-center-x))
            (other-monitor-idx (modulo (+ i 1) (vector-length monitors)))
            (other-monitor (vector-ref monitors other-monitor-idx))
            (other-mg (json-ref other-monitor 'geometry))
            (current-monitor (vector-ref monitors i))
            (current-mg (json-ref current-monitor 'geometry))
            (current-mx (json-ref current-mg 'x))
            (current-mw (json-ref current-mg 'width))
            (column (car (metric-minimizer
			  `((left . ,(+ current-mx (* current-mw 1/4)))
                            (maximized . ,(+ current-mx (/ current-mw 2)))
                            (right . ,(+ current-mx (* current-mw 3/4))))
                          (lambda (a) (abs (- (cdr a) w-center-x))))))
            (target-mx (json-ref other-mg 'x))
            (target-mw (json-ref other-mg 'width))
            (target-mh (json-ref other-mg 'height))
            (height target-mh))
       (case column
         ((left)
          (wch-act! (list->vector
                     `(((type . "set_window_geometry")
			(window . ,(window-hex window))
			(geometry . ((x . ,target-mx)
                                     (y . 0)
                                     (width . ,(/ target-mw 2))
                                     (height . ,height))))))))
         ((right)
          (wch-act! (list->vector
                     `(((type . "set_window_geometry")
			(window . ,(window-hex window))
			(geometry . ((x . ,(+ target-mx (/ target-mw 2)))
                                     (y . 0)
                                     (width . ,(/ target-mw 2))
                                     (height . ,height))))))))
         (else
          (wch-act! (list->vector
                     `(((type . "set_window_geometry")
			(window . ,(window-hex window))
			(geometry . ((x . ,target-mx)
                                     (y . 0)
                                     (width . ,(/ target-mw 2))
                                     (height . ,height))))
                       ((type . "set_window_state")
			(window . ,(window-hex window))
			(remove_states . #("_NET_WM_STATE_FULLSCREEN"))
			(add_states . #("_NET_WM_STATE_MAXIMIZED_HORZ"))))))))))))

(define (switch-monitor-all monitor-name)
  "Move all windows to the monitor named `monitor-name'."
  (let* ((result (wch-query `((all_windows . #true)
                              (monitors . #true))))
         (monitors (json-ref result 'monitors))
         (target-monitor
          (let loop ((i 0))
            (and (< i (vector-length monitors))
                 (let ((m (vector-ref monitors i)))
                   (if (string-contains (json-ref m 'name) monitor-name)
                       m
                       (loop (+ i 1)))))))
         (target-mg (and target-monitor (json-ref target-monitor 'geometry))))
    (when target-mg
      (let ((all-wins (all-windows))
            (target-mx (json-ref target-mg 'x))
            (target-mw (json-ref target-mg 'width))
            (target-mh (json-ref target-mg 'height)))
        (for-each
         (lambda (w)
           (wch-act! (list->vector
                      `(((type . "set_window_geometry")
                         (window . ,(window-hex w))
                         (geometry . ((x . ,target-mx)
                                      (y . 0)
                                      (width . ,(/ target-mw 2))
                                      (height . ,target-mh))))))))
         all-wins)))))