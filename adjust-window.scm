;; <> Get these from the root window using <wmctrl -d>.
(define (root-height) 1080)
(define (root-width) 1920)
;;;; Window Chord

;;; Copyright MMXXI-MMXXII Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

(define-record-type extents
    (make-extents left right top bottom)
    extents?
  (bottom xt/bottom)
  (left xt/left)
  (right xt/right)
  (top xt/top))

(define (active-window)
  (process->string '("xdotool" "getactivewindow")))

(define (xprop name window)
  (process->string `("xprop" "-id" ,window ,name)))

(define (xprop-extents name window transform)
  (let* ((output (xprop name window))
	 (=-cursor (string-index output (lambda (c) (char=? c #\=)))))
    (and (not (string-cursor=? =-cursor (string-cursor-end output)))
	 (apply make-extents
		(map (lambda (s) (transform (string->number (string-trim s))))
		     (string-split
		      (string-copy/cursors
		       output
		       (string-cursor-forward output =-cursor 1))
		      ","))))))

(define (frame-extents-gtk window)
  (xprop-extents "_GTK_FRAME_EXTENTS" window +))

(define (frame-extents-net window)
  (xprop-extents "_NET_FRAME_EXTENTS" window -))

(define (system-for-effect command arguments)
  (or (apply system? command arguments)
      (error "Call failed." command)))

(define (xdotool . arguments)
  (define (stringify x)
    (cond ((number? x) (number->string x))
	  ((string? x) x)
	  ((symbol? x) (symbol->string x))
	  (else (error "Unexpected type." x))))
  (system-for-effect "xdotool"
		     (map stringify arguments)))

(define (window-extents window)
  (or (frame-extents-gtk window)
      (frame-extents-net window)
      (make-extents 0 0 0 0)))

(define (half-screen extents window)
  (xdotool 'windowsize
	   window
	   (+ (/ (root-width) 2)
	      (xt/left extents)
	      (xt/right extents))
	   (+ (root-height)
	      (xt/top extents)
	      (xt/bottom extents))))

(define (left-half window)
  (let ((extents (window-extents window)))
    (half-screen extents window)
    (xdotool 'windowmove
	     window
	     (- (xt/left extents))
	     0)))

(define (right-half window)
  (let ((extents (window-extents window)))
    (half-screen extents window)
    (xdotool 'windowmove
	     window
	     (+ (/ (root-width) 2)
		(- (xt/left extents)))
	     0)))

(define (maximize window)
  (let ((extents (window-extents window)))
    (xdotool 'windowmove window 0 0)
    (xdotool 'windowsize
	     window
	     (+ (root-width)
		(xt/left extents)
		(xt/right extents))
	     (+ (root-height)
		(xt/top extents)
		(xt/bottom extents)))))