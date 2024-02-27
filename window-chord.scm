;;;; Window Chord

;;; Copyright MMXXI-MMXXIII Arthur A. Gleckler.

;;; Licensed under MIT license.  See file "LICENSE".

;;; This assumes that monitors are laid out horizontally.

(define-record-type extents
    (make-extents left right top bottom)
    extents?
  (left xt/left)
  (right xt/right)
  (top xt/top)
  (bottom xt/bottom))

(define-record-type geometry
    (make-geometry x y width height)
    geometry?
  (x g/x)
  (y g/y)
  (width g/width)
  (height g/height))

(define (geometry= g1 g2)
  (define (compare key) (= (key g1) (key g2)))
  (and (compare g/width)
       (compare g/height)
       (compare g/x)
       (compare g/y)))

(define (centroid g)
  (values (+ (g/x g) (/ (g/width g) 2))
	  (+ (g/y g) (/ (g/height g) 2))))

(define (square x) (* x x))

(define (geometries-distance-metric g1)
  (let-values (((x1 y1) (centroid g1)))
    (lambda (g2)
      (let-values (((x2 y2) (centroid g2)))
	(+ (square (- x2 x1))
	   (square (- y2 y1)))))))

(define (normalize n) (if (integer? n) n (floor n)))

(define (stringify x)
  (cond ((number? x) (number->string (normalize x)))
	((string? x) x)
	((symbol? x) (symbol->string x))
	(else (error "Unexpected type." x))))

(define (split-at-commas string)
  (string-split string ", " 'suffix))

(define (split-lines string) (string-split string "\n" 'suffix))

(define (active-window)
  "Return a string that is the ID Of the active window."
  (car (split-lines (process->string '(xdotool getactivewindow)))))

(define (all-windows)
  "Return a list of strings that are the IDs of all windows."
  (map (lambda (l)
	 (number->string
	  (string->number (substring (car (string-split l " " 'suffix)) 2)
			 16)))
       (split-lines (process->string '(wmctrl "-l")))))

(define (same-monitor? window-1)
  (let ((mg (monitor-geometry window-1)))
    (lambda (window-2)
      "Return true iff `window-1' and `window-2' are on the same monitor."
      (geometry= mg (monitor-geometry window-2)))))

(define (all-windows-same-monitor window)
  "Return a list of strings that are the IDs of all windows on the same monitor
as `window'."
  (filter (same-monitor? window)
	  (all-windows)))

(define (class->windows class)
  (cond ((process->string `(xdotool search "--onlyvisible" "--class" ,class))
	 => split-lines
	(else #false))))

(define (geometry+extents g xt)
  (make-geometry (- (g/x g) (xt/left xt))
		 (g/y g)
		 (+ (g/width g) (xt/left xt) (xt/right xt))
		 (+ (g/height g) (xt/top xt) (xt/bottom xt))))

(define (negate-extents xt)
  (make-extents (- (xt/left xt))
		(- (xt/right xt))
		(- (xt/top xt))
		(- (xt/bottom xt))))

(define (window-geometry window)
  (let* ((lines (split-lines
		 (process->string
		  `(xdotool getwindowgeometry "-shell" ,window))))
	 (binding-rx (rx (-> name (+ upper-case))
			 "="
			 (-> value (+ any))))
	 (alist
	  (filter-map
	   (lambda (l)
	     (let ((m (regexp-matches binding-rx l)))
	       (and m
		    (cons (string->symbol
			   (string-downcase (regexp-match-submatch m 'name)))
			  (string->number (regexp-match-submatch m 'value))))))
	   lines)))
    (geometry+extents (apply make-geometry
			     (map (lambda (name) (cdr (assq name alist)))
				  '(x y width height)))
		      (window-extents window))))

(define (xrandr . arguments)
  (process->string `(xrandr ,@(map stringify arguments))))

(define monitor-geometry-alist
  (let ((alist
	 (delay
	   (let ((lines (filter (lambda (l) (string-contains l " connected"))
				(split-lines (xrandr "--query"))))
		 (geometry-rx (rx (-> name word)
				  (+ any)
				  (-> width (+ num))
				  "x"
				  (-> height (+ num))
				  "+"
				  (-> x (+ num))
				  "+"
				  (-> y (+ num))
				  (+ any))))
	     (map (lambda (l) (let ((m (regexp-matches geometry-rx l)))
			   (cons (regexp-match-submatch m 'name)
				 (apply make-geometry
					(map (lambda (name)
					       (string->number
						(regexp-match-submatch m name)))
					     '(x y width height))))))
		  lines)))))
    (lambda () (force alist))))

(define (xprop window name)
  (process->string `(xprop "-id" ,window ,name)))

(define (set-xprop! window name value)
  (system-for-effect 'xprop
		     `("-format" ,name "8s"
		       "-set" ,name ,value
		       "-id" ,window)))

(define (xprop-atoms window name)
  (let* ((value-rx (rx ,name
		       "(ATOM) = "
		       (-> value (+ (complement "\n")))
		       "\n"))
	 (m (regexp-matches value-rx (xprop window name))))
    (and m (split-at-commas (regexp-match-submatch m 'value)))))

(define (xprop-string window name)
  (let* ((value-rx (rx ,name
		       "(STRING) = "
		       "\""
		       (-> value (+ (complement "\"")))
		       "\"\n"))
	 (m (regexp-matches value-rx (xprop window name))))
    (and m (regexp-match-submatch m 'value))))

(define (xprop-symbol window name)
  (cond ((xprop-string window name) => string->symbol)
	(else #false)))

(define (xprop-extents name window transform)
  (let* ((extents-rx (rx ,name
			 "(CARDINAL) = "
			 (-> left (+ num))
			 ", "
			 (-> right (+ num))
			 ", "
			 (-> top (+ num))
			 ", "
			 (-> bottom (+ num))
			 "\n"))
	 (m (regexp-matches extents-rx (xprop window name))))
    (and m
	 (apply make-extents
		(map (lambda (name)
		       (transform
			(string->number (regexp-match-submatch m name))))
		     '(left right top bottom))))))

(define (frame-extents-gtk window)
  (xprop-extents "_GTK_FRAME_EXTENTS" window +))

(define (frame-extents-net window)
  (xprop-extents "_NET_FRAME_EXTENTS" window -))

(define (system-for-effect command arguments)
  (or (apply system? command (map stringify arguments))
      (error "Call failed." command)))

(define (xdotool . arguments)
  (system-for-effect "xdotool" arguments))

(define (wmctrl . arguments)
  (system-for-effect "wmctrl" arguments))

(define (max-monitor-width-height)
  (let* ((alist (monitor-geometry-alist)))
    (values (fold (lambda (a x) (max x (g/width (cdr a)))) 0 alist)
	    (fold (lambda (a x) (max x (g/height (cdr a)))) 0 alist))))

;; Using `max-monitor-width-height' here works around a bug that prevents
;; <wmctrl> from setting the height to a number higher than the vertical
;; resolution of the monitor, to account for window exents, when the width of
;; the window is not the full width of the monitor.
(define (wmctrl-mvarg geometry)
  (let-values (((width height) (max-monitor-width-height)))
    (string-join (map (lambda (n) (number->string (normalize n)))
		      `(0
			,(g/x geometry)
			,(g/y geometry)
			,(min width (g/width geometry))
			,(min height (g/height geometry))))
		 ",")))

(define set-window-geometry!
  (case-lambda
   ((window geometry)
    (wmctrl "-i"
	    "-r" window
	    "-e" (wmctrl-mvarg
		  (geometry+extents geometry (window-extents window)))))
   ((window x y width height)
    (set-window-geometry! window (make-geometry x y width height)))))

(define (window-extents window)
  (or (frame-extents-gtk window)
      (frame-extents-net window)
      (make-extents 0 0 0 0)))

(define (monitor-geometry window)
  (let* ((wg (window-geometry window))
	 (wx (+ (g/x wg) (/ (g/width wg) 2))))
    (cond ((find (lambda (a)
		   (let* ((mg (cdr a))
			  (x (g/x mg)))
		     (< x wx (+ x (g/width mg)))))
		 (monitor-geometry-alist))
	   => cdr)
	  (else (error "Unable to find geometry for window.")))))

(define (horizontal-geometry monitor-geometry left right vertical)
  (let* ((h (g/height monitor-geometry))
	 (mw (g/width monitor-geometry))
	 (ww (* (- right left) mw))
	 (wx (+ (g/x monitor-geometry) (* left mw))))
    (make-geometry wx
		   (* (- 1 vertical) h)
		   ww
		   (* vertical h))))

(define (horizontal-geometries fractions)
  (lambda (window)
    (let ((mg (monitor-geometry window)))
      (map (lambda (f) (apply horizontal-geometry mg f))
	   fractions))))

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

(define (rotate predicate elements)
  "Return the element of `elements' after the first one that satisfies
`predicate', or the first element of `elements'."
  (let ((tail (find-tail predicate elements)))
    (if (and tail (not (null? (cdr tail))))
	(cadr tail)
	(car elements))))



(define (window-column window)
  (let ((mg (monitor-geometry window))
	(left-x (g/x (window-geometry window))))
    (car (metric-minimizer `((left . ,(g/x mg))
			     (right . ,(+ (g/x mg) (/ (g/width mg) 2))))
			   (lambda (a) (abs (- (cdr a) left-x)))))))

(define (window-height window)
  (let ((mg (monitor-geometry window)))
    (if (< (/ (g/y (window-geometry window))
	      (g/height mg))
	   1/8)
	'tall
	'short)))

(define (next-geometry window geometries position)
  "If window's \"window-chord\" property matches `position', return the first
element of `geometries' after the one that has the minimum
`geometries-distance-metric' from window's geometry. wrapping to the beginning
if necessary.  Otherwise, return the first element of `geometries'."
  (let ((label (window-column window)))
    (if (and label (eq? label position))
	(let ((minimizer
	       (metric-minimizer geometries
				 (geometries-distance-metric
				  (window-geometry window)))))
	  (rotate (lambda (g) (eq? g minimizer)) geometries))
	(car geometries))))

(define (twist-window! window)
  "Toggle windows between 1/2-1/2 left-right configuration and 1/3-2/3
left-right configuration."
  (let ((h (/ (g/height (window-geometry window))
	      (g/height (monitor-geometry window)))))
    (case (window-column window)
      ((left)
       (set-window-geometry! window
			     (next-geometry window
					    ((horizontal-geometries
					      `((0 1/2 ,h)
						(0 1/3 ,h)))
					     window)
					    'left)))
      ((right)
       (set-window-geometry! window
			     (next-geometry window
					    ((horizontal-geometries
					      `((1/2 1 ,h)
						(1/3 1 ,h)))
					     window)
					    'right))))))

(define (twist active)
  (for-each twist-window! (all-windows-same-monitor active)))

(define short-fraction 7/8)

;; The `toggle-height' and `set-window-column!' procedures are more complicated
;; than one would expect because they work around bugs in <wmctrl> (and
;; <xdotool>) that cause the window to move horizontally unless the position is
;; specified explicitly.
(define (toggle-height window)
  (let* ((mg (monitor-geometry window))
	 (wg (window-geometry window))
	 (xt (window-extents window))
	 (height (+ (g/height mg) (xt/top xt) (xt/bottom xt)))
	 (fraction (case (window-height window)
		     ((short) 1)
		     (else short-fraction))))
    (set-window-geometry! window
			  (+ (g/x mg)
			     (case (window-column window)
			       ((left) 0)
			       (else (/ (g/width mg) 2))))
			  (* (- 1 fraction) height)
			  (g/width wg)
			  (* fraction height))))

(define (set-window-column! column)
  (lambda (window)
    (wmctrl "-i"
	    "-r" window
	    "-b" "remove,fullscreen,maximized_horz")
    (let* ((mg (monitor-geometry window))
	   (wg (window-geometry window))
	   (xt (window-extents window))
	   (height (+ (g/height mg) (xt/top xt) (xt/bottom xt))))
      (set-window-geometry! window
			    (+ (g/x mg)
			       (case column
				 ((left) 0)
				 (else (/ (g/width mg) 2))))
			    0
			    (/ (g/width mg) 2)
			    height))))

(define left (set-window-column! 'left))
(define right (set-window-column! 'right))

(define (maximize window)
  "Start with fullscreen, but switch to maximized just horizontally if
repeated."
  (let* ((wm-states (xprop-atoms window "_NET_WM_STATE"))
	 (fullscreen?
	  (and wm-states
	       (find (lambda (s) (string-contains s "_NET_WM_STATE_FULLSCREEN"))
		     wm-states))))
    (cond (fullscreen?
	   (wmctrl "-i"
		   "-r" window
		   "-b" "remove,fullscreen")
	   (wmctrl "-i"
		   "-r" window
		   "-b" "add,maximized_horz"))
	  (else (wmctrl "-i"
			"-r" window
			"-b" "remove,maximized_horz")
		(wmctrl "-i"
			"-r" window
			"-b" "add,fullscreen")))))

(define (other-monitor window)
  (let* ((mg1 (monitor-geometry window))
	 (mg2 (cdr (rotate (lambda (a) (geometry= (cdr a) mg1))
			   (monitor-geometry-alist)))))
    (set-window-geometry! window mg2)
    (case (window-column window)
      ((left) (left window))
      ((right) (right window))
      (else (maximize window)))))