;;;; Window Chord

;;; Copyright MMXXI-MMXXII Arthur A. Gleckler.

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

(define (split-lines string) (string-split string "\n" 'suffix))

(define (active-window)
  (car (split-lines (process->string '(xdotool getactivewindow)))))

(define (class->windows class)
  (cond ((process->string `(xdotool search "--onlyvisible" "--class" ,class))
	 => split-lines
	(else #false))))

(define (geometry+extents g xt)
  (make-geometry (+ (g/x g) (xt/left xt))
		 (+ (g/y g) (xt/top xt))
		 (- (g/width g) (xt/left xt) (xt/right xt))
		 (- (g/height g) (xt/top xt) (xt/bottom xt))))

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
		  (geometry+extents geometry
				    (negate-extents (window-extents window))))))
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

(define (next-geometry window geometries position)
  "If window's \"window-chord\" property matches `position', return the first
element of `geometries' after the one that has the minimum
`geometries-distance-metric' from window's geometry. wrapping to the beginning
if necessary.  Otherwise, return the first element of `geometries'."
  (let ((label (xprop-symbol window "WINDOW_CHORD")))
    (if (and label (eq? label position))
	(let ((minimizer
	       (metric-minimizer geometries
				 (geometries-distance-metric
				  (window-geometry window)))))
	  (rotate (lambda (g) (eq? g minimizer)) geometries))
	(car geometries))))

(define (next-geometry! position geometries)
  (lambda (window)
    "Set the geometry of window to `(next-geometry window position (geometries
window))' and its \"window-chord\" property to `position'."
    (set-window-geometry! window
			  (next-geometry window (geometries window) position))
    (set-xprop! window "WINDOW_CHORD" (symbol->string position))))

(define left
  (next-geometry! 'left
		  (horizontal-geometries '((0 1/2 1)
					   (0 1/2 3/4)
					   (0 1/3 3/4)
					   (0 1/3 1)))))
(define right
  (next-geometry! 'right
		  (horizontal-geometries '((1/2 1 1)
					   (1/2 1 3/4)
					   (1/3 1 3/4)
					   (1/3 1 1)))))
(define maximize
  (next-geometry! 'full-width
		  (horizontal-geometries '((0 1 1)
					   (0 1 3/4)))))

(define (other-monitor window)
  (let* ((mg1 (monitor-geometry window))
	 (mg2 (cdr (rotate (lambda (a) (geometry= (cdr a) mg1))
			   (monitor-geometry-alist)))))
    (set-window-geometry! window mg2)
    (case (xprop-symbol window "WINDOW_CHORD")
      ((left) (left window))
      ((right) (right window))
      (else (maximize window)))))