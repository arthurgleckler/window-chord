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

(define (xprop name window)
  (process->string `(xprop "-id" ,window ,name)))

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
	 (m (regexp-matches extents-rx (xprop name window))))
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

;; The use of `max-monitor-width-height' here works around a bug that prevents
;; <wmctrl> from setting the height to a number higher than the vertical
;; resolution of the monitor, to account for window exents, when the width of
;; the window is not the full width of the monitor.
(define (wmctrl-mvargs geometry)
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
	    "-e" (wmctrl-mvargs
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
		     (<= x wx (+ x (g/width mg)))))
		 (monitor-geometry-alist))
	   => cdr)
	  (else (error "Unable to find geometry for window.")))))

(define (mark-fullscreen window)
  (wmctrl "-i" "-r" window "-b" "add,fullscreen"))

(define (mark-not-fullscreen window)
  (wmctrl "-i" "-r" window "-b" "remove,fullscreen,maximized_horz")
  (wmctrl "-i" "-r" window "-b" "remove,maximized_vert"))

(define (short window)
  (let ((mg (monitor-geometry window))
	(wg (window-geometry window)))
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (g/x wg)
			  (* 1/4 (g/height mg))
			  (g/width wg)
			  (* 3/4 (g/height mg)))))

(define (tall window)
  (let ((mg (monitor-geometry window))
	(wg (window-geometry window)))
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (g/x wg)
			  0
			  (g/width wg)
			  (g/height mg))))

(define (bottom window)
  (let ((mg (monitor-geometry window))
	(wg (window-geometry window)))
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (g/x wg)
			  (- (g/height mg) (g/height wg))
			  (g/width wg)
			  (g/height wg))))

(define (top window)
  (let ((mg (monitor-geometry window))
	(wg (window-geometry window)))
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (g/x wg)
			  0
			  (g/width wg)
			  (g/height wg))))

(define left-half
  (case-lambda
   ((window) (left-half window (monitor-geometry window)))
   ((window mg)
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (g/x mg)
			  0
			  (/ (g/width mg) 2)
			  (g/height mg)))))

(define right-half
  (case-lambda
   ((window) (right-half window (monitor-geometry window)))
   ((window mg)
    (mark-not-fullscreen window)
    (set-window-geometry! window
			  (+ (g/x mg) (/ (g/width mg) 2))
			  0
			  (/ (g/width mg) 2)
			  (g/height mg)))))

(define (maximize window) (mark-fullscreen window))

(define (rotate predicate list)
  (let ((tail (find-tail predicate list)))
    (if (and tail (not (null? (cdr tail))))
	(cadr tail)
	(car list))))

(define (other-monitor window)
  (let* ((mg1 (monitor-geometry window))
	 (mg2 (cdr (rotate (lambda (a) (geometry= (cdr a) mg1))
			   (monitor-geometry-alist))))
	 (wg (window-geometry window)))
    (cond ((> (g/width wg) (* 3/4 (g/width mg1)))
	   (set-window-geometry! window mg2)
	   (maximize window))
	  ((< (- (g/x wg) (g/x mg1))
	      (/ (g/width mg1) 2))
	   (left-half window mg2))
	  (else (right-half window mg2)))))