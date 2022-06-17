(require :incudine)

(defpackage mode3-navigator
  (:use :cl :incudine))


(defparameter *osc-out* (osc:open :port 5900 :direction :output))

(rt-start)

(defun key-off (index)
  (when (< 0 index 147)
    (format t "~a:off " index)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 0)))

(defun key-on (index &optional duration-in-sec)
  (when (< 0 index 147)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
    (format t "~a:on " index)
    (when duration-in-sec
      (at (+ (now) #[duration-in-sec sec]) #'key-off index))))



(defparameter *network*
  '((e♭ b♭+ g a♭ nil)
    (g d+ b♮ c e♭)
    (b♮ f♯+ d♯ e g)
    (d♯ a♯+ nil g♯ b♮)
    (a♭ e♭ c d♭ nil)
    (c g e f a♭)
    (e b♮ g♯ a c)
    (g♯ d♯ b♯ c♯ e)
    (b♯ nil e♭+ e♯ g♯)
    (d♭ a♭ f g♭ nil)
    (f c a b♭ d♭)
    (a e c♯ d f)
    (c♯ g♯ e♯ f♯ a)
    (e♯ b♯ a♭+ a♯ c♯)
    (g♭ d♭ b♭ b♮+ nil)
    (b♭ f d d♯+ g♭)
    (d a f♯ nil b♭)
    (f♯ c♯ a♯ nil d)
    (a♯ e♯ d♭+ nil f♯)
    (e♭+ nil g+ a♭+ b♯)
    (g+ nil b♮+ c+ e♭+)
    (b♮+ g♭ d♯+ e+ g+)
    (d♯+ b♭ nil g♯+ b♮+)
    (a♭+ e♭+ c+ d♭+ e♯)
    (c+ g+ e+ f+ a♭+)
    (e+ b♮+ g♯+ a+ c+)
    (g♯+ d♯+ nil c♯+ e+)
    (d♭+ a♭+ f+ g♭+ a♯)
    (f+ c+ a+ b♭+ d♭+)
    (a+ e+ c♯+ d+ f+)
    (c♯+ g♯+ nil f♯+ a+)
    (g♭+ d♭+ b♭+ nil nil)
    (b♭+ f+ d+ e♭ g♭+)
    (d+ a+ f♯+ g b♭+)
    (f♯+ c♯+ a♯+ b♮ d+)
    (a♯+ nil nil d♯ f♯+)))

(defun move-1 (origin direction)
  (let ((neighbours (cdr (assoc origin *network*))))
    (case direction
      (north (first neighbours))
      (east (second neighbours))
      (south (third neighbours))
      (west (fourth neighbours)))))

(defun lookup-direction (x y)
  (let ((d (list x y)))
    (cond ((equal d '(0 0)) nil)
	  ((equal d '(0 1)) 'north)
	  ((equal d '(1 0)) 'east)
	  ((equal d '(0 -1)) 'south)
	  ((equal d '(-1 0)) 'west)
	  (t nil))))

(defun move (origin delta-x delta-y)
  (cond ((null origin) nil)
	((and (zerop delta-x) (zerop delta-y))
	 origin)
	((zerop delta-x)
	 (let ((step (if (> delta-y 0) -1 1)))
	   (move (move-1 origin (lookup-direction 0 (- step))) 0 (+ delta-y step))))
	(t (let ((step (if (> delta-x 0) -1 1)))
	   (move (move-1 origin (lookup-direction (- step) 0)) (+ delta-x step) delta-y)))))

(defun generate-series (origin delta-x delta-y)
  (cond ((null origin) nil)
	(t (format t "~&~a" origin)
	   (generate-series (move origin delta-x delta-y) delta-x delta-y))))

(defparameter *pitchclass-table*
  '((e♭ . 11)
    (g . 22)
    (b♮ . 34)
    (d♯ . 9)
    (a♭ . 26)
    (c . 1)
    (e . 13)
    (g♯ . 24)
    (b♯ . 36)
    (d♭ . 5)
    (f . 16)
    (a . 28)
    (c♯ . 3)
    (e♯ . 15)
    (g♭ . 20)
    (b♭ . 32)
    (d . 7)
    (f♯ . 18)
    (a♯ . 30)
    (e♭+ . 12)
    (g+ . 23)
    (b♮+ . 35)
    (d♯+ . 10)
    (a♭+ . 27)
    (c+ . 2)
    (e+ . 14)
    (g♯+ . 25)
    (d♭+ . 6)
    (f+ . 17)
    (a+ . 29)
    (c♯+ . 4)
    (g♭+ . 21)
    (b♭+ . 33)
    (d+ . 8)
    (f♯+ . 19)
    (a♯+ . 31)))

(defun lookup-pitchclass (notename)
  (cdr (assoc notename *pitchclass-table*)))

(defun play-pitchclass (notename octave-id duration)
  (key-on (+ (lookup-pitchclass notename) (* (1- octave-id) 36)) duration))

(defun play-maj (root octave-id duration)
  (mapc (lambda (notename)
	  (when notename
	    (play-pitchclass notename octave-id duration)))
	(list root
	      (move root 0 1)
	      (move root 1 0))))

(defun play-series (origin delta-x delta-y step duration &optional (play-fun #'play-pitchclass))
  (cond ((null origin) nil)
	(t (funcall play-fun origin 2 duration)
	   (at (+ (now) #[step sec])
	       #'play-series
	       (move origin delta-x delta-y)
	       delta-x
	       delta-y
	       step
	       duration
	       play-fun))))

(defparameter *running* t)

(defun play-list (lst step duration &optional last-pitch)
  (when (and *running* lst)
    (play-pitchclass (if (listp (first lst))
			 last-pitch
			 (first lst))
		     2
		     duration)
    (at (+ (now) #[step sec])
	#'play-list
	(rest lst) step duration
	(move last-pitch (first (second lst)) (second (second lst))))))

(defun loop-list (lst step duration &optional (play-fun #'play-pitchclass))
  (labels ((looper (restlst last-pitch)
	     (when *running*
	       (cond
		 ((null restlst) (looper (rest lst) last-pitch))
		 ((null last-pitch) nil)
		 (t (funcall play-fun last-pitch 2 duration)
		    (at (+ (now) #[step sec])
			#'looper
			(rest restlst)
			(move last-pitch
			      (first (first restlst))
			      (second (first restlst)))))))))
    (looper (rest lst) (first lst))))

(defun loop-chords (root-list chord-list duration)
  (labels ((looper (rest-root-list rest-chord-list last-pitch)
	     (when *running*
	       (cond
		 ((and (null rest-root-list) (null rest-chord-list))
		  (looper (rest root-list) chord-list last-pitch))
		 ((null rest-chord-list)
		  (looper rest-root-list chord-list last-pitch))
		 ((null rest-root-list)
		  (looper (rest root-list) rest-chord-list last-pitch))
		 ((null last-pitch) nil)
		 (t (play-pitchclass last-pitch 2 duration)
		    (mapc (lambda (notename)
			    (when notename
			      (play-pitchclass notename 2 duration)))
			  (mapcar (lambda (movement)
				    (move last-pitch
					  (first movement)
					  (second movement)))
				  (first rest-chord-list)))
		    (at (+ (now) #[duration sec])
			#'looper
			(rest rest-root-list)
			(rest rest-chord-list)
			(move last-pitch
			      (first (first rest-root-list))
			      (second (first rest-root-list)))))))))
    (looper (rest root-list) chord-list (first root-list))))


;; (loop-chords '(f (1 0) (-1 1)) 
;; 	     '(((0 1) (-1 1)) ((1 0) (0 1)))
;; 	     2)
