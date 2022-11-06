(ql:quickload :incudine)

(defpackage mode3-navigator
  (:use :cl :incudine))

(in-package mode3-navigator)

(defparameter *osc-out* (osc:open :port 5900 :direction :output))

(rt-start)

(defun key-off (index)
  "Switches off the magnet with `index' by sending an OSC message to the PureData-Patch."
  (when (and index (< 0 index 147))
    (format t "~a:off " index)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 0)))

(defun key-on (index &optional duration-in-sec)
  "Switches on the magnet with `index' by sending an OSC message to the PureData-Patch."
  (when (and index (< 0 index 147))
    (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
    (format t "~a:on " index)
    (when duration-in-sec
      (at (+ (now) (* duration-in-sec 44100)) #'key-off index))))



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
    (a♯+ nil nil d♯ f♯+))
  "This represents the Mode3 Tonnetz. The first symbol is considered the origin, the CDR are the neighbours: north, east, south, west.")

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
    (a♯+ . 31))
  "Mapping between pitchclass names and key indices, required to play pitchclass information on the organ.")


;; Navigating the Tonnetz

(defun move-1 (origin direction)
  "Returns the pitchclass name of a neighbour of `origin', based on `direction' ('north, 'east, 'south, 'west)."
  (let ((neighbours (cdr (assoc origin *network*))))
    (case direction
      (north (first neighbours))
      (east (second neighbours))
      (south (third neighbours))
      (west (fourth neighbours)))))

(defun lookup-direction (x y)
  "Returns a direction symbol based on relative unit coordinates."
  (let ((d (list x y)))
    (cond ((equal d '(0 0)) nil)
          ((equal d '(0 1)) 'north)
          ((equal d '(1 0)) 'east)
          ((equal d '(0 -1)) 'south)
          ((equal d '(-1 0)) 'west)
          (t nil))))

(defun move (origin delta-x delta-y)
  "Returns the pitchclass name of the neighbour of `origin' (also a pitchclass name) specified by the direction vector (`delta-x' `delta-y')."
  (cond ((null origin) nil)
        ((and (zerop delta-x) (zerop delta-y))
         origin)
        ((zerop delta-x)
         (let ((step (if (> delta-y 0) -1 1)))
           (move (move-1 origin (lookup-direction 0 (- step))) 0 (+ delta-y step))))
        (t (let ((step (if (> delta-x 0) -1 1)))
             (move (move-1 origin (lookup-direction (- step) 0)) (+ delta-x step) delta-y)))))

(defun generate-series (origin delta-x delta-y)
  "Returns a list of pitchclass names by moving recursively from `origin' by an interval described by the vector (`delta-x' `delta-y')."
  (cond ((null origin) nil)
        (t (cons origin (generate-series (move origin delta-x delta-y) delta-x delta-y)))))





;; Playing the arciorgano


(defun lookup-pitchclass (notename)
  "Returns the key index of the pitchclass name `notename'."
  (cdr (assoc notename *pitchclass-table*)))

(defun play-note (notename duration)
  "Plays all available octaves of the pitchclass name `notename'."
  (when notename
    (let ((pitchclass-index (lookup-pitchclass notename)))
      (dotimes (i 5)
        (key-on (+ pitchclass-index (* i 36)) duration)))))

(defun play-shape (origin shape-list duration)
  "Plays a group of pitchclasses based on `origin' (a pitchclass name) and a list of directions, each of them a pair."
  (dolist (movement (cons '(0 . 0) shape-list))
    (play-note (move origin (car movement) (cdr movement)) duration)))

(defparameter *harmonies*
  '((major ((1 . 0) (0 . 1)))
    (minor ((0 . 1) (-1 . 1)))
    (nat-seven ((1 . 0) (0 . 1) (2 . 2)))))

(defun play-harmony (origin harmony duration)
  "Plays a shape named by `harmony' based on `origin' (pichclass name)."
  (play-shape origin (second (assoc harmony *harmonies*)) duration))

(defparameter *running* t)

(defun random-element (lst)
  (nth (random (length lst)) lst))

(defun harmony-walk (origin harmony duration)
  (when *running*
    (play-harmony origin harmony duration)
    (let ((shape (second (assoc harmony *harmonies*))))
      (at (+ (now) (* 44100 duration))
          #'harmony-walk
          (let ((new-origin-delta (random-element shape)))
            (move origin (car new-origin-delta) (cdr new-origin-delta)))
          (first (random-element *harmonies*))
          duration))))

(defun stop-walk ()
  (setf *running* nil))

(defun start-walk ()
  (setf *running* t)
  (harmony-walk 'c 'major 5))



;; Old and obsolete


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
       (at (+ (now) (* step 44100))
           #'play-series
           (move origin delta-x delta-y)
           delta-x
           delta-y
           step
           duration
           play-fun))))


(defun play-list (lst step duration &optional last-pitch)
  (when (and *running* lst)
    (play-pitchclass (if (listp (first lst))
             last-pitch
             (first lst))
             2
             duration)
    (at (+ (now) (* step 44100))
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
            (at (+ (now) (* step 44100))
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
            (at (+ (now) (* duration 44100))
            #'looper
            (rest rest-root-list)
            (rest rest-chord-list)
            (move last-pitch
                  (first (first rest-root-list))
                  (second (first rest-root-list)))))))))
    (looper (rest root-list) chord-list (first root-list))))

(defun play-interval (root interval &optional (octave 2))
  (let ((lower (+ (lookup-pitchclass root) (* 36 (1- octave)))))
    (key-on lower 2)
    (at (+ (now) (* 2 44100))
    #'key-on
    (+ lower (- (lookup-pitchclass (move root
                         (first interval)
                         (second interval)))
            (lookup-pitchclass root)))
    2)))

;; (loop-chords '(f (1 0) (-1 1))
;;       '(((0 1) (-1 1)) ((1 0) (0 1)))
;;       2)

(loop-chords '(d (0 1) (-1 2) (0 0) (0 1))
         '(((0 1)) ((0 1)) ((1 0) (1 -1)) ((0 1) (1 0)))
         2)
