(in-package :mode3-navigator)

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
  "This represents the Mode3 Tonnetz. The first symbol is considered the origin,
   the CDR are the neighbours: north, east, south, west.")

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
  "Mapping between pitchclass names and key indices, required to play
   pitchclass information on the organ.")


(defun lookup-pitchclass (notename)
  "Returns the key index of the pitchclass name `notename'."
  (cdr (assoc notename *pitchclass-table*)))


;; tonnetz-based playing

(defun calculate-time-shift (origin target number-of-notes note-id)
  (+ origin (* note-id (/ (- target origin) number-of-notes))))

(defun reorder-key-list (key-list shape)
  (case shape
    (:arpeggio-down (sort key-list #'>))
    (:arpeggio-up (sort key-list #'<))
    (:random (nshuffle key-list))
    (otherwise key-list)))

(defun fuzzy-trigger (origin-time target-time key-list trigger-fun shape)
  (format t "~&Real world fading time: ~a~&" (/ (- target-time origin-time)
                      incudine.util:*sample-rate*))
  (let ((ordered-key-list (reorder-key-list key-list shape)))
    (format t "~&Reordered key list: ~a" ordered-key-list)
    (loop for key in ordered-key-list
          for note-counter from 0
          do (at (calculate-time-shift origin-time target-time (length key-list) note-counter)
                 trigger-fun key))))

(defparameter *default-attack-spread* 1)
(defparameter *default-attack-shape* :random)
(defparameter *default-release-spread* 1)
(defparameter *default-release-shape* :random)

(defun play-key-list (key-list duration
                      &key (attack-spread *default-attack-spread*)
                        (attack-shape *default-attack-shape*)
                        (release-spread *default-release-spread*)
                        (release-shape *default-release-shape*))
  (let* ((attack-origin-time (now))
         (attack-target-time (+ attack-origin-time #[attack-spread s]))
         (release-origin-time (+ (now) #[duration s]))
         (release-target-time (+ release-origin-time #[release-spread s])))
    (fuzzy-trigger attack-origin-time attack-target-time key-list #'key-on* attack-shape)
    (fuzzy-trigger release-origin-time release-target-time key-list #'key-off* release-shape)
    key-list))

(defun play-note (notename &optional duration)
  "Plays all available octaves of the pitchclass name `notename'.
   When `duration' is nil a list of keys is returned without triggering them."
  (when notename
    (let ((pitchclass-index (lookup-pitchclass notename)))
      (let ((key-list (loop for i from 0 to 5
                            collect (+ pitchclass-index (* i 36)))))
        (if duration
            (play-key-list key-list duration)
            key-list)))))

(defun get-shape-names (origin shape-list)
  (loop for movement in (cons (make-vec 0 0) shape-list)
        collect (move origin movement)))

(defun missing-notes-p (origin shape-list)
  (member nil (get-shape-names origin shape-list)))

(defun play-shape (origin shape-list &optional duration attack-spread attack-shape release-spread release-shape)
  "Plays a group of pitchclasses based on `origin' (a pitchclass name) and
   a list of directions, each of them a pair.

   Example for `shape-list': '((1 . 0) (0 . 1)) for a major triad.

   When `duration' is nil a flattened list of keys is returned without
   triggering them."
  (format t "~&Arciorgano key tabulature:~&  Origin: ~a~&  Shape: ~a"
          origin
          (get-shape-names origin shape-list))
  (dolist (vector shape-list)
    (add-vector-to-shape *painter* vector))
  (push-shape *painter*)
  (when *drawing* (draw *painter*))
  (let* ((note-list (get-shape-names origin shape-list))
         (key-list (alexandria:flatten (loop for note in note-list
                                             collect (play-note note)))))
    (if duration
        (play-key-list key-list
                       duration
                       :attack-spread attack-spread
                       :attack-shape attack-shape
                       :release-spread release-spread
                       :release-shape release-shape)
        key-list)))



;; Navigating the Tonnetz

(defun move-1 (origin direction)
  "Returns the pitchclass name of a neighbour of `origin', based on `direction' ('north, 'east, 'south, 'west)."
  (let ((neighbours (cdr (assoc origin *network*))))
    (case direction
      (north (first neighbours))
      (east (second neighbours))
      (south (third neighbours))
      (west (fourth neighbours)))))

(defun lookup-direction (vec)
  "Returns a direction symbol based on relative unit coordinates."
  (cond ((equal vec (make-vec 0 0)) nil)
        ((equal vec (make-vec 0 1)) 'north)
        ((equal vec (make-vec 1 0)) 'east)
        ((equal vec (make-vec 0 -1)) 'south)
        ((equal vec (make-vec -1 0)) 'west)
        (t nil)))

(defun move-x-priority (origin delta-vec)
  "Returns the pitchclass name of the neighbour of `origin' (also a pitchclass name) specified by the direction vector (`delta-x' `delta-y')."
  (cond ((null origin) nil)
        ((and (zerop (get-x delta-vec)) (zerop (get-y delta-vec)))
         origin)
        ((zerop (get-x delta-vec))
         (let ((step (if (> (get-y delta-vec) 0) -1 1)))
           (move (move-1 origin (lookup-direction (make-vec 0 (- step))))
                 (make-vec 0 (+ (get-y delta-vec) step)))))
        (t (let ((step (if (> (get-x delta-vec) 0) -1 1)))
             (move (move-1 origin (lookup-direction (make-vec (- step) 0)))
                   (make-vec (+ (get-x delta-vec) step) (get-y delta-vec)))))))

(defun move-y-priority (origin delta-vec)
  "Returns the pitchclass name of the neighbour of `origin' (also a pitchclass name) specified by the direction vector (`delta-x' `delta-y')."
  (cond ((null origin) nil)
        ((and (zerop (get-x delta-vec)) (zerop (get-y delta-vec))) origin)
        ((zerop (get-y delta-vec))
         (let ((step (if (> (get-x delta-vec) 0) -1 1)))
           (move (move-1 origin (lookup-direction (make-vec (- step) 0)))
                 (make-vec (+ (get-x delta-vec) step) 0))))
        (t (let ((step (if (> (get-y delta-vec) 0) -1 1)))
             (move (move-1 origin (lookup-direction (make-vec 0 (- step))))
                   (make-vec (get-x delta-vec) (+ (get-y delta-vec) step)))))))

(defun move (origin delta-vec)
  "X and Y priority."
  (or (move-x-priority origin delta-vec)
      (move-y-priority origin delta-vec)))
