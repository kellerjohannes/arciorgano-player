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

(defun calculate-time-delta (origin target number-of-notes note-id)
  (+ origin (* note-id (/ (- target origin) number-of-notes))))

(defparameter *default-attack-spread* 1)
(defparameter *default-attack-order* :random)
(defparameter *default-release-spread* 1)
(defparameter *default-release-order* :arpeggio-down)

;; :random-regular, :random-irregular, :arpeggio-regular, :arpeggio-acc
(defun play-key-list (key-list duration
                      &key (attack-spread *default-attack-spread*)
                        (attack-order *default-attack-order*)
                        (release-spread *default-release-spread*)
                        (release-order *default-release-order*))
  (let* ((attack-origin-time (now))
         (attack-target-time (+ attack-origin-time (* attack-spread incudine.util:*sample-rate*)))
         (release-origin-time (+ (now) (* duration incudine.util:*sample-rate*)))
         (release-target-time (+ release-origin-time (* release-spread incudine.util:*sample-rate*))))
    (loop for key in (if (eq attack-order :random)
                         (nshuffle key-list)
                         (sort key-list #'<))
          for key-counter-up from 0
          for key-counter-down downfrom (length key-list)
          do (progn
               (at (calculate-time-delta attack-origin-time
                                         attack-target-time
                                         (length key-list)
                                         (if (eq attack-order :arpeggio-down)
                                             key-counter-down
                                             key-counter-up))
                   #'key-on key)
               (at (calculate-time-delta release-origin-time
                                         release-target-time
                                         (length key-list)
                                         (if (eq release-order :arpeggio-down)
                                             key-counter-down
                                             key-counter-up))
                   #'key-off key)))
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


(defun play-shape (origin shape-list &optional duration)
  "Plays a group of pitchclasses based on `origin' (a pitchclass name) and
   a list of directions, each of them a pair.

   Example for `shape-list': '((1 . 0) (0 . 1)) for a major triad.

   When `duration' is nil a flattened list of keys is returned without
   triggering them."
  (let* ((note-list (loop for movement in (cons '(0 . 0) shape-list)
                          collect (move origin (car movement) (cdr movement))))
         (key-list (alexandria:flatten (loop for note in note-list
                                             collect (play-note note)))))

    (if duration
        (play-key-list key-list duration)
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
