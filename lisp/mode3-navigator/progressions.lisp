(in-package :mode3-navigator)


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
