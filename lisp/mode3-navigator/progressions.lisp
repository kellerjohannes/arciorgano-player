(in-package :mode3-navigator)

;; old and obsolete

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
