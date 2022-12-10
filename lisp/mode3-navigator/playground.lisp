(in-package :mode3-navigator)

(connect-to-pd)


(defparameter *sym* '((1 . 0) (0 . 1) (2 . 2) (-1 . 0) (0 . -1) (-2 . -2)))
(defparameter *pyth* '((0 . 1) (0 . 2) (0 . -1) (0 . -2)))
(defparameter *u-maj7* '((-1 . 0) (0 . -1) (-2 . -2)))
(defparameter *maj7* '((1 . 0) (0 . 1) (2 . 2)))
(defparameter *maj* '((1 . 0) (0 . 1)))
(defparameter *min* '((-1 . 1) (0 . 1)))
(defparameter *5* '((0 . 1)))
(defparameter *3* '((1 . 0)))

(defparameter *shape-selection* (list *maj7*))
(defparameter *shape-selection* (list *maj7* *u-maj7* *maj* *min* *3* *u-maj7*))

(defparameter *default-attack-spread* 15)
(defparameter *default-release-spread* 15)
(defparameter *default-attack-shape* :random)
(defparameter *default-release-shape* :random)


(defun pick-next-origin-3 (origin shape &optional (target-shape shape))
  (let ((result nil))
    (dolist (shape-vector shape)
      (dolist (target-shape-vector (cons '(0 . 0) target-shape))
        (let ((test-vec (vec-sub shape-vector target-shape-vector)))
          (unless (missing-notes-p (move origin test-vec) target-shape)
            (push test-vec result)))))
    (setf result (remove '(0 . 0) (remove-duplicates result :test #'equal) :test #'equal))
    (let ((pick (rand-nth result)))
      (move-origin *painter* pick)
      (move origin pick))))

(defparameter *play* t)

(defun stop-modulation ()
  (setf *play* nil))

(defun start-modulation ()
  (setf *play* t)
  (reset *painter*)
  (play-modulation-2 'c (rand-nth *shape-selection*)))

(defun play-modulation (origin shape)
  (when *play*
    (play-shape origin shape 3.2 .3 :random .3 :random)
    (at (+ (now) #[3 s]) #'play-modulation (pick-next-origin-3 origin shape) shape)))

(defparameter *drawing* nil)

(defun play-modulation-2 (origin shape)
  (when *play*
    (play-shape origin shape .09 0 :random 0 :random)
    (let ((new-shape (rand-nth *shape-selection*)))
      (at (+ (now) #[.05 s])
          #'play-modulation-2
          (pick-next-origin-3 origin shape new-shape)
          new-shape))))
