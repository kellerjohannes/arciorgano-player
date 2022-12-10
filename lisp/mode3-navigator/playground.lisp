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
(defparameter *shape-selection* (list *maj7* *u-maj7* *maj* *min*))

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
  (reset *keyboard*)
  (play-modulation-2 'c (rand-nth *shape-selection*)))

(defun play-modulation (origin shape)
  (when *play*
    (play-shape origin shape 3.2 .3 :random .3 :random)
    (at (+ (now) #[3 s]) #'play-modulation (pick-next-origin-3 origin shape) shape)))

(defparameter *drawing* nil)

(defclass timing-preset ()
  ((duration :initform 25
             :initarg :duration
             :accessor duration)
   (on-spread :initform 8
              :initarg :on-spread
              :accessor on-spread)
   (off-spread :initform 8
               :initarg :off-spread
               :accessor off-spread)
   (delta :initform 20
          :initarg :delta
          :accessor delta)))

(defparameter *medium* (make-instance 'timing-preset :duration 35 :delta 30
                                                     :on-spread 8 :off-spread 8))
(defparameter *slow* (make-instance 'timing-preset :duration 45 :delta 45
                                                   :on-spread 12 :off-spread 12))
(defparameter *fast* (make-instance 'timing-preset :duration 12 :delta 10
                                                   :on-spread 3 :off-spread 3))
(defparameter *superfast* (make-instance 'timing-preset :duration 6 :delta 5
                                                        :on-spread 2 :off-spread 1))
(defparameter *insane* (make-instance 'timing-preset :duration 0.2 :delta 0.2
                                                     :on-spread 0 :off-spread 0))

(defparameter *current-preset* *superfast*)

(defparameter *preset-selection* (list *superfast* *fast* *slow* *medium*))

(defmethod check-times ((preset timing-preset))
  (with-accessors ((out duration)
                   (in delta)
                   (dur-in on-spread)
                   (dur-out off-spread))
      preset
    (format t "~&Complete crossfade: ~as~&Overlay sustain: ~as~&Single sustain: ~as"
            (+ (- out in) dur-out)
            (- out in dur-in)
            (- (* 2 in) out dur-out))))

(defun play-modulation-2 (origin shape)
  (when (= 0 (rand-nth '(0 1 2 3 4)))
    (setf *current-preset* (rand-nth *preset-selection*))
    (format t "~&Timing preset changed to ~a." *current-preset*)
    (check-times *current-preset*))
  (when *play*
    (play-shape origin shape
                (duration *current-preset*)
                (on-spread *current-preset*)
                :random
                (off-spread *current-preset*)
                :random)
    (let ((new-shape (rand-nth *shape-selection*)))
      (at (+ (now) #[(delta *current-preset*) s])
          #'play-modulation-2
          (pick-next-origin-3 origin shape new-shape)
          new-shape))))
