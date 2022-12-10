(in-package :mode3-navigator)


(defparameter *osc-out* nil)

(defun connect-to-pd ()
  (format t "~&Please confirm that a Jack audio server is running [yes]: ")
  (when (eq (read) 'yes)
    (incudine:rt-start)
    (format t "~&Remember to run the Arciorgano Player Pd patch and to activate \"incudine bridge\"."))
  (setf *osc-out* (osc:open :port 5900 :direction :output)))



(defclass keyboard ()
  ((keys :initform (make-array 146 :element-type 'number :initial-element 0)
         :accessor keys)))

(defmethod reset ((keyboard keyboard))
  (loop for key across (keys keyboard)
        do (setf (aref (keys keyboard) key) 0)))

(defmethod request-key ((keyboard keyboard) index &optional duration-in-sec)
  (when (< -1 index 146)
    (incf (aref (keys keyboard) index))
    (key-on index duration-in-sec)
    (when duration-in-sec
      (at (+ (now) #[duration-in-sec s]) #'release-key keyboard index))))

(defmethod release-key ((keyboard keyboard) index)
  (when (< -1 index 146)
    (with-accessors ((keys keys))
        keyboard
      (decf (aref keys index))
      (when (minusp (aref keys index))
        (setf (aref keys index) 0))
      (when (zerop (aref keys index))
        (key-off index)))))

(defun key-off (index)
  "Switches off the magnet with `index' by sending an OSC message to the PureData-Patch."
  (when (and index (< 0 index 147))
    (format t "~a:off " index)
    (finish-output nil)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 0)))

(defun key-on (index &optional duration-in-sec)
  "Switches on the magnet with `index' by sending an OSC message to the PureData-Patch."
  (when (and index (< 0 index 147))
    (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
    (format t "~a:on " index)
    (finish-output nil)
    (when duration-in-sec
      (at (+ (now) (* duration-in-sec 44100)) #'key-off index))))

(defparameter *keyboard* (make-instance 'keyboard))

(defun key-on* (index &optional duration-in-sec)
  (request-key *keyboard* index duration-in-sec))

(defun key-off* (index)
  (release-key *keyboard* index))
