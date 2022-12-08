(in-package :mode3-navigator)


(defparameter *osc-out* nil)

(defun connect-to-pd ()
  (format t "~&Please confirm that a Jack audio server is running [yes]: ")
  (when (eq (read) 'yes)
    (incudine:rt-start)
    (format t "~&Remember to run the Arciorgano Player Pd patch and to activate \"incudine bridge\"."))
  (setf *osc-out* (osc:open :port 5900 :direction :output)))

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
