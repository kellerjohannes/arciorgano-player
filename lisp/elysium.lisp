(require 'incudine)
(in-package :scratch)

(defparameter *osc-out* (osc:open :port 5900 :direction :output))

(defun key-on (index &optional duration-in-sec)
  (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
  (when duration-in-sec
    (at (+ (now) #[duration-in-sec sec]) #'key-off index)))

(defun key-off (index)
  (osc:message *osc-out* "/incudine-bridge" "ii" index 0))

(defun panic ()
  (loop for i from 0 to 146 do
	(key-off i)))

(defun cluster-on (lowest highest)
  (loop for i from lowest to highest do
	(key-on i)))

(defun cluster-off (lowest highest)
  (loop for i from lowest to highest do
	(key-off i)))

(defun swipe (&key (start 1) (end 146) (delta 1/4) (duration 1/4))
  (cond ((>= start end) nil)
	(t (key-on start duration)
	   (at (+ (now) #[delta sec])
	       #'swipe
	       :start (1+ start)
	       :end end
	       :delta delta
	       :duration duration))))
