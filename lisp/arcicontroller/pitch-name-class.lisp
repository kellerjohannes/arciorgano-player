(in-package :arcicontroller)

(defclass pitch-name ()
  ()
  (:documentation "Subclasses of `pitch-name' are used to reference pitches in various formats."))

(defgeneric get-notename-string (pitch-name convention)
  (:documentation "Returns a string with a human-readable representation of the given pitch name instance, following a certain convention. :default needs to be provided by all methods."))

(defclass modern-pitch-name (pitch-name)
  ((letter :initform #\c
           :initarg :letter
           :accessor letter
           :documentation "Character describing one of seven root note names (white keys): a, b, c, d, e, f, g.")
   (accidental :initform 0
               :initarg :accidental
               :accessor accidental
               :documentation "Index describing the accidental. Negative numbers represent flats, positive numbers sharps, 0 represents a natural note. Example: -2 stands for 'double flat'.")
   (octave :initform 0
           :initarg :octave
           :accessor octave
           :documentation "This number represents the octave where 0 stands for 'middle c', which is the central note of a c-clef or the a fourth above the g-string of a violin."))
  (:documentation "Representation of a note name according to the modern convention: a letter with an accidental and an octave indication."))

(defun create-modern-pitch-name (letter accidental octave)
  "Initializes an instance of `modern-pitch-name'. Letter needs to be provided as char, accidental as a number and octave as a number (0 for the octave starting with middle c)."
  (make-instance 'modern-pitch-name
                 :letter letter
                 :accidental accidental
                 :octave octave))

(defmethod get-notename-string ((name modern-pitch-name) convention)
  "Possible values for `convention': :english (scientific pitch notation), :italian (following the convention where 0 exists), :german (following the Helmholtz standard). :default is the same as :english."
  (let* ((this-letter (letter name))
         (octave-index (case convention
                         (:english (+ (octave name) 4))
                         (:italian (+ (octave name) 3))
                         (:german (cond ((>= (octave name) 0)
                                         (collect-indices (1+ (octave name)) "'"))
                                        ((= (octave name) -1) "")
                                        ((= (octave name) -2)
                                         (setf this-letter (char-upcase this-letter))
                                         "")
                                        (t (setf this-letter (char-upcase this-letter))
                                           (collect-indices (- (+ 2 (octave name))) ","))))
                         (:default (+ (octave name) 4))
                         (otherwise (+ (octave name) 4)))))
    (format nil "~a~a~a"
            this-letter
            (let ((alteration (collect-alterations (accidental name))))
              (if alteration alteration "♮"))
            octave-index)))
