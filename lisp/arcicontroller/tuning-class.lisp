(in-package :arcicontroller)


(defclass tuning ()
  ((name :initform ""
         :initarg :name
         :accessor name
         :documentation "Name of the tuning system.")
   (description :initform ""
                :initarg :description
                :accessor description
                :documentation "Description of the tuning system."))
  (:documentation "Superclass for tuning definitions."))

(defgeneric get-frequency (tuning pitch-reference reference-frequency)
  (:documentation "Returns an absolute frequency based on the tuning system `tuning' and a pitch reference. Different implementations are required to meet the needs of specific types for `pitch-reference'. `reference-frequency' defines the 1/1 pitch."))


(defclass linear-system (tuning)
  ((generator-interval :initform 3/2
                       :initarg :generator-interval
                       :accessor generator-interval
                       :documentation "Interval used to generate all pitches of the linear system. Normally some version of a fifth.")
   (identity-interval :initform 2/1
                      :initarg :identity-interval
                      :accessor identity-interval
                      :documentation "Interval used as identity. Usually 2/1.")
   (left-border :initform -3
                :initarg :left-border
                :accessor left-border
                :documentation "Number of iterations from the origin (1/1) downwards.")
   (right-border :initform 8
                 :initarg :right-border
                 :accessor right-border
                 :documentation "Number of iterations from the origin (1/1) upwards.")
   (pitch-list :initform (make-array 0 :fill-pointer 0 :adjustable t :element-type 'real)
               :accessor pitch-list
               :documentation "Vector of pitches, ordered along the chain of generator intervals.")
   (dictionary-modern-note-name :initform nil
                                :accessor dictionary-modern-note-name
                                :documentation "Contains an instance of `dictionary' that describes the relationship between indices for `pitch-list' and `modern-note-name'.")))

(defmethod initialize-instance :after ((tuning linear-system) &key)
  (do ((index (left-border tuning) (1+ index))
       (current-interval (simplify-interval (expt (generator-interval tuning)
                                                  (left-border tuning)))
                         (simplify-interval (* current-interval (generator-interval tuning)))))
      ((> index (right-border tuning)) nil)
    (vector-push-extend current-interval (pitch-list tuning))))

(defmethod get-pitch-list ((tuning linear-system))
  (pitch-list tuning))

(defmethod get-frequency ((tuning linear-system)
                          (pitch-reference modern-pitch-name)
                          reference-frequency)
  (* reference-frequency ...))
