(in-package :arcicontroller)

(defclass dictionary ()
  ((data :initform (make-hash-table)
         :accessor data
         :documentation "Stores the information of the dictionary as a hash table."))
  (:documentation "Provides the data structure and functionality to translate between various pitch naming systems. Subclasses of `tuning' can use `dictionary' in their slots to provide different naming systems for their interfaces."))

(defgeneric add-entry (dictionary notename1 notename2)
  (:documentation "Adds an element to the `data' hash table."))

(defgeneric read-dictionary-list (dictionary data-list)
  (:documentation "Convenience function to read typed definitions for a dictionary."))

(defmethod reset-data ((dict dictionary))
  (setf (data dict) (make-hash-table)))


(defclass dictionary-modern-note-name->index (dictionary)
  ()
  (:documentation "This provides translation methods to convert `modern-note-name' into simple numbers representing an index (in a ratio list for example)."))

(defmethod add-entry ((dict dictionary-modern-note-name->index) (notename1 modern-pitch-name) index)
  (setf (gethash (intern (get-notename-string notename1 :default)) (data dict))
        (list notename1 index)))

(defmethod read-dictionary-list ((dict dictionary-modern-note-name->index) data-list)
  "The format of `data-list' is expected to the the following: a list of lists, of which each is (list letter accidental octave index"
  (dolist (entry data-list)
    (add-entry dict (create-modern-pitch-name (first entry)
                                              (second entry)
                                              (third entry))
               (fourth entry))))
