(asdf:defsystem arcicontroller
  :description "Tools to interact with the Arciorgano."
  :version "0.1"
  :author "Johannes Keller <johannkell@gmail.com>"
  :licence "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "dictionary-class")
               (:file "pitch-name-class")
               (:file "keyboard-class")
               (:file "sound-generator-class")
               (:file "tuning-class")
               (:file "playground"))
  :depends-on (:incudine)
  :build-pathname "word-counter/bin"
  :entry-point "word-counter::main")
