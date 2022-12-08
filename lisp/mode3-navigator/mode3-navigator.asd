(asdf:defsystem mode3-navigator
  :description "Generative chord progressions in mode3"
  :version "0.1"
  :author "Johannes Keller <johannkell@gmail.com>"
  :licence "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "tonnetz")
               (:file "progressions"))
  :depends-on (:incudine))
