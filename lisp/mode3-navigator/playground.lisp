(in-package :mode3-navigator)

(connect-to-pd)

(defparameter *maj7* '((1 . 0) (0 . 1) (2 . 2)))


(defparameter *default-attack-spread* 15)
(defparameter *default-release-spread* 15)
(defparameter *default-attack-shape* :random)
(defparameter *default-release-shape* :random)

(defun crossfade (chord-list)
  "Each chord is a pair of a notename and a shapelist."
  (cond ((null chord-list) nil)
        (t (play-shape (car (first chord-list))
                       (cdr (first chord-list))
                       10
                       2
                       :random
                       2
                       :random)
           (at (+ (now) #[6 s]) #'crossfade (rest chord-list)))))

(crossfade `((c . ,*maj7*) (f♯ . ,*maj7*)))
;;(crossfade `((c . ,*maj7*)))


;; (play-shape 'c *maj7* 5 1 :random 1 :random)


(defun pick-next-origin-1 (origin shape-list &optional (safety-counter 0))
  "Progressive."
  (when (< safety-counter 50)
    (let ((candidate (rand-nth (rest (get-shape-names origin shape-list)))))
      (if (missing-notes-p candidate shape-list)
          (pick-next-origin origin shape-list (1+ safety-counter))
          candidate))))

(defun candidates (origin shape-list &optional (target-shape-list shape-list))
  (remove-if #'null (mapcar (lambda (candidate)
                              (unless (missing-notes-p candidate target-shape-list)
                                candidate))
                            (rest (get-shape-names origin shape-list)))))

(defun all-candidates (origin shape-list &optional target-shape-list)

  )

(defun pick-next-origin-2 (origin shape-list &optional (safety-counter 0))
  "Omnidirectional."
  ())
