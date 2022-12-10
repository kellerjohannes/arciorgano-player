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
(defparameter *shape-selection* (list *maj7* *u-maj7* *maj* *min* *3* *u-maj7*))

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

;;(crossfade `((c . ,*maj7*) (f♯ . ,*maj7*)))
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

(defun list-all-candidates (origin shape-list &optional (target-shape-list shape-list))
  (mapcar (lambda (local-origin)
            (mapcar (lambda (move)
                      ;;(format t "~&Testing: ~a" (move local-origin (- (car move)) (- (cdr move))))
                      (get-shape-names (move local-origin (- (car move)) (- (cdr move)))
                                       target-shape-list))
                    target-shape-list))
          (candidates origin (cons '(0 . 0) shape-list) target-shape-list)))

(defparameter *result* nil)

(defun flatten-candidates (candidates)
  (cond ((null candidates) *result*)
        ((atom (first (first candidates)))
         (push (first candidates) *result*)
         (flatten-candidates (rest candidates)))
        (t (flatten-candidates (first candidates))
           (flatten-candidates (rest candidates)))))

(defun remove-bad-candidates (candidates)
  (let ((*result* nil))
    (flatten-candidates candidates)
    (remove-if (lambda (chord-list)
                 (member nil chord-list))
               *result*)))

(defun pick-next-origin-2 (origin shape-list &optional (safety-counter 0))
  "Omnidirectional."
  (when (< safety-counter 50)
    (move-origin *painter* (random 3) (random 3))
    (let ((result (rand-nth (remove origin
                                    (mapcar #'first
                                            (remove-bad-candidates
                                             (list-all-candidates origin
                                                                  shape-list)))))))
      (format t "~&Picked new origin: ~a" result)
      (unless result (stop-modulation))
      result)))

(defun pick-next-origin-3 (origin shape &optional (target-shape shape))
  (let ((result nil))
    (dolist (shape-vector shape)
      (dolist (target-shape-vector (cons '(0 . 0) target-shape))
        (let ((test-x (- (car shape-vector) (car target-shape-vector)))
              (test-y (- (cdr shape-vector) (cdr target-shape-vector))))
          (unless (missing-notes-p (move origin test-x test-y) target-shape)
            (push (cons test-x test-y) result)))))
    (setf result (remove '(0 . 0) (remove-duplicates result :test #'equal) :test #'equal))
    (let ((pick (rand-nth result)))
      (move-origin *painter* (car pick) (cdr pick))
      (move origin (car pick) (cdr pick)))))

(defun test-chain (origin shape)
  (let ((new-origin (pick-next-origin-2 origin shape)))
    (format t "~&new origin: ~a" new-origin)
    (read)
    (test-chain new-origin shape)))

(defparameter *play* t)

(defun stop-modulation ()
  (setf *play* nil))

(defun start-modulation ()
  (setf *play* t)
  (reset *painter*)
  (play-modulation-2 'c (rand-nth *shape-selection*)))

(defun play-modulation (origin shape)
  (when *play*
    (play-shape origin shape 3.2 .3 :random .3 :random)
    (at (+ (now) #[3 s]) #'play-modulation (pick-next-origin-3 origin shape) shape)))

(defparameter *drawing* nil)

(defun play-modulation-2 (origin shape)
  (when *play*
    (play-shape origin shape .09 0 :random 0 :random)
    (let ((new-shape (rand-nth *shape-selection*)))
      (at (+ (now) #[.05 s])
          #'play-modulation-2
          (pick-next-origin-3 origin shape new-shape)
          new-shape))))
