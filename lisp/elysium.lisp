(require 'incudine)
(in-package :scratch)

(defparameter *osc-out* (osc:open :port 5900 :direction :output))



;; session 1, erste Probe, Selbstspieler-Skizzen

(defparameter *scale-names* '(c c. cis des des. d d. dis es es. e e. eis f f. fis ges ges. g g. gis as as. a a. ais bes bes. b b. bis))

(defparameter *dict-name-pitch* (loop for name in *scale-names*
				      for i from 1
				      collect (cons name i)))

(defparameter *dict-pitch-key* '((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 6) (6 . 7) (7 . 8) (8 . 9) (9 . 11) (10 . 12) (11 . 13) (12 . 14) (13 . 15) (14 . 16) (15 . 17) (16 . 18) (17 . 20) (18 . 21) (19 . 22) (20 . 23) (21 . 24) (22 . 26) (23 . 27) (24 . 28) (25 . 29) (26 . 30) (27 . 31) (28 . 33) (29 . 34) (30 . 35) (31 . 0)))

(defparameter *dict-interval-pitch* '((unisono . 0) (diesis . 1) (diesis-maggiore . 2) (semitono-minore . 2) (semitono-maggiore . 3) (tono-minore . 4) (tono . 5) (tono-maggiore . 6) (terza-minima . 7) (terza-minore . 8) (terza-piu-di-minore . 9) (terza-maggiore . 10) (terza-piu-di-maggiore . 11) (quarta-minima . 12) (quarta . 13) (piu-di-quarta . 14) (tritono . 15) (quinta-imperfetta . 16) (quinta-piu-di-imperfetta . 17) (quinta . 18) (piu-di-quinta . 19) (settima-naturale . 26) (sesta-minore . 23) (sesta-maggiore . 25) (ottava . 31)))

(defun unify-pitch (pitch)
  (cond ((< pitch 1) (unify-pitch (+ pitch 31)))
	((> pitch 31) (unify-pitch (- pitch 31)))
	(t pitch)))

(defun name->pitch (name)
  (cdr (assoc name *dict-name-pitch*)))

(defun pitch->name (pitch)
  (car (find (unify-pitch pitch) *dict-name-pitch* :key #'cdr)))

(defun name->key (name)
  (cdr (assoc (name->pitch name) *dict-pitch-key*)))

(defun pitch->key (pitch)
  (multiple-value-bind (octave pitch-class) (floor pitch 31)
    (+ (* 36 octave) (cdr (assoc pitch-class *dict-pitch-key*)))))

(defun interval->pitch (interval)
  (cdr (assoc interval *dict-interval-pitch*)))

(defun pitch->interval (pitch)
  (car (find (unify-pitch pitch) *dict-interval-pitch* :key #'cdr)))

(defun apply-interval (name-origin interval &key (direction :ascendente))
  "Returns interval name."
  (pitch->name (funcall (if (eq direction :ascendente) #'+ #'-)
			(name->pitch name-origin)
			(interval->pitch interval))))


(defun apply-absolute-interval (name-origin octave interval &key (direction :ascendente))
  "Returns 31 pitch."
  (funcall (if (eq direction :ascendente) #'+ #'-)
	   (+ (* (1- octave) 31) (name->pitch name-origin))
	   (interval->pitch interval)))

(defun apply-interval-to-pitch (pitch interval &key (direction :ascendente))
  "Returns 31 pitch."
  (funcall (if (eq direction :ascendente) #'+ #'-)
	   pitch
	   (interval->pitch interval)))

(defun key-on (index &optional duration-in-sec)
  (when (< 0 index 147)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
    (format t "~a:on " index)
    (when duration-in-sec
      (at (+ (now) #[duration-in-sec sec]) #'key-off index))))

(defun key-off (index)
  (when (< 0 index 147)
    (format t "~a:off " index)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 0)))

(defun panic ()
  (setf *perforation-panic* t)
  (format t "*perforation-panic* set to T. ")
  (loop for i from 0 to 146 do
    (key-off i))
  (at (+ (now) #[5 sec]) #'perforation-on))

(defun cluster-on (lowest highest &optional duration)
  (loop for i from lowest to highest do
    (key-on i))
  (when duration
    (at (+ (now) #[duration sec]) #'cluster-off lowest highest)))

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

(defun waffencluster (&optional duration)
  (cluster-on 1 21 duration))

(defparameter *perforation-panic* nil)

(defun perforation-on ()
  (setf *perforation-panic* nil)
  (format t "*perforation-panic* set to nil. "))

(defun perforation (index &key (on-dur 1/2) (off-dur 1/2) (duration nil) (end-time nil))
  (when (and (null *perforation-panic*) (or (null end-time) (< (now) end-time))) 
    (when (and duration (null end-time))
      (setf end-time (+ (now) #[duration sec])))
    (key-on index on-dur)
    (at (+ (now) #[(+ on-dur off-dur) sec])
	#'perforation
	index
	:on-dur on-dur
	:off-dur off-dur
	:duration duration
	:end-time end-time)))

(defun random-perforation (number-of-keys &key (on-delta 3) (fraction-range 20))
  (when (> number-of-keys 0)
    (perforation (1+ (random 147))
		 :on-dur (/ 0.5 (1+ (random fraction-range)))
		 :off-dur (/ 0.5 (1+ (random fraction-range))))
    (format t "random-perforation started. ~a to go." number-of-keys)
    (at (+ (now) #[on-delta sec])
	#'random-perforation
	(1- number-of-keys)
	:on-delta on-delta
	:fraction-range fraction-range)))

(defun play-pitch (pitch duration)
  (key-on (pitch->key pitch) duration))

(defun play-note (name &key (duration 4) (octave '(1 . 5)))
  (let ((key (name->key name))
	(octave-range (if (consp octave)
			  octave
			  (cons octave octave))))
    (loop for i from (car octave-range) to (cdr octave-range)
	  do (key-on (+ key (* (1- i) 36)) duration))))

(defun play-list (lst &key (dur-on 1) (dur-off 1/4))
  (cond ((null lst) nil)
	(t (play-note (car lst) :duration dur-on)
	   (at (+ (now) #[(+ dur-on dur-off) sec])
	       #'play-list
	       (cdr lst)
	       :dur-on dur-on
	       :dur-off dur-off))))

(defun play-block (lst &key (duration 4))
  (loop for name in lst
	do (play-note name :duration duration)))

(defun chord->name-list (root-name interval-list)
  (cons root-name (mapcar (lambda (interval)
			    (apply-interval root-name interval))
			  interval-list)))

(defun play-chord (root-name interval-list &key (duration 4))
  (play-block (chord->name-list root-name interval-list) :duration duration))

(defparameter *chords* '((major . (terza-maggiore quinta))
			 (minor . (terza-minore quinta))
			 (neutral . (terza-piu-di-minore quinta))
			 (major-nat-7 . (terza-maggiore quinta settima-naturale))
			 (diminished . (terza-minore quinta-imperfetta sesta-maggiore))
			 (spectral . (tono terza-maggiore tritono quinta settima-naturale))))

(defun lookup-chord (name)
  (cdr (assoc name *chords*)))


(defun play-chord-name (root-name chord-name &key (duration 4))
  (play-chord root-name (lookup-chord chord-name) :duration duration))

(defun play-chord-sequence (root-names chord-names &key (dur-on 2) (dur-off 0))
  (unless (consp chord-names)
    (setf chord-names (loop for name in root-names collect chord-names)))
  (cond ((null root-names) nil)
	(t (play-chord-name (car root-names) (car chord-names) :duration dur-on)
	   (at (+ (now) #[(+ dur-on dur-off) sec])
	       #'play-chord-sequence
	       (cdr root-names) (cdr chord-names)
	       :dur-on dur-on
	       :dur-off dur-off))))


(defun pick-random-member (lst)
  (nth (random (length lst)) lst))

(defun play-random-chords (number-of-chords &key (duration 4))
  (unless (zerop number-of-chords)
    (play-chord-name (pick-random-member *scale-names*)
		     (car (pick-random-member *chords*))
		     :duration (* 1.4 duration))
    (at (+ (now) #[duration sec])
	#'play-random-chords
	(1- number-of-chords)
	:duration duration)))



;; Session 2, Versuch einer Kontrapunkt-Maschine
;;
;; Baut teilweise auf Code von Session 1 auf



(defparameter *tetrachord* '(tono tono semitono-maggiore))
(defparameter *tetrachord-dur-on* 1)
(defparameter *tetrachord-articulation* 1.1)
(defparameter *tetrachord-running* t)
(defparameter *fundament-name* 'g)
(defparameter *tetrachord-octave* 2)
(defparameter *number-of-voices* 3)


(defun tet-toggle (onp)
  (if onp
      (setf *tetrachord-running* t)
      (setf *tetrachord-running* nil)))

(defun tet-fundament (name)
  (setf *fundament-name* name))

(defun tet-overlap (factor)
  (setf *tetrachord-articulation* factor))

(defun tet-speed (duration)
  (setf *tetrachord-dur-on* duration))

(defun tet-octave (octave)
  (setf *tetrachord-octave* octave))

(defparameter *score* '((81) (89) (99)))

(defun perform-pitch (pitch duration voice)
  (play-pitch pitch duration)
  (push pitch (nth voice *score*)))

(defun make-model-rotator (interval-list)
  (let ((remainder interval-list))
    #'(lambda ()
      (when (null remainder) (setf remainder interval-list))
	(let ((result (first remainder)))
	  (setf remainder (rest remainder))
	  result))))

(defun make-harmony-rotator (harmony-list)
  (let ((remainder harmony-list))
    #'(lambda ()
      (when (null remainder) (setf remainder harmony-list))
	(let ((result (first remainder)))
	  (setf remainder (rest remainder))
	  result))))

(defparameter *model-3-5* (make-model-rotator '(quinta terza-maggiore)))
(defparameter *model-5-8* (make-model-rotator '(quinta ottava)))

(defparameter *harmony-standard* (make-harmony-rotator '((terza-minore quinta)
							 (terza-maggiore sesta-minore)
							 (terza-maggiore sesta-maggiore)
							 (terza-maggiore quinta))))

(defun compose-gymel (gymel-interval)
  (apply-interval-to-pitch (first (first *score*)) gymel-interval :direction :ascendente))

(defun compose-model (model-rotator)
  (compose-gymel (funcall model-rotator)))

(defun find-voiceleading (origin consonance-options last-note tessitura)
  (let ((options (mapcar (lambda (interval)
			   (let ((distance (- last-note (apply-interval-to-pitch origin interval))))
			     (cons interval distance)))
			 consonance-options)))
    (setf options (sort options #'< :key (lambda (candidate) (abs (cdr candidate)))))
    (+ last-note (cdr (first options)))))

(defparameter *score* '((81) (89) (99)))

(defun play-tetrachord (&optional (current-pitch *fundament-name*) rest-tetrachord)
  (when (symbolp current-pitch)
    (setf current-pitch (+ (* *tetrachord-octave* 31) (name->pitch current-pitch))))
  (when *tetrachord-running*
    (let ((sounding-duration (* *tetrachord-articulation* *tetrachord-dur-on*)))
      (perform-pitch current-pitch sounding-duration 0)
      (perform-pitch (find-voiceleading current-pitch
					(funcall *harmony-standard*)
					(first (nth 1 *score*))
					nil)
		     sounding-duration 1)
      ;(perform-pitch (compose-gymel 'quinta) sounding-duration)
      )
    (at (+ (now) #[*tetrachord-dur-on* sec])
	#'play-tetrachord
	(if rest-tetrachord
	    (apply-interval-to-pitch current-pitch (first rest-tetrachord) :direction :discendente)
	    *fundament-name*)
	(if rest-tetrachord
	    (rest rest-tetrachord)
	    *tetrachord*))))

(defun play-pitch-scale (first-pitch last-pitch duration)
  (when (<= first-pitch last-pitch)
    (play-pitch first-pitch duration)
    (at (+ (now) #[duration sec])
	#'play-pitch-scale
	(1+ first-pitch)
	last-pitch
	duration)))


;; Session 3, couterpoint machine, standalone code (using duplicates from above)



(require 'incudine)
(in-package :scratch)

(defparameter *osc-out* (osc:open :port 5900 :direction :output))



(defparameter *scale-names* '(c c. cis des des. d d. dis es es. e e. eis f f. fis ges ges. g g. gis as as. a a. ais bes bes. b b. bis))

(defparameter *dict-name-pitch* (loop for name in *scale-names*
				      for i from 1
				      collect (cons name i)))

(defparameter *dict-pitch-key* '((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 6) (6 . 7) (7 . 8) (8 . 9) (9 . 11) (10 . 12) (11 . 13) (12 . 14) (13 . 15) (14 . 16) (15 . 17) (16 . 18) (17 . 20) (18 . 21) (19 . 22) (20 . 23) (21 . 24) (22 . 26) (23 . 27) (24 . 28) (25 . 29) (26 . 30) (27 . 31) (28 . 33) (29 . 34) (30 . 35) (31 . 0)))

(defparameter *dict-interval-pitch* '((unisono . 0) (diesis . 1) (diesis-minore . 1) (diesis-maggiore . 2) (semitono-minore . 2) (semitono-maggiore . 3) (tono-minore . 4) (tono . 5) (tono-maggiore . 6) (terza-minima . 7) (terza-minore . 8) (terza-piu-di-minore . 9) (terza-maggiore . 10) (terza-piu-di-maggiore . 11) (quarta-minima . 12) (quarta . 13) (piu-di-quarta . 14) (tritono . 15) (quinta-imperfetta . 16) (quinta-piu-di-imperfetta . 17) (quinta . 18) (piu-di-quinta . 19) (settima-naturale . 26) (sesta-minore . 21) (sesta-maggiore . 23) (ottava . 31)))

(defun unify-pitch (pitch)
  (cond ((< pitch 1) (unify-pitch (+ pitch 31)))
	((> pitch 31) (unify-pitch (- pitch 31)))
	(t pitch)))

(defun name->pitch (name)
  (cdr (assoc name *dict-name-pitch*)))

(defun pitch->name (pitch)
  (car (find (unify-pitch pitch) *dict-name-pitch* :key #'cdr)))

(defun name->key (name)
  (cdr (assoc (name->pitch name) *dict-pitch-key*)))

(defun pitch->key (pitch)
  (multiple-value-bind (octave pitch-class) (floor pitch 31)
    (+ (* 36 octave) (cdr (assoc pitch-class *dict-pitch-key*)))))

(defun interval->pitch (interval)
  (cdr (assoc interval *dict-interval-pitch*)))

(defun pitch->interval (pitch)
  (car (find (unify-pitch pitch) *dict-interval-pitch* :key #'cdr)))


(defun parse-tetrachord (origin-name octave tetrachord)
  (let ((start-pitch (+ (* 31 octave) (name->pitch origin-name))))
    (labels ((rec (val lst)
	       (unless (null lst)
		 (let ((new-val (- val (interval->pitch (car lst)))))
		   (cons new-val (rec new-val (rest lst)))))))
      (cons start-pitch (rec start-pitch tetrachord)))))

(defun key-off (index)
  (when (< 0 index 147)
    ;(format t "~a:off " index)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 0)))

(defun key-on (index &optional duration-in-sec)
  (when (< 0 index 147)
    (osc:message *osc-out* "/incudine-bridge" "ii" index 1)
    ;(format t "~a:on " index)
    (when duration-in-sec
      (at (+ (now) #[duration-in-sec sec]) #'key-off index))))

(defun panic ()
  (loop for i from 0 to 146 do
    (key-off i)))


(defun play-pitch (pitch duration)
  (key-on (pitch->key pitch) duration))



(defparameter *score* '(() () ()))

(defparameter *multiplexer* nil)

(defun multi (val)
  (if (zerop val)
      (setf *multiplexer* nil)
      (setf *multiplexer* val)))

(defun play-latest-keyframe (score duration)
  (mapc (lambda (voice)
	  (play-pitch (first voice) duration)
	  (when *multiplexer*
	    (let ((bottom (- 0 (floor *multiplexer* 2))))
	      (loop for i from bottom to (+ bottom *multiplexer*) do
		(play-pitch (+ (first voice) (* i 31)) duration)))))
	score))

(defun make-harmony-server (interval-list)
  (let ((current-list interval-list))
    #'(lambda (&optional selection)
	(cond ((null current-list) (setf current-list interval-list))
	      (selection (setf current-list (remove selection current-list)))
	      (t current-list)))))

(defun find-voiceleading (origin last-note consonance-options)
  (let ((choice (first (sort (mapcar (lambda (interval)
			      (cons interval (+ origin (interval->pitch interval))))
			    (funcall consonance-options))
		    #'< :key (lambda (pitch-candidate)
			       (abs (- last-note (cdr pitch-candidate))))))))
    (funcall consonance-options (car choice))
    (cdr choice)))

(defun write-to-score (value-list score)
  (cond ((null score) nil)
	(t (cons (cons (car value-list) (car score))
		 (write-to-score (rest value-list) (rest score))))))

(defun compose-keyframe (tetrachord-position tetrachord origin model-position model score &optional start-harmony)
  (let ((current-pitch (nth tetrachord-position (parse-tetrachord
						 (car origin)
						 (cdr origin)
						 (funcall *tetrachord-generator*)))))
    (when start-harmony
      (setf score (write-to-score (cons current-pitch
					(mapcar (lambda (interval)
						  (+ current-pitch (interval->pitch interval)))
						start-harmony))
				  score)))
    (let ((get-harmony-options (make-harmony-server (nth model-position (funcall *model-generator*)))))
      (write-to-score (cons current-pitch
			    (mapcar (lambda (voice)
				      (find-voiceleading current-pitch
							 (first voice)
							 get-harmony-options))
				    (rest score)))
		      score))))

(defparameter *playing* t)

(defun start () (setf *playing* t))
(defun stop () (setf *playing* nil))

(defun loop-tetrachord (position tetrachord origin model score duration-old)
  (when *playing*
    (let ((duration (funcall *duration-generator*)))
      (cond ((>= position 4) (loop-tetrachord 0 tetrachord origin model score duration))
	    (t (play-latest-keyframe score duration)
	       (at (+ (now) #[duration sec])
		   #'loop-tetrachord
		   (1+ position)
		   tetrachord
		   origin
		   model
		   (compose-keyframe position tetrachord origin position model score)
		   duration))))))

(defun without-last (lst)
    (reverse (cdr (reverse lst))))

(defun permutate (lst)
  (cons (car (last lst)) (without-last lst))
  ;(append (rest lst) (list (first lst)))
  )

(defmacro make-rotator (data)
  `(let ((lst ,data))
     #'(lambda ()
	 (setf lst (permutate lst))
	 (first lst))))

(defparameter *limit-rotator* (make-rotator '(limit-2 limit-3 limit-5 limit-7)))
(defparameter *limit* (funcall *limit-rotator*))

(defun next-limit ()
  (setf *limit* (funcall *limit-rotator*)))

(defparameter *quality-rotator* (make-rotator '(consonant dissonant)))

(defparameter *quality* (funcall *quality-rotator*))

(defun next-quality ()
  (setf *quality* (funcall *quality-rotator*)))

(defparameter *model-generator*
  (let ((counter 0))
    #'(lambda (&optional next)
	(let* ((models '((limit-2
			  (consonant
			   ((ottava)
			    (ottava)
			    (ottava)
			    (ottava)))
			  (dissonant
			   ((tritono ottava))))
			 (limit-3
			  (consonant
			   ((quinta ottava)
			    (quinta ottava)
			    (quinta ottava)
			    (quinta ottava))
			   ((quarta ottava)
			    (quarta ottava)
			    (quarta ottava)
			    (quarta ottava))
			   ((quinta ottava)
			    (quarta ottava)
			    (quinta ottava)
			    (quarta ottava)))
			  (dissonant
			   ((tritono settima-naturale)
			    (tritono settima-naturale)
			    (tritono settima-naturale)
			    (tritono settima-naturale))))
			 (limit-5
			  (consonant
			   ((terza-minore quinta ottava)
			    (terza-maggiore sesta-maggiore ottava)
			    (terza-maggiore sesta-maggiore)
			    (terza-maggiore quinta ottava))
			   ((terza-maggiore quinta ottava)
			    (terza-minore sesta-minore ottava)
			    (terza-maggiore sesta-maggiore ottava)
			    (terza-minore quinta ottava)))
			  (dissonant
			   ((tono-maggiore terza-piu-di-minore quinta-imperfetta)
			    (tono-maggiore terza-piu-di-minore quinta-imperfetta)
			    (tono-maggiore terza-piu-di-minore quinta-imperfetta)
			    (tono-maggiore terza-piu-di-minore quinta-imperfetta))))
			 (limit-7
			  (consonant
			   ((terza-maggiore quinta settima-naturale ottava)
			    (terza-maggiore quinta settima-naturale ottava)
			    (terza-maggiore quinta settima-naturale ottava)
			    (terza-maggiore quinta settima-naturale ottava)))
			  (dissonant
			   ((tono-minore tono-maggiore quarta-minima)
			    (tono-minore tono-maggiore quarta-minima)
			    (tono-minore tono-maggiore quarta-minima)
			    (tono-minore tono-maggiore quarta-minima))))))
	       (pick (cdr (assoc *quality* (cdr (assoc *limit* models)))))
	       (len (length pick)))
	  (when next (incf counter))
	  (when (>= counter len) (setf counter 0))
	  (nth counter pick)))))

(defun next-model ()
  (funcall *model-generator* t))

(defparameter *genere-rotator* (make-rotator '(diatonico cromatico enarmonico)))

(defparameter *quarta-rotator* (make-rotator '(prima seconda terza)))

(defun next-genus ()
  (setf *genere* (funcall *genere-rotator*)))

(defun next-quarta ()
  (setf *quarta* (funcall *quarta-rotator*)))

(defparameter *genere* (next-genus))

(defparameter *quarta* (next-quarta))


(defparameter *tetrachord-generator*
  #'(lambda ()
      (let ((quarte '((diatonico
		       (prima . (tono semitono-maggiore tono))
		       (seconda . (tono tono semitono-maggiore))
		       (terza . (semitono-maggiore tono tono)))
		      (cromatico
		       (prima . (semitono-minore terza-minore semitono-maggiore))
		       (seconda . (semitono-maggiore semitono-minore terza-minore))
		       (terza . (terza-minore semitono-minore semitono-maggiore)))
		      (enarmonico
		       (prima . (diesis-maggiore terza-maggiore diesis-minore))
		       (seconda . (diesis-minore terza-maggiore diesis-maggiore))
		       (terza . (terza-maggiore diesis-maggiore diesis-minore))))))
	(cdr (assoc *quarta* (cdr (assoc *genere* quarte)))))))

(defparameter *duration-generator*
  (let ((counter 1)
	(internal-factor 1)
	(random-on 0))
    #'(lambda (&key (reset nil) (factor nil) (rand nil))
	(when reset (setf counter reset))
	(when factor (setf internal-factor factor))
	(cond ((null rand)
	       (setf counter (* counter internal-factor))
	       counter)
	      (t (+ 1/10 (random 2.0)))))))

(defun reset-speed (&optional (val 2))
  (funcall *duration-generator* :reset val))

(defun speed-factor (val)
  (funcall *duration-generator* :factor val))

(defun speed-random (toggle)
  (funcall *duration-generator* :rand toggle))

(defun play-loop ()
  (loop-tetrachord 0
		   *tetrachord-generator*
		   '(g . 1)
		   *model-generator*
		   '((50) (58) (68))
		   1))






(osc:close *oscin*)

(defparameter *oscin* (osc:open :port 5800 :host "localhost" :protocol :udp :direction :input))

(recv-start *oscin*)

(make-osc-responder *oscin* "incudine" "i" 
                             (lambda (genus)
			       (msg warn "~a" genus)))
