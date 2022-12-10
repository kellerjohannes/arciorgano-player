(in-package :mode3-navigator)


(defclass canvas ()
  ((shapes :initform nil
            :accessor shapes)
   (movements :initform nil
              :accessor movements)
   (current-shape :initform nil
                  :accessor current-shape)
   (current-origin :initform '(0 . 0)
                   :accessor current-origin)))

(defmethod reset ((canvas canvas))
  (setf (shapes canvas) nil)
  (setf (current-shape canvas) nil)
  (setf (movements canvas) nil)
  (setf (current-origin canvas) '(0 . 0)))

(defmethod add-vector-to-shape ((canvas canvas) x y)
  (push (cons x y) (current-shape canvas)))

(defmethod push-shape ((canvas canvas))
  (push (cons (current-origin canvas) (current-shape canvas)) (shapes canvas))
  (setf (current-shape canvas) nil))

(defmethod move-origin ((canvas canvas) delta-x delta-y)
  (with-accessors ((current-origin current-origin))
      canvas
    (let ((new-origin (cons (+ (car current-origin) delta-x)
                            (+ (cdr current-origin) delta-y))))
      (push (cons current-origin new-origin) (movements canvas))
      (setf current-origin new-origin))))

(defun calculate-absolute-coordinates (shape)
  (let ((origin (first shape)))
    (cons origin (mapcar (lambda (point)
                           (cons (+ (car origin) (car point))
                                 (+ (cdr origin) (cdr point))))
                         (rest shape)))))

(defun isolate-axis (shape axis)
  (mapcar (if (eq axis :x) #'car #'cdr) shape))

(defun get-extreme (shape axis sign)
  (first (sort (isolate-axis (calculate-absolute-coordinates shape) axis)
               (if (eq sign :+) #'> #'<))))

(defun pick-extreme (shapes axis sign)
  (first (sort (mapcar (lambda (shape)
                         (get-extreme shape axis sign))
                       shapes)
               (if (eq sign :+) #'> #'<))))

(defun convert-to-ratio (width height &optional (x-dim 16.0) (y-dim 9.0))
  (let ((new-height (* y-dim (/ width x-dim)))
        (new-width (* x-dim (/ height y-dim))))
    (if (> new-height height)
        (cons width new-height)
        (cons new-width height))))

(defmethod calculate-dimensions ((canvas canvas))
  ;; 1920 1080
  (with-accessors ((shapes shapes))
      canvas
    (let* ((north (pick-extreme shapes :y :+))
           (east (pick-extreme shapes :x :+))
           (south (pick-extreme shapes :y :-))
           (west (pick-extreme shapes :x :-))
           (width (- east west))
           (height (- north south))
           (real-dims (convert-to-ratio (+ width 2) (+ height 2))))
      (values (scale-x (car real-dims))
              (scale-y (cdr real-dims))
              (scale-y (+ (+ north 0.5) (* 0.5 (- (cdr real-dims) height))))
              (scale-x (+ (+ east 0.5) (* 0.5 (- (car real-dims) width))))
              (scale-y (- (- south 0.5) (* 0.5 (- (cdr real-dims) height))))
              (scale-x (- (- west 0.5) (* 0.5 (- (car real-dims) width))))))))

(defparameter *x-scaling* 100)
(defparameter *y-scaling* 100)
(defparameter *x-translation* 0)
(defparameter *y-translation* 0)

(defun scale-x (x-length)
  (+ *x-translation* (* *x-scaling* x-length)))

(defun scale-y (y-length)
  (+ *y-translation* (* *y-scaling* y-length)))

(defun draw-grid (scene)
  (loop for i from -10 to 10
        do (loop for j from -10 to 10
                 do (svg:draw scene (:circle :cx (scale-x i) :cy (scale-y j) :r 27
                                             :fill "white"
                                             :stroke "gainsboro"
                                             :stroke-width 10)))))

(defun draw-point (scene x y)
  (svg:draw scene (:circle :cx (scale-x x) :cy (scale-y y)
                           :r 10
                           :fill "black")))

(defun draw-chord-line (scene origin-x origin-y vector-x vector-y)
  (svg:draw scene (:line :x1 (scale-x origin-x)
                         :y1 (scale-y origin-y)
                         :x2 (scale-x (+ origin-x vector-x))
                         :y2 (scale-y (+ origin-y vector-y))
                         :stroke "black"
                         :stroke-width 2)))

(defun draw-movement-line (scene origin-x origin-y target-x target-y &optional highlight)
  (svg:draw scene (:line :x1 (scale-x origin-x)
                         :y1 (scale-y origin-y)
                         :x2 (scale-x target-x)
                         :y2 (scale-y target-y)
                         :stroke (if highlight "red" "blue")
                         :stroke-width (if highlight 7 5))))

(defmethod draw ((canvas canvas))
  (multiple-value-bind (width height north east south west)
      (calculate-dimensions canvas)
    (declare (ignore north east))
    (svg:with-svg-to-file (scene 'svg:svg-1.2-toplevel
                                 :width width
                                 :height height
                                 :view-box (format nil "~a ~a ~a ~a"
                                                   west south width height))
        ("mode3-output.svg" :if-does-not-exist :create
                            :if-exists :supersede)
      (svg:draw scene (:rect :x west :y south :width width :height height :fill "white"))
      ;; (draw-grid scene)
      (loop for movement in (reverse (movements canvas))
            for i downfrom (1- (length (movements canvas)))
        do (let ((origin (car movement))
              (vector (cdr movement)))
          (draw-movement-line scene
                              (car origin) (cdr origin)
                              (car vector) (cdr vector)
                              (if (zerop i) t))))
      (dolist (shape (shapes canvas))
        (let ((origin (first shape)))
          (dolist (vector (rest shape))
            (draw-chord-line scene (car origin) (cdr origin) (car vector) (cdr vector)))
          (dolist (vector (rest shape))
            (draw-point scene (+ (car origin) (car vector)) (+ (cdr origin) (cdr vector))))
          (draw-point scene (car origin) (cdr origin)))))))



(defparameter *painter* (make-instance 'canvas))


(defun test-fill ()
  (add-vector-to-shape *painter* 1 0)
  (add-vector-to-shape *painter* 0 1)
  (add-vector-to-shape *painter* 2 2)
  (push-shape *painter*)
  (move-origin *painter* -1 2)
  (add-vector-to-shape *painter* 1 0)
  (add-vector-to-shape *painter* 0 1)
  (add-vector-to-shape *painter* 2 2)
  (push-shape *painter*)
  (move-origin *painter* 3 -4)
  (add-vector-to-shape *painter* 1 0)
  (add-vector-to-shape *painter* 0 1)
  (add-vector-to-shape *painter* 2 2)
  (push-shape *painter*))

(defun add-more ()
  (move-origin *painter* -1 2)
  (add-vector-to-shape *painter* 1 0)
  (add-vector-to-shape *painter* 0 1)
  (add-vector-to-shape *painter* 2 2)
  (push-shape *painter*))

(defun test ()
  (reset *painter*)
  (test-fill)
  (draw *painter*))
