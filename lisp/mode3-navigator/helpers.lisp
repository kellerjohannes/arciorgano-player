(in-package :mode3-navigator)

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun rand-nth (sequence)
  (if sequence
      (nth (random (length sequence)) sequence)
      nil))

(defun make-vec (x y)
  (cons x y))

(defun vec-add (vec1 vec2)
  (cons (+ (car vec1) (car vec2))
        (+ (cdr vec1) (cdr vec2))))

(defun vec-sub (vec1 vec2)
  (cons (- (car vec1) (car vec2))
        (- (cdr vec1) (cdr vec2))))

(defun vec-mul (vec1 factor)
  (cons (* factor (car vec1))
        (* factor (cdr vec1))))

(defun get-x (vec)
  (car vec))

(defun get-y (vec)
  (cdr vec))

(defmacro when-let (bindings &body body)
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `body`, but if any binding's value
  evaluates to `nil` the process stops and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let ((a (progn (print :a) nil))
               (b (progn (print :b) 2)))
      (list a b))
    ; =>
    :A
    NIL

  "
  (alexandria:with-gensyms (block)
    `(block ,block
       (let ,(loop :for (symbol value) :in bindings
                   :collect `(,symbol (or ,value
                                          (return-from ,block nil))))
         ,@body))))