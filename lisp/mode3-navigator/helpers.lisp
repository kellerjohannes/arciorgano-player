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
  (if (and (numberp x) (numberp y))
      (cons x y)
      (error "Can't create a vector with this.")))

(defun vecp (vec)
  (and (numberp (car vec)) (numberp (cdr vec))))

(defun vec-op (vec1 vec2 fun)
  (if (and (vecp vec1) (vecp vec2))
      (cons (funcall fun (car vec1) (car vec2))
            (funcall fun (cdr vec1) (cdr vec2)))
      (error "Can't operate on non-vectors.")))

(defun vec-add (vec1 vec2)
  (vec-op vec1 vec2 #'+))

(defun vec-sub (vec1 vec2)
  (vec-op vec1 vec2 #'-))

(defun vec-mul (vec1 factor)
  (if (and (vecp vec1) (numberp factor))
      (cons (* factor (car vec1))
            (* factor (cdr vec1)))
      (error "Can't multiply a non-vector with a non-number.")))

(defun get-x (vec)
  (if (vecp vec)
      (car vec)
      (error "Can't get x-coordinate of a non-vector.")))

(defun get-y (vec)
  (if (vecp vec)
      (cdr vec)
      (error "Can't get y-coordinate of a non-vector.")))

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
