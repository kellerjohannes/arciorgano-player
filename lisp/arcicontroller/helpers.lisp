(in-package :arcicontroller)

(defun collect-alterations (index &optional (plus-glyph "♯") (minus-glyph "♭"))
  "Returns a string with a series of glyphs, depending on the value and sign of `index'. Used to generate (multiple) alterations."
  (unless (zerop index)
    (if (plusp index)
        (concatenate 'string plus-glyph (collect-alterations (1- index)))
        (concatenate 'string minus-glyph (collect-alterations (1+ index))))))

(defun collect-indices (number-of-iterations index-glyph)
  "Return a string with a series of glyphs (provided as string). Used to generate octave indices of pitch names."
  (unless (zerop number-of-iterations)
    (concatenate 'string index-glyph (collect-indices (1- number-of-iterations) index-glyph))))

(defun simplify-interval (interval &optional (identity-interval 2/1))
  (cond ((< interval 1)
         (simplify-interval (* interval identity-interval) identity-interval))
        ((>= interval identity-interval)
         (simplify-interval (/ interval identity-interval) identity-interval))
        (t interval)))

(defun temper-interval (interval fraction-of-comma &optional (comma 81/80))
  (* interval (expt comma fraction-of-comma)))
