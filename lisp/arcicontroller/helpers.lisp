(in-package :arcicontroller)

(defun simplify-interval (interval &optional (identity-interval 2/1))
  (cond ((< interval 1)
         (simplify-interval (* interval identity-interval) identity-interval))
        ((>= interval identity-interval)
         (simplify-interval (/ interval identity-interval) identity-interval))
        (t interval)))

(defun temper-interval (interval fraction-of-comma &optional (comma 81/80))
  (* interval (expt comma fraction-of-comma)))
