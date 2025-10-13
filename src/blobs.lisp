(defpackage :blobs
  (:use :cl)
  (:export :make-blobs))

(in-package :blobs)

(defun randn ()
  ;; Fonction Box-Muller
  ;; Retourne un nombre aleatoire selon une loi normale (moyenne 0, ecart type 1)
  (let* ((u1 (random 1.0))
         (u2 (random 1.0)))
    (* (sqrt (* -2 (log u1)))
       (cos (* 2 pi u2)))))

(defun make-blobs (n-samples n-features centers stddev)
  "Génère n-samples points de n-features dimensions, autour de centers (liste de listes),
   avec un stddev global ou par centre. Retourne deux listes : les données et les labels."
  (let* ((k (length centers))
         (samples-per-center (floor n-samples k))
         (data '())
         (labels '()))
    (loop for i from 0 below k
          for center in centers
          for std = (if (listp stddev)
                        (nth i stddev)
                        stddev)
          do (loop repeat samples-per-center
                   do (let ((point '()))
                        (dotimes (j n-features)
                          (push (+ (nth j center)
                                   (* std (randn)))
                                point))
                        (push (nreverse point) data)
                        (push i labels))))
    (values (nreverse data) (nreverse labels))))

