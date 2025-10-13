(defpackage :blobs
  (:use :cl)
  (:export :make-blobs))

(in-package :blobs)

(defun randn ()
  ;; Box-Muller Function
  ;; Give a random  according to the normal law  (average 0, standard deviation 1)
  (let* ((u1 (random 1.0))
         (u2 (random 1.0)))
    (* (sqrt (* -2 (log u1)))
       (cos (* 2 pi u2)))))

(defun make-blobs (n-samples n-features centers stddev)
  ;;Generates n-sample points of n-dimensional features, around centers (list of lists),
  ;;with a global or per-center stddev. Returns two lists: the data and the labels.  
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

