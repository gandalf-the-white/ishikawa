(defpackage :gnuplot
  (:use :cl)
  (:export #:save-blobs-for-gnuplot
           #:save-loss-history))

(in-package :gnuplot)

(defun save-blobs-for-gnuplot (points labels filepath)
  ;; Saves points and labels to a text file for gnuplot.
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "# x y label~%")
    (loop for point in points
          for label in labels
          do (format stream "~f ~f ~d~%"
                     (first point)
                     (second point)
                     label))))

(defun save-logistic-dataset (points labels filepath)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (format out "# x y label~%")
    (loop for x in X
          for y in Y
          do (format out "~f ~f ~d~%" (first x) (second x) y))))

(defun save-loss-history (model filepath)
  (with-open-file (out filepath :direction :output :if-exists :supersede)
    (format out "# epoch loss~%")
    (loop for l in (perceptron:loss-history model)
          for i from 0
          do (format out "~a ~f~%" i l))))
