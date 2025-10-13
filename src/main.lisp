(uiop:define-package ishikawa
  (:use #:cl
        #:algebra
        #:blobs))

(in-package #:ishikawa)

(show-matrix '((1 2 3)(4 5 6)))

(defun example ()
  (let ((centers '((0.0 0.0)
                   (3.0 3.0))))

    (multiple-value-bind (X Y)
        (make-blobs 200 2 centers 0.8)
      ;; Converts (0,1,2,...) labels to binary (0 et 1)
      (setf Y (mapcar (lambda (y) (if (= y 0) 0 1)) Y))
      
      (defparameter model (make-instance 'perceptron:logistic-regression :eta 0.1 :epochs 200))
      
      (gnuplot:save-blobs-for-gnuplot X Y "./datas/blobs.dat")
      
      (perceptron:train model X Y)
      
      (gnuplot:save-loss-history model "./datas/loss-history.dat")
      
      ;; (format t "Weights: ~a~%" (perceptron:weights model))
      (Format t "Biais: ~a~%" (perceptron:bias model))
      (format t "Loss: ~f~%" (perceptron:log-loss model X Y))
      (format t "Accuracy: ~f~%" (perceptron:score model X Y))
      ;; (format t "Loss history : ~a~%" (perceptron:loss-history model))
      )))
