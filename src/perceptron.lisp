(defpackage :perceptron
  (:use :cl)
  (:export #:logistic-regression
           #:weights
           #:bias
           #:train
           #:log-loss
           #:loss-history
           #:score))

(in-package :perceptron)

(defclass logistic-regression ()
  ((w :initform nil :accessor weights)
   (b :initform 0.0 :accessor bias)
   (eta :initarg :eta :accessor eta)
   (epochs :initarg :epochs :accessor epochs)
   (loss-history :initform nil :accessor loss-history)))

(defun dot-product-list (l1 l2)
  "The dot product between two numeric lists."
  (reduce #'+ (mapcar #'* l1 l2)))

(defun sigmoid (z)
  "Standard sigmoid function"
  (/ 1.0 (+ 1.0 (exp (- z)))))

(defmethod train ((model logistic-regression) X Y)
  "Trains a logistic regression model on X (lists) and Y (0/1)."
  (let* ((n-features (length (first X)))
         (w (make-list n-features :initial-element 0.0))
         (b 0.0)
         (eta (eta model))
         (epochs (epochs model))
         (losses '()))
    (dotimes (epoch epochs)
      (let ((grad-w (make-list n-features :initial-element 0.0))
            (grad-b 0.0)
            (n (length X)))
        ;; Calculation of gradients
        (loop for x in X
              for y in Y
              do (let* ((z (+ (dot-product-list w x) b))
                        (y-hat (sigmoid z))
                        (error (- y-hat y)))
                   (setf grad-w (mapcar #'+ grad-w
                                        (mapcar (lambda (xi)
                                                  (* error xi))
                                                x)))
                   (incf grad-b error)))
        ;; Update settings
        (setf w (mapcar (lambda (wi gwi)
                          (- wi (* eta (/ gwi n))))
                        w grad-w))
        (setf b (- b (* eta (/ grad-b n))))
        (let ((current-loss (log-loss-from-params X Y w b)))
          (push current-loss losses)
          ;; (format t "Epoch ~a, loss=~f~%" epoch current-loss)
          )))
    
    (setf (weights model) w
          (bias model) b
          (loss-history model) (nreverse losses))
    model))

(defun log-loss-from-params (X Y w b)
  (let ((loss 0.0))
    (loop for x in X
          for y in Y
          do (let* ((z (+ (dot-product-list w x) b))
                    (y-hat (sigmoid z)))
               (incf loss
                     (+ (* y (log (+ y-hat 1e-10)))
                        (* (- 1 y) (log (+ (- 1 y-hat) 1e-10)))))))
    (/ (- loss) (length X))))

(defmethod log-loss ((model logistic-regression) X Y)
  "Calculates the log-loss on the data X and Y."
  (let ((loss 0.0))
    (loop for x in X
          for y in Y
          do (let* ((z (+ (dot-product-list (weights model) x)
                          (bias model)))
                    (y-hat (sigmoid z)))
               (incf loss
                     (+ (* y (log (+ y-hat 1e-10)))
                        (* (- 1 y) (log (+ (- 1 y-hat) 1e-10)))))))
    (/ (- loss) (length X))))

(defmethod predict-proba ((model logistic-regression) x)
  "Returns the predicted probability for point x"
  (sigmoid (+ (dot-product-list (weights model) x)
              (bias model))))

;; (defmethod predict ((model logistic-regression) x &optional (threshold 0.5))
(defmethod predict ((model logistic-regression) x)
  "Returns 1 if p>=threshold, otherwise 0."
  (if (>= (predict-proba model x) 0.5)
      1
      0))

(defmethod score ((model logistic-regression) X Y)
  "Returns the accuracy of the model."
  (let ((correct 0)
        (total (length X)))
    (loop for x in X
          for y in Y
          do (when (= (predict model x) y)
               (incf correct)))
    (/ correct (float total))))

