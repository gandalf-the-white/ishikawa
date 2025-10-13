(defpackage :algebra
  (:use :cl)
  (:export :show-matrix
   :dot-product-list))

(in-package :algebra)

(defun show-matrix (matrix)
  (if (null matrix)
      (error "The matrix is empty~%")
      (let* ((rows (length matrix))
             (cols (length (first matrix)))
             (widths (mapcar (lambda (col)
                               (loop for rowA in matrix
                                     maximize (length (format nil "~a" (nth col rowA)))))
                             (loop for i from 0 below cols collect i))))
        (format t "Matrix (~dx~d):~%" rows cols)
        (loop for rowB in matrix do
          (loop for elem in rowB
                for width in widths do
                  (format t "~v,,vA " width 0 elem))
          (format t "~%"))))) 

(defun multiply-matrix (matrixA matrixB)
  (let ((rowsA (length matrixA))
        (colsA (length (first matrixA)))
        (rowsB (length matrixB))
        (colsB (length (first matrixB))))
    (if (/= colsA rowsB)
        (error "Incompatible Dimensions for matrix multiplication.")
        (loop for i from 0 below rowsA
              collect
              (loop for j from 0 below colsB
                    collect
                    (loop for k from 0 below colsA
                          sum (* (elt (elt matrixA i) k)
                                 (elt (elt matrixB k) j))))))))

(defun add-matrix (matrixA matrixB)
  (let ((lrowsA (length matrixA))
        (lrowsB (length matrixB)))
    (unless (= lrowsA lrowsB)
      (error "Incompatible Dimensions for matrix addition.")
      (loop for rowA in matrixA
            for rowB in matrixB
            for lcolsA = (length rowA)
            for lcolsB = (length rowB)
            collect (mapcar #'+ rowA rowB))
      (error "Incompatible Dimensions for matrix addition."))))

(defun transpose-matrix (matrix)
  ;; Returns the transpose of the given matrix.
  ;; The matrix must be a list of lists of the same length.
  (when (null matrix)
    (return-from transpose-matrix '()))
  (let* ((nb-lignes (length matrix))
         (nb-colonnes (length (first matrix)))
         (transposee (make-array (list nb-colonnes nb-lignes) :initial-element 0)))
    ;; Check that all lines have the same length
    (unless (every (lambda (ligne) (= (length ligne) nb-colonnes)) matrix)
      (error "Toutes les lignes de la matrix doivent avoir la même longueur."))
    ;; Filling the transposed matrix
    (loop for i from 0 below nb-lignes do
      (loop for j from 0 below nb-colonnes do
        (setf (aref transposee j i) (nth j (nth i matrix)))))
    ;; Converting the array to a list of lists
    (loop for j from 0 below nb-colonnes
          collect
          (loop for i from 0 below nb-lignes
                collect
                (aref transposee j i)))))
