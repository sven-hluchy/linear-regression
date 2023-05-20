(defun invert-matrix (mat)
  "This function inverts a 2x2 matrix"
  (let* ((a (aref mat 0 0))
         (b (aref mat 0 1))
         (c (aref mat 1 0))
         (d (aref mat 1 1))
         (det (/ 1.0 (- (* a d) (* b c))))
         (inv (make-array (array-dimensions mat))))
    (progn
      (setf (aref inv 0 0) (* d det))
      (setf (aref inv 0 1) (* (- b) det))
      (setf (aref inv 1 0) (* (- c) det))
      (setf (aref inv 1 1) (* a det)))
    inv))

(defun add-matrices (m1 m2)
  "This function adds two matrices together"
  (let* ((rows (array-dimension m1 0))
        (cols (array-dimension m1 1))
        (res (make-array (list rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref res i j) (+ (aref m1 i j) (aref m2 i j)))))
    res))

(defun multiply-matrix-vector (mat vec)
  "This function calculates the product between a matrix and a vector"
  (let* ((rows (array-dimension mat 0))
         (cols (array-dimension mat 1))
         (res (make-array rows)))
    (dotimes (i rows)
      (setf (aref res i)
            (loop for j below cols
                  sum (* (aref mat i j) (aref vec j)))))
    res))
               
(defun point->mat (point)
  "This function takes a point of the form (x, y) and transforms it into
  a matrix of the form
                          x^2 x
                           x  1
  "
  (let ((x (nth 0 point)))
    (make-array '(2 2)
                :initial-contents
                `((,(expt x 2) ,x)
                 (,x 1)))))

(defun point->vec (point)
  "This function takes in a point of the form (x,y) and transforms it
  into a vector with the form (y * x, y)"
  (let ((x (nth 0 point))
        (y (nth 1 point)))
    (make-array '(2 1)
                :initial-contents
                `((,(* y x))
                  (,y)))))

(defun linear-regression (data)
  "outputs the a and b values for the linear function y = ax + b as a
  vector"
  (multiply-matrix-vector
   (invert-matrix (reduce #'add-matrices (mapcar #'point->mat data)))
   (reduce #'add-matrices (mapcar #'point->vec data))))

(defun load-data (filepath)
  "loads a lisp file and reads the data from it, this can be used in
  combination with the linear-regression function to compute the linear
  regression of the dataset"
  (with-open-file (in filepath :if-does-not-exist nil)
    (if in
        (read in)
        (error "File ~a not found" filepath))))
    
