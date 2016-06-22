
(defclass Renderer (geom-object)
  ((vertexes :accessor vertices :initform nil :initarg :vertices)
   (indexes  :accessor indices  :initform nil :initarg :indices )
  )
  )

(defmethod render ((rend Renderer))
  (let* ((vertices (vertices rend))
         (indices  (indices rend ))
         (n (array-dimension indices 1))
         (m (array-dimension indices 0))
        )
    (opengl:with-gl-vectors ((n1 :type :float :length 3)
                             (n2 :type :float :length 3)
                             (n3 :type :float :length 3))
      (opengl:gl-shade-model opengl:*gl-flat*)
      (loop for i below m do
            (let ((v1 (aref vertices (aref indices i 0)))
                  (v2 (aref vertices (aref indices i 1)))
                  (v3 (aref vertices (aref indices i 2))))

              (opengl:gl-begin opengl:*gl-polygon*)
              (vector-diff v1 v2 n1)
              (vector-diff v2 v3 n2)
              (vector-cross n1 n2 n3)
              (vector-normal n3)
              (opengl:gl-normal3-dv n3)
              (loop for j below n
                    as index = (aref indices i j)
                    unless (minusp index) do
                    (opengl:gl-vertex4-dv (aref vertices index)))
              (opengl:gl-end))))))
