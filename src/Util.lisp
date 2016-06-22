(deftype vector3f (opengl-vector :float 3))

(defstruct vec3f
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(defun make-vector3f (x y z)
  (opengl:make-gl-vector :float 3 :contents (list x y z)))

(defun vector-sum (v1 v1 out)
  (setf (opengl:gl-vector-aref out 0) (+ (opengl:gl-vector-aref v1 0) (opengl:gl-vector-aref v2 0))
        (opengl:gl-vector-aref out 1) (+ (opengl:gl-vector-aref v1 1) (opengl:gl-vector-aref v2 1))
        (opengl:gl-vector-aref out 2) (+ (opengl:gl-vector-aref v1 2) (opengl:gl-vector-aref v2 2))))

(defun vector-diff (v1 v1 out)
  (setf (opengl:gl-vector-aref out 0) (- (opengl:gl-vector-aref v1 0) (opengl:gl-vector-aref v2 0))
        (opengl:gl-vector-aref out 1) (- (opengl:gl-vector-aref v1 1) (opengl:gl-vector-aref v2 1))
        (opengl:gl-vector-aref out 2) (- (opengl:gl-vector-aref v1 2) (opengl:gl-vector-aref v2 2))))

(defun vector-mult (vec mul out)
  (setf (opengl:gl-vector-aref out 0) (* (opengl:gl-vector-aref vec 0) mul)
        (opengl:gl-vector-aref out 1) (* (opengl:gl-vector-aref vec 1) mul)
        (opengl:gl-vector-aref out 2) (* (opengl:gl-vector-aref vec 2) mul))
  out)
 

(defun vector-normal (vec)
  (let* ((x (opengl:gl-vector-aref vec 0))
         (y (opengl:gl-vector-aref vec 1))
         (z (opengl:gl-vector-aref vec 2))
         (w (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (eql w 0 ) nil
      (setf (opengl:gl-vector-aref vec 0) (/ x w)
            (opengl:gl-vector-aref vec 1) (/ y w)
            (opengl:gl-vector-aref vec 2) (/ z w)))vec))

(defun vector-cross (v1 v2 out)
  ;;(progn ;;(setf out (new-vector 0.0 0.0 0.0))
         (setf (opengl:gl-vector-aref out 0) (- (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 2))
                                           (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 1)))
               (opengl:gl-vector-aref out 1) (- (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 0))
                                           (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 2)))
               (opengl:gl-vector-aref out 2) (- (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 1))
                                           (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 0))))
         );)

(defun make-matrix4f ()
  (opengl:make-gl-vector :double 16))

(defun init-identity (matrix)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* matrix)))

(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))


(defparameter world ())


(defmethod add-object ((object GameObject))
  (setq world (append world (list object))))

(defmethod remove-object ((object GameObject))
  (remove-object-1 object world ()))
                   
  
(defmethod remove-object-1 ((object GameObject) lst out)
  (if (null (car lst)) world
    (if (eq (car lst) object) (setq world (append out (cdr lst)))
      (remove-object-1 object (cdr lst) (append out (list (car lst))))
      )))