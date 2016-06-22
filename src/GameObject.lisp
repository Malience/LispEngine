

(defclass GameObject ()
	((render 	:accessor get-render
                        :initform nil
                        :initarg  :render
	)))

(defmethod getRender ((object GameObject))
  (get-render object))

(defmethod setRender ((object GameObject) (render renderer))
  (setq (get-render object) render))

(defmethod render ((object GameObject))
  (render (get-render object)))


;(defparameter cube-vertices
;  '((1.0d0 1.0d0 -1.0d0) (-1.0d0 1.0d0 -1.0d0) (1.0d0 -1.0d0 -1.0d0) (-1.0d0 -1.0d0 -1.0d0)
;    (1.0d0 1.0d0 1.0d0) (-1.0d0 1.0d0 1.0d0) (1.0d0 -1.0d0 1.0d0) (-1.0d0 -1.0d0 1.0d0)))

(defun make-cube-vertices (x y z halfsize)
  (list (list (+ x halfsize) (+ y halfsize) (+ z halfsize))
        (list (+ x halfsize) (+ y halfsize) (- z halfsize))
        (list (+ x halfsize) (- y halfsize) (+ z halfsize))
        (list (+ x halfsize) (- y halfsize) (- z halfsize))
        (list (- x halfsize) (+ y halfsize) (+ z halfsize))
        (list (- x halfsize) (+ y halfsize) (- z halfsize))
        (list (- x halfsize) (- y halfsize) (+ z halfsize))
        (list (- x halfsize) (- y halfsize) (- z halfsize))))

(defconstant cube-indices
  '(    (7 4 5)
	(4 7 6)			
	(3 0 2)
	(0 3 1)			
	(4 2 0)
	(2 4 6)			
	(7 1 3)
	(1 7 5)			
	(5 0 1)
	(0 5 4)			
	(6 3 2)
	(3 6 7)))
    
    

(defclass Cube (geom-object)
  ()
  (:default-initargs 
       ; :render (make-instance 'renderer 
                               :vertexes (make-cube-vertices .5d0 0 .85d0 10.0d0) ;Both from Icosohedron
                               :indexes  cube-indices
                               :name "Cube"))


(defclass ico (GameObject)
  ()
  (:default-initargs
        :render (make-instance 'renderer
                               :vertices (make-icosahedron-vertexes)
                               :indices  (make-icosahedron-indexes))))