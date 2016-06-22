

(defparameter speed 5)
(defparameter sensitivity 1.0)


(defun window-input-callback (char)
    (input window char))

(defun input (char)
  (case char
    (#\x  (setf running nil))
    (#\w  (move (make-vec3f :z 1.0))) ;Forwards
    (#\a  (move (make-vec3f :x 1.0)))
    (#\s  (move (make-vec3f :z -1.0)))
    (#\d  (move (make-vec3f :x -1.0)))))

(defun mouse-1 (canvas x y)
  (setf (lastxy main-window) (list x y))
  "Hello!")

(defun mouse-2 (canvas x y))

(defun mouse-1-motion (canvas x y)
  (if (null (lastxy main-window)) nil
    (opengl:rendering-on ((canvas main-window))
      (rotate (world-transform main-window) (- x (car (lastxy main-window))) (- y (cadr (lastxy main-window)))))))


(defun move (dir)
  (vector-mult dir speed dir)
  (vector-sum camera-loc dir camera-loc))
  

(defun rotate (matrix x y)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-rotated (float (* x sensitivity) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (opengl:gl-rotated (float (* (- y) sensitivity) 1.0d0) 1.0d0 0.0d0 0.0d0)
    (opengl:gl-mult-matrixd matrix)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* matrix)))
