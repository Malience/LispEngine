;;WINDOW STUFF


(defparameter fov 70.0d0)
(defparameter width 800.0d0)
(defparameter height 600.0d0)
(defparameter aratio (/ width height))
(defparameter near 1.0d0)
(defparameter far 1000.0d0)

(defparameter up (make-vec3f :y 1.0))
(defparameter forward (make-vec3f :z 5.0))
(defparameter camera-loc (make-vec3f))

(capi:define-interface main-window-interface (capi:interface)
  ((lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (world-transform :initform nil :initarg :world-transform :accessor world-transform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   )
  (:panes
   (canvas opengl:opengl-pane
           :configuration (list :rgba t :depth nil :double-buffered t)
                :min-width 400
                :min-height 400
                :message "Shoot em!"      
           :display-callback 'render-world
           ;:resize-callback 'resize-window
           :input-model '(((:button-1 :press) mouse-1)
			  ((:button-2 :press) mouse-2)	 
			  ((:motion :button-1) mouse-1-motion))
                :reader canvas))
  (:default-initargs :auto-menus nil :title "Hello!")
  )


(capi:define-interface simple-viewer (capi:interface)
  ((double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)   ;;Defaults from Icosahedron
   (background :initform (make-default-background) :initarg :background :accessor background)
   (camera :initform (make-camera) :initarg :camera :accessor camera)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (geom-object :initform (make-instance 'cube :use-display-list t)
                :initarg :icosahedron :initarg :object :accessor icosahedron :accessor object)
   (texture-image :initform (get-icosahedron-texture-data) :initarg :texture-image :accessor texture-image)
   (texture-filter :initform opengl:*gl-nearest* :initarg :texture-filter :accessor texture-filter)
   (texturep :initform nil :initarg :texturep :accessor texturep)
   (smoothp :initform nil :initarg :smoothp :accessor smoothp))
  (:panes 
   (canvas opengl:opengl-pane
	   :configuration (list :rgba t :depth nil :double-buffered double-buffered-p)
           :min-width 400
           :min-height 400
           :message "Stuff"
	   :display-callback 'redisplay-canvas
	   :resize-callback 'resize-canvas
	   :input-model '(((:button-1 :press) viewer-button-1)
			  ((:button-2 :press) viewer-button-2)
			  ((:button-3 :press) viewer-button-2)
			  ((:button-1 :shift :press) viewer-button-1-shift)
			  ((:motion :button-1) viewer-motion-button-1)
			  ((:motion :button-1 :shift) viewer-motion-button-1-shift))
           :reader canvas
   ))
   ;Edited from Icosahedron
   (:default-initargs :auto-menus NIL :title "An attempt"))


(defun init-viewer ()
  (opengl:rendering-on ((canvas main-window))
    (setf (world-transform main-window) (make-matrix4f))
    (setf (light-transform main-window) (make-matrix4f))
    (init-identity (world-transform main-window))
    (init-identity (world-transform main-window))))

;;END WINDOW STUFF


(defun init-graphics ()
  (opengl:gl-clear-color 0.0 1.0 1.0 1.0)  ;;RGBA, don't touch the last number 0.0-1.0
)  


(defun render-world ()
  (opengl:rendering-on ((canvas main-window))

    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (opengl:glu-perspective fov aratio near far)

    (opengl:gl-matrix-mode opengl:*gl-modelview*)
    (opengl:gl-load-identity)
    (opengl:glu-look-at (vec3f-x forward) (vec3f-y forward) (vec3f-z )forward
                        (vec3f-x camera-loc) (vec3f-y camera-loc) (vec3f-z camera-loc)
                        (vec3f-x up) (vec3f-y up) (vec3f-z up))
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)
    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)


    (opengl:with-matrix-pushed
      (opengl:gl-mult-matrixd (world-transform viewer))
      (opengl:gl-cull-face opengl:*gl-back*)
      (opengl:gl-enable opengl:*gl-cull-face*))

    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)
    (opengl:gl-enable opengl:*gl-lighting*)

    (render-all world)
  
    (opengl:gl-disable opengl:*gl-lighting*)
    (opengl:gl-disable opengl:*gl-depth-test*)
    (opengl:gl-disable opengl:*gl-cull-face*)


    (opengl:swap-buffers (canvas main-window))
      
      
  ))
  
  
                      
(defun render-all (objects)
  (if (null objects) nil
    (let object (car objects)
      (declare (type GameObject object))
      (render object)
      (render-all (cdr objects)))))





