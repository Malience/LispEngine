;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/icosahedron.lisp,v 1.20.2.1 2011/08/24 13:27:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "USER")

;;; This file contains a demonstration of the LispWorks FLI for
;;; OpenGL.  To see the demonstration, compile and load the system
;;; OPENGL-EXAMPLES by
;;;
;;;    (load "<opengl-directory>/host")
;;;    (load "OPENGL:EXAMPLES;load")
;;;
;;; then evaluate
;;;
;;;    (setf v (capi:display (make-instance 'icosahedron-viewer)))

;;; ------------------------------------------------------------
;;; Types and creators.

(deftype gl-double () 'double-float)
(deftype gl-double-vector (n) `(opengl:gl-vector :double ,n))
(deftype gl-single () 'single-float)
(deftype gl-single-vector (n) `(opengl:gl-vector :float ,n))


(defun gl-float-vector (type contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

(defun gl-double-vector (&rest contents)
  (gl-float-vector :double contents))

(defun gl-single-vector (&rest contents)
  (gl-float-vector :float contents))


(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))

(defun make-gl-single-vector (size)
  (opengl:make-gl-vector :float size))


;;; ------------------------------
;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C'
;;; ------------------------------

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (contents)
  (mapcar #'(lambda (c) (apply 'gl-double-vector c)) contents))

;;; ------------------------------------------------------------
;;; Geometric objects


(defun make-object-vertexes (vertex-list)
  (coerce (gl-vertexes vertex-list) 'simple-vector))

(defun make-object-indexes (index-list)
  (let* ((biggest (loop for i in index-list maximize (length i)))
         (indexes (make-array (list (length index-list) biggest)
                              :element-type 'fixnum
                              :initial-element -1)))
    (loop for id in index-list
          for i from 0 do
          (loop for jd in id
                for j from 0 do
                (setf (aref indexes i j) jd)))
    indexes))

;;; ------------------------------------------------------------
;;; Icosahedron

(defvar *icosahedron-vertexes*
  (let ((x 0.525731112119133606d0)
        (z 0.850650808352039932d0))
    (list (list (- x) 0.0d0 z     1.0d0) (list x     0.0d0 z     1.0d0) 
          (list (- x) 0.0d0 (- z) 1.0d0) (list x     0.0d0 (- z) 1.0d0)
          (list 0.0d0 z     x     1.0d0) (list 0.0d0 z     (- x) 1.0d0)
          (list 0.0d0 (- z) x     1.0d0) (list 0.0d0 (- z) (- x) 1.0d0)
          (list z     x     0.0d0 1.0d0) (list (- z) x     0.0d0 1.0d0)
          (list z     (- x) 0.0d0 1.0d0) (list (- z) (- x) 0.0d0 1.0d0))))

(defconstant *icosahedron-cw-indexes*
  '((0 4 1) (0 9 4) (9 5 4) (4 5 8) (4 8 1)
    (8 10 1) (8 3 10) (5 3 8) (5 2 3) (2 7 3)
    (7 10 3) (7 6 10) (7 11 6) (11 0 6) (0 1 6)
    (6 1 10) (9 0 11) (9 11 2) (9 2 5) (7 2 11)))

(defconstant *icosahedron-indexes*
  '((1 4 0) (4 9 0) (4 5 9) (8 5 4) (1 8 4)
    (1 10 8) (10 3 8) (8 3 5) (3 2 5) (3 7 2)
    (3 10 7) (10 6 7) (6 11 7) (6 0 11) (6 1 0)
    (10 1 6) (11 0 9) (2 11 9) (5 2 9) (11 2 7)))

(defun make-icosahedron-vertexes ()
  (make-object-vertexes *icosahedron-vertexes*))

(defun make-icosahedron-indexes ()
  (make-object-indexes *icosahedron-indexes*))

(defclass icosahedron (geom-object)
  ()
  (:default-initargs
   :vertexes *icosahedron-vertexes*
   :indexes *icosahedron-indexes*
   :name "Icosahedron"
   ))

(defmethod draw ((icosahedron icosahedron))
  (let* ((colors (colors icosahedron))
         (vertexes (vertexes icosahedron))
         (indexes (indexes icosahedron))
         (n1 (make-gl-double-vector 3))
         (n2 (make-gl-double-vector 3))
         (n3 (make-gl-double-vector 3))
         (texturep (texturep icosahedron)))
    
    (when texturep
      (setf colors nil) 
      (opengl:gl-color4-fv (get-texture-color)))       
    (labels ((tri-simple (v1 v2 v3 normalp)                         ;; Draw a basic textured triangle
                         (multiple-value-bind (ts1 tt1 ts2 tt2 ts3 tt3) 
                             (tri-texture-coords v1 v2 v3)
                           (labels ((v-simple (ts tt v)
                                              (opengl:gl-tex-coord2-f (float ts 1.0) (float tt 1.0))
                                              (when normalp (opengl:gl-normal3-dv v))
                                              (opengl:gl-vertex4-dv v)))
                             (opengl:gl-begin opengl:*gl-polygon*)
                             (v-simple ts1 tt1 v1)
                             (v-simple ts2 tt2 v2)
                             (v-simple ts3 tt3 v3)
                             (opengl:gl-end))))

             ;; Test if 3 vertexes cross the XZ plane and if they do draw two textured
             ;; triangles which don't. 
             (tri-XZ-cross (v1 v2 v3 normalp)
                           (labels ((XZ-cross (v1 v2)
                                              (flet ((lindiv (a b i) (* 0.5d0 (+ (opengl:gl-vector-aref a i)
                                                                                 (opengl:gl-vector-aref b i)))))
                                                (let* ((s1 (signum (opengl:gl-vector-aref v1 1)))
                                                       (s2 (signum (opengl:gl-vector-aref v2 1))))
                                                  (when (minusp (* s1 s2))         ; return the new interpolated vertex
                                                    (gl-vertex (lindiv v1 v2 0) (lindiv v1 v2 1) (lindiv v1 v2 2) 1.0d0)))))
                                    (triv (va vb vc)
                                          (let ((vv (XZ-cross va vb)))
                                            (when vv
                                              (tri-simple va vv vc normalp)
                                              (tri-simple vb vc vv normalp)
                                              T))))
                             (or (triv v1 v2 v3)
                                 (triv v2 v3 v1)
                                 (triv v3 v1 v2))))

             ;; Draw a textured triangle, breaking it into two triangles when it intersects the XZ plane
             ;; in order to get the texture coords right.
             (texture-tri (v1 v2 v3 normalp)
                          (or (tri-XZ-cross v1 v2 v3 normalp)
                              (tri-simple v1 v2 v3 normalp)))

             ;; Draw a single triangle, maybe with texture, include per-vertex normals if normalp
             (vertex3 (v1 v2 v3 normalp)
                      (if texturep
                          (texture-tri v1 v2 v3 normalp)
                        (progn
                          (opengl:gl-begin opengl:*gl-polygon*)
                          (when normalp (opengl:gl-normal3-dv v1))
                          (opengl:gl-vertex4-dv v1)
                          (when normalp (opengl:gl-normal3-dv v2))
                          (opengl:gl-vertex4-dv v2)
                          (when normalp (opengl:gl-normal3-dv v3))
                          (opengl:gl-vertex4-dv v3)
                          (opengl:gl-end))))

             ;; Draw a single triangle, either with each vertex having its own normal (smoothp)
             ;; or with a single shared normal.
             (draw-tri (v1 v2 v3 color smoothp)
                       (if smoothp
                           (progn
                             (when color
                               (opengl:gl-color4-fv color))
                             (vertex3 v1 v2 v3 T))
                         (progn
                           (vector-difference v1 v2 n1)
                           (vector-difference v2 v3 n2)
                           (normalized-cross-product n1 n2 n3)
                           (when color
                             (opengl:gl-color4-fv color))
                           (opengl:gl-normal3-dv n3)
                           (vertex3 v1 v2 v3 nil)))))
      (if (smoothp icosahedron)
          (opengl:gl-shade-model opengl:*gl-smooth*)
        (opengl:gl-shade-model opengl:*gl-flat*))

      ;; The following draws an icosahedron, each triangle of which may be subdivided n times.
      (if (subdivision icosahedron)
          (labels ((subdivide (a b c depth i)
                              (if (zerop depth)
                                  (draw-tri a b c (when colors
                                                    (aref colors i)) (smoothp icosahedron))
                                (opengl:with-gl-vectors 
                                    ((ab :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0))
                                     (bc :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0))
                                     (ca :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0)))
                                  (let ((d (1- depth)))
                                    (vector-sum a b ab) (normalize ab)
                                    (vector-sum b c bc) (normalize bc) 
                                    (vector-sum c a ca) (normalize ca)
                                    (subdivide a ab ca d i)
                                    (subdivide b bc ab d i)
                                    (subdivide c ca bc d i)
                                    (subdivide ab bc ca d i))))))
            (loop for i below 20 do
                  (subdivide (aref vertexes (aref indexes i 0))
                             (aref vertexes (aref indexes i 1))
                             (aref vertexes (aref indexes i 2))
                             (subdivision icosahedron)
                             i)))
        (loop for i below 20 do
              (draw-tri (aref vertexes (aref indexes i 0))
                        (aref vertexes (aref indexes i 1))
                        (aref vertexes (aref indexes i 2)) 
                        (when colors
                          (aref colors i))
                        (smoothp icosahedron))
              )))))
;;; The CAPI Interface
    
(defun make-default-background ()
  (gl-single-vector 0.5 0.5 0.5 1.0))

(eval-when (:load-toplevel :execute)
  (gp:register-image-translation :up *up-arrow*)
  (gp:register-image-translation :down *down-arrow*)
  (gp:register-image-translation :up-disabled *up-disabled*)
  (gp:register-image-translation :down-disabled *down-disabled*))

(capi:define-interface icosahedron-viewer (capi:interface)
  ((double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)
   (background :initform (make-default-background) :initarg :background :accessor background)
   (camera :initform (make-camera) :initarg :camera :accessor camera)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (geom-object :initform (make-instance 'icosahedron :use-display-list t)
                :initarg :icosahedron :initarg :object :accessor icosahedron :accessor object)
   (texture-image :initform (get-icosahedron-texture-data) :initarg :texture-image :accessor texture-image)
   (texture-filter :initform opengl:*gl-nearest* :initarg :texture-filter :accessor texture-filter)
   (texturep :initform nil :initarg :texturep :accessor texturep)
   (smoothp :initform nil :initarg :smoothp :accessor smoothp)
   (subdivision-buttons :initform (make-ico-button-panel  (list #\> #\<) "Subdiv."))
   (specular-buttons :initform (make-ico-button-panel (list #\s #\S) "Spec."))
   (emission-buttons :initform (make-ico-button-panel (list #\e #\E) "Em."))
   (ambient-buttons :initform (make-ico-button-panel (list #\a #\A) "Ambient"))
   (shine-buttons :initform (make-ico-button-panel (list #\p #\P) "Shine.")))
  (:panes 
   (canvas opengl:opengl-pane
	   :configuration (list :rgba t :depth nil :double-buffered double-buffered-p)
           :min-width 400
           :min-height 400
           :message "Mouse-L spins the object, Mouse-R moves the light, Shift Mouse-L moves your view."
	   :display-callback 'redisplay-canvas
	   :resize-callback 'resize-canvas
	   :input-model '(((:button-1 :press) viewer-button-1)
			  ((:button-2 :press) viewer-button-2)
			  ((:button-3 :press) viewer-button-2)
			  ((:button-1 :shift :press) viewer-button-1-shift)
			  ((:motion :button-1) viewer-motion-button-1)
			  ((:motion :button-2) viewer-motion-button-2)
			  ((:motion :button-3) viewer-motion-button-2)
			  ((:motion :button-1 :shift) viewer-motion-button-1-shift))
           :reader canvas
           :font (gp:make-font-description :family "Arial"))
   (shade-buttons capi:check-button-panel
                  :callback-type :interface-data
                  :selection-callback 'process-character
                  :retract-callback 'process-character
                  :layout-class 'capi:column-layout
                  :title "Shading"
                  :title-position :frame
                  :items (list #\o)
                  :print-function #'(lambda (x) (getf '(#\o "Smooth") x)))
   (texture-buttons capi:check-button-panel
                    :callback-type :interface-data
                    :selection-callback 'process-character
                    :retract-callback 'process-character
                    :layout-class 'capi:row-layout
                    :title "Texture"
                    :title-position :frame
                    :items (list #\t #\n)
                    :print-function #'(lambda (x) (getf '(#\t "On"  #\n "Nice") x)))
   (reset-buttons capi:button-panel
                  :interaction :no-selection
                  :title "Reset"
                  :title-position :frame
                  :callback-type :interface-data
                  :selection-callback 'process-character
                  :layout-class 'capi:row-layout
                  :items (list #\home #\Insert)
                  :print-function #'(lambda (x) (getf '(#\home "View" #\Insert "Material") x)))
   (quit-button capi:button-panel
                :interaction :no-selection
                :callback-type :interface-data
                :selection-callback 'process-character
                :layout-class 'capi:column-layout
                :items (list #\escape)
                :print-function #'(lambda (x) x "Quit")))

  (:layouts
   (main capi:column-layout '(top bottom canvas))
   (top capi:row-layout '(reset-buttons shade-buttons texture-buttons) :x-adjust :right :y-adjust :center :max-width T)
   (object capi:row-layout '(subdivision-buttons) :title "Object" :title-position :frame)
   (material capi:row-layout '(specular-buttons emission-buttons shine-buttons)
             :title "Material" :title-position :frame)
   (light capi:row-layout '(ambient-buttons)
             :title "Light" :title-position :frame)

   (buttons capi:row-layout '(object light material))
   (bottom capi:row-layout '(buttons quit-button)))
  (:default-initargs :auto-menus NIL :title "OpenGL Viewer"))

(defmethod initialize-instance :after ((self icosahedron-viewer) &key &allow-other-keys)
  (setf (viewer (object self)) self))

(defun initialize-viewer (icosahedron-viewer)
  ;; Initialize the icotransform to unity.
  (opengl:rendering-on ((canvas icosahedron-viewer))
    (setf (icotransform icosahedron-viewer) (make-gl-double-vector 16))
    (setf (light-transform icosahedron-viewer) (make-gl-double-vector 16))
    (initialize-transform (icotransform icosahedron-viewer))
    (initialize-transform (light-transform icosahedron-viewer))
    (reset-lights-and-materials)))

(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun viewer-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-icosahedron viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-motion-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-light viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (let ((eye (eye (camera viewer))))
          (setf (xyz-y eye)
                (max (+ (xyz-y eye) (/ (- (car last) x) 20)) 2d0)))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defparameter *pointer-rotation-gain* 0.4d0)

(defun polar-rotate (transform dx dy)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-rotated (float (* dx *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (opengl:gl-rotated (float (* (- dy) *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0)
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun polar-rotate-light (viewer dx dy)
  (polar-rotate (light-transform viewer) dx dy))

(defun polar-rotate-icosahedron (viewer dx dy)
  (polar-rotate (icotransform viewer) dx dy))


(defun viewer-character-callback (canvas x y character)
  x y
  (with-slots ((viewer capi:interface)) canvas
    (process-character viewer character)))
     

(defun redisplay-canvas (canvas &rest ignore)
  ignore
  (with-slots ((viewer capi:interface)) canvas
    (unless (icotransform viewer)
      (initialize-viewer viewer))
    (opengl:rendering-on (canvas)

      (draw (camera viewer))

      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (light-transform viewer))

        (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* *light-model-ambient*)
        (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 0.0)
        (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 0.0)

        (opengl:gl-enable opengl:*gl-light0*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-position* *light-position*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-ambient* *light-ambient*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-diffuse* *light-diffuse*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-specular* *light-specular*))

      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (icotransform viewer))
        (opengl:gl-cull-face opengl:*gl-back*)
        (opengl:gl-enable opengl:*gl-cull-face*)

        (opengl:gl-enable opengl:*gl-color-material*)
        (opengl:gl-color-material opengl:*gl-front* opengl:*gl-ambient-and-diffuse*)

        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-specular* *material-specular*)
        (opengl:gl-materialf opengl:*gl-front* opengl:*gl-shininess* *material-shininess*)
        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-emission* *material-emission*)

        (draw (icosahedron viewer)))

      (when (double-buffered-p viewer)
        (opengl:swap-buffers canvas)))))

(defun resize-canvas (canvas x y width height)
  x y
  (when #+Win32 (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
	#-Win32 T
    (opengl:rendering-on (canvas)
      (opengl:gl-viewport 0 0 width height))
    (with-slots ((viewer capi:interface)) canvas
      (setf (aspect (projection (camera viewer)))
            (coerce (/ width height) 'double-float)))
    (redisplay-canvas canvas)))