

(defparameter running nil)
(defparameter main-window (make-instance 'simple-viewer))

(defun start ()
  (start-core-engine))

(defun start-core-engine () 
  ;(setf (viewer capi:interface) main-window)
  (setf v (capi:display main-window))
  ;(init-viewer)
  (init-graphics)
 ; (run-core-engine)
)

(defun run-core-engine ()  
  (setf running t)
  (loop
   (if (not running) nil
         
     (render-world)
     )	
   )
  )