(defun init-window ()
  (
    
  )
)

(defclass gl-window (glut:window)()
  (:default-initargs :width 800 :height 600 :title "Window" :mode '(:double :rgba :depth)))
