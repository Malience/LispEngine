
;(defun init-window ()
;  (
;    
;  )
;)

;(defclass gl-window (glut:window)()
;  (:default-initargs :width 800 :height 600 :title "Window" :mode '(:double :rgba :depth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This retarded thing won't let me make a java class, so I'm doing it in here for now

;package com.base.engine;

;import org.lwjgl.LWJGLException;
;import org.lwjgl.opengl.Display;
;import org.lwjgl.opengl.DisplayMode;

;public class Window
;{
;     public static void createWindow (int width, int height, String title)
;     {
;          Display.setTitle(title);
;          try
;          {
;               Display.setDisplayMode(new DisplayMode(width,height));
;               Display.create();
;          }	
;          catch (LWJGLException e)
;          {
;               e.printStackTrace();
;          }
;     }

    ; public static void render()
    ; {
    ;      Display.update();
    ; }

 ;    public static boolean isCloseRequested()
 ;    {
 ;         return Display.isCloseRequested();
 ;    }
;
 ;    public static int getWidth()
 ;    {
 ;         return Display.getDisplayMode().getWidth();
 ;    }

 ;    public static int getWidth()
 ;    {
 ;         return Display.getDisplayMode().getHeight();
 ;    }

 ;    public static String getTitle()
 ;    {
 ;         return Display.getTitle();
 ;    }
;}