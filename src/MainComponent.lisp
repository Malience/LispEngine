;; This retarded thing won't let me make a java class, so I'm doing it in here for now

;package com.base.engine;

;public class MainComponent
;{
;     public static final int WIDTH = 800;
;     public static final int HEIGHT = 800;
;     public static final String TITLE = "Frigging Window";

;     public MainComponent()
;     {

 ;    }

 ;    public void start()
 ;    {
 ;         run();
 ;    }

  ;   public void stop()
  ;   {

  ;   }

   ;  public void run()
   ;  {
    ;      while(!Window.isCloseRequested())
     ;     {
      ;         render();
       ;   }
    ; }

     ;public void render()
     ;{
     ;     Window.render();
     ;}

     ;public void cleanUp()
     ;{

     ;}

     ;public static void main(String[] args)
     ;{
      ;    Window.createWindow(WIDTH, HEIGHT, TITLE);
          
      ;    MainComponent game = new MainComponent();
       ;   game.start();
     ;}
;}