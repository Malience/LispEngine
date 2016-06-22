;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/excel:load-dde-demo.lisp,v 1.3.13.1 2011/08/24 13:27:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "../../host"))
(load (current-pathname "../load"))

(require "dde")

(load (current-pathname "demo" nil))

;; Use (demo) to start the viewer.

;; Open the spreadsheet and click on the pictures to transfer the data
;; from Excel to LispWorks.
