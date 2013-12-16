;
; This file loads in all the needed files for SHRDLU.
; 
(defpackage shrdlu
  (:use :common-lisp))
(in-package :shrdlu)

; This appears to be the width of the terminal
; No, it's not.  Not sure what it is....
(DEFCONSTANT CHRCT 76)

(load "remote")
(load "fixes")
(load "progmr")
(load "cgram")
(load "syscom")
(load "smutil")
(load "smspec")
(load "smass")
(load "dictio")
(load "morpho")
(load "show")
(load "blockl")
(load "plnr")
(load "setup")
(load "thtrac")
(load "blockp")
(load "data")
(load "newans")
(load "mover")

; Set either (DEBUGMODE) or (USERMODE) here.
;(USERMODE)
(DEBUGMODE)

(setq remote-cmds-p nil)

; Start the program
(INITIALSTUFF 'UMR-1.0 NIL)

