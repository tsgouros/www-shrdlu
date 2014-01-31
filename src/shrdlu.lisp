#!/usr/bin/clisp
;
; This file loads in all the needed files for SHRDLU.
;
(defpackage shrdlu
  (:use :common-lisp))
(in-package :shrdlu)

; This appears to be the width of the terminal
; No, it's not.  Not sure what it is....
(DEFCONSTANT CHRCT 76)

(setq custom:*load-paths* (cons "/home/tsgouros/shrdlu/src/" custom:*load-paths*))

(princ custom:*load-paths*)

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
(USERMODE)
;(DEBUGMODE)

;; Parse and distribute the input arguments.  These identify the
;; server and port to which the move commands are to be delivered.
(if (cadr EXT:*ARGS*) 
    (progn
      (setq remote-cmds-p t)
      (setq remote-actions-p t)
      (setq remote-host (car EXT:*ARGS*))
      (setq remote-port (parse-integer (cadr EXT:*ARGS*))) )
    (progn
      (setq remote-cmds-p nil)
      (setq remote-actions-p nil)))

; Start the program
(INITIALSTUFF 'UMR-1.0 NIL)

