(in-package :shrdlu)
; Contains functions related to the communication between SHRDLU and
; the display running in a browser.  Meant to communicate via the
; queue-server.js server.
;
; The communication with the display runs over three separate channels:
;   - commands are typed by the user and delivered from the display to
;     the running SHRDLU code
;   - reponses are the replies to those commands made by SHRDLU that are
;     to be displayed to the user.
;   - actions are the actual move commands that control the display of 
;     block and pyramids, see mover.lisp for a list.
;
; -ts. 12/14/2013


(defvar remote-host "localhost" 
  "Name of host where the graphic interface resides.  It is probably a
  severe security hole to put this anywhere besides on the localhost,
  but we leave it in a variable for development.  For deployment
  consider hard-coding this.")

(defvar remote-port 1337 
  "The port on the remote host with which to communicate.")

(defvar cmd-buffer "" 
  "buffer to hold characters received from the remote source")

(defvar remote-cmds-p nil
  "Whether the user dialog is being conducted remotely or not.  Set to
  t to use web interface.")

(defvar remote-cmd-buffer-polling-freq 0.2
  "SHRDLU will poll the queue server for user-issued commands, at an
  interval of this many seconds.")

(defvar remote-actions-p t
  "Flag to indicate whether we're displaying the actions on the remote
  display.  Note that this is commonly done even when the commands and
  responses are not displayed remotely.")

(defvar response-buffer "" 
  "Buffer used to reply to the user in dialog.")

(defun socket-cmd (host page &optional (port 1337))
  "Sends a GET HTTP request to some HOST on some PORT.  Return value
  is the last line of the response we get.  This is meant to work with
  one line messages and the queue-server.js code and might not be that
  useful in a general sense."
  ;; HTTP requires the :DOS line terminator
  (with-open-stream 
      (socket (socket:socket-connect port host :external-format :dos))
     (format socket "GET ~A HTTP/1.0~2%" page)
     ;; dump the whole thing - header+data
     (let ((out ""))
       (loop :for line = (read-line socket nil nil) :while line 
	  :do (setq out line))
       out)))

(defun remote-action (cmd)
  "Sends an action command.  This is a command that actually changes the
  display: move, grasp, release, etc."
  (if remote-actions-p
      (socket-cmd remote-host (format nil "/?act=~A" cmd) remote-port)))


(defun read-remote ()
  "Replacement for (read) that operates remotely.  Used to receive user
  sentences."
  (if remote-cmds-p
      (prog2
	(reload-cmd-buffer)
	(subseq cmd-buffer 0 (position #\  cmd-buffer))
	(setq cmd-buffer ""))
      (read)))

(defun peek-char-remote () 
  "Replacement for peek-char, meant to operate on a buffer, received
  from a remote call.  Used to receive user sentences."
  (if remote-cmds-p 
      (progn
	(reload-cmd-buffer)
	(char cmd-buffer 0))
      (peek-char)))

(defun read-char-remote () 
  "Replacement for read-char, meant to operate remotely if necessary.
  Used to receive user sentences."
  (if remote-cmds-p 
      (progn
	(reload-cmd-buffer)
	(let ((outchar (char cmd-buffer 0)))
	  (setq cmd-buffer (subseq cmd-buffer 1))
	  outchar))
      (read-char)))

(defun purge-cmd-buffer () 
  "Clear the command-receiving buffer."
  (setq cmd-buffer ""))

(defun reload-cmd-buffer ()
  "We're out of characters in the command (user sentence) buffer.
  Query the queue server to get some more."
  (if (= 0 (length cmd-buffer))
      (loop :for tmp = (setq cmd-buffer 
			     (socket-cmd remote-host "/?cmdget=top" remote-port))
	 :while (equal tmp "-empty-") 
	 :do (progn 
	       (setq cmd-buffer "")
	       (sleep remote-cmd-buffer-polling-freq)))))


;; This is just to pretty up the output.
(defun fix-sentence-spacing (s)
  "This function operates on entire sentences.  It removes double
  spaces and attempts to fix punctuation and quotation mark spacing.
  Tries to capitalize 'I', too."
  (let ((out '())
	(quotedp nil)
	(new-sentence t)
	(final-punct  '(#\. #\? #\!))
	(maxindex (- (length s) 1)))
    (loop
       for c from 0 to maxindex
       do (let ((previous-char (char s (max 0 (- c 1))))
		(current-char (char s c))
		(next-char (char s (min (+ c 1) maxindex))))
	    ;; eliminate inappropriate spaces next to quotation marks
	    (if (char= current-char #\") 
		(setq quotedp (not quotedp)))
	    (cond
	      ;; eliminate double spaces and spaces before final punctuation.
	      ((char= current-char #\ )
	       (if (and (not (char= previous-char #\ ))
			(not (member next-char final-punct))
			(not (and quotedp
				  (char= previous-char #\")))
			(not (and quotedp
				  (char= next-char #\"))))
		   (setq out (cons current-char out))))
	      ((char= current-char #\i)
	       (if (and (or (= c 0)
			    (char= previous-char #\ ))
			(or (char= next-char #\ )
			    (char= next-char #\')))
		   (setq out (cons #\I out))
		   (setq out (cons current-char out)))
	       (setq new-sentence nil))
	      (t
	       (setq out (cons  (if new-sentence 
				    (progn 
				      (setq new-sentence nil)
				      (char-upcase current-char)) 
				    current-char) out))))
	    (if (member current-char final-punct)
		(setq new-sentence t))))
    (coerce (reverse out) 'string)))

(defun serialize (s)
  "Converts strings to a web-appropriate serialization, converting
  spaces to '%20' and so forth."
  (let ((out '()))
    (loop 
       for c across s
       do (cond
	    ((char= c #\ )
	     (setq out (cons #\+ out)))
	    ((char= c #\?)
	     (setq out (append '(#\f #\3 #\%) out)))
	    ((char= c #\&)
	     (setq out (append '(#\6 #\2 #\%) out)))
	    ((char= c #\')
	     (setq out (append '(#\7 #\2 #\%) out)))
	    ((char= c #\")
	     (setq out (append '(#\2 #\2 #\%) out)))
	    ((char= c #\=)
	     (setq out (append '(#\d #\3 #\%) out)))
	    ((char= c #\+)
	     (setq out (append '(#\b #\2 #\%) out)))
	    ((char= c #\%)
	     (setq out (append '(#\5 #\2 #\%) out)))
	    ((char= c #\()
	     (setq out (append '(#\8 #\2 #\%) out)))
	    ((char= c #\))
	     (setq out (append '(#\9 #\2 #\%) out)))
	    (t
	     (setq out (cons c out)))))
    (coerce (reverse out) 'string)))

       
(defun show-response () 
  "Show a response from SHRDLU, remotely if that is appropriate,
  locally otherwise, according to the remote-cmds-p flag."
  (let ((resp (fix-sentence-spacing response-buffer)))
    (if (and remote-cmds-p (< 0 (length resp)))
	(progn
	  (socket-cmd remote-host
		      (format nil "/?res=~A" 
			      (serialize resp))
		      remote-port)
	  (terpri)
	  (princ "sending...")
	  (princ (serialize resp)))
	(progn
	  (terpri)
	  (princ resp))))
  (setq response-buffer ""))

(DEFUN PRINT2 (X) 
  "A replacement for SHDRLU's original PRINT2, meant to play nicely
  with the remote response mechanism, if necessary."
  (COND ((> CHRCT (FLATSIZE X)) (PRINC '\ ))
	(T (TERPRI)))
  (setq response-buffer
	(concatenate 'string 
		     response-buffer 
		     (if (or (stringp X) (symbolp X))
			 (format nil " ~A" (string-downcase X))
			 (format nil " ~S" X)))))

(DEFUN PRINT3 (X) 
  "A replacement for SHDRLU's original PRINT3, meant to play nicely
  with the remote response mechanism, if necessary.  So far as I can
  tell, the primary difference between PRINT2 and PRINT3 is the
  placement of spaces, which isn't that important because of the
  sentence-fixing done in fix-sentence-spacing."
  (PROG2 (OR (> CHRCT (FLATSIZE X)) (TERPRI))
      (setq response-buffer 
	    (concatenate 'string 
			 response-buffer 
			 (if (or (stringp X) (symbolp X))
			     (format nil "~A " (string-downcase X))
			     (format nil "~S " X))))))

(DEFUN PRINT4 (X)  
  "This is for PRINT3 instances that don't need to be
  remote-capable (because they are part of the debug mechanism).
  Admittedly a lame name, but there are limits to cleverness."
  (PROG2 (OR (> CHRCT (FLATSIZE X)) (TERPRI))
      (PRINC X)
    (PRINC '\ ))) 

