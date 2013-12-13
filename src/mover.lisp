; Functions to report the movement of the hand and blocks.

(DEFUN socket-cmd-debug (host page &OPTIONAL (port 1337))
  ;; HTTP requires the :DOS line terminator
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
     (FORMAT socket "GET ~A HTTP/1.0~2%" page)
     ;; dump the whole thing - header+data
     (LOOP :for line = (READ-LINE socket nil nil) :while line
	:do (princ line)))
  (terpri))

(DEFUN socket-cmd (host page &OPTIONAL (port 1337))
  ;; HTTP requires the :DOS line terminator
  (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
     (FORMAT socket "GET ~A HTTP/1.0~2%" page)
     ;; dump the whole thing - header+data
     (LOOP :for line = (READ-LINE socket nil nil) :while line)))

(defun shrdlu-move (cmd)
  (socket-cmd "localhost" (format nil "/?act=~A" cmd) 1337))


(DEFUN MOVETO (X Y Z)
	(PRINC "~MOVING HAND TO ")
	(PRINC (LIST X Y Z))
	(PRINC "~")
	(TERPRI)
	(shrdlu-move (format nil "MOVE/~D/~D/~D" X Y Z)))

(DEFUN GRASP (X)
	(PRINC "~GRASPING BLOCK ")
	(PRINC X)
	(PRINC "~")
	(TERPRI)
	;; Note that the ~A strips off the leading ':'
	(shrdlu-move (format nil "GRASP/~A" X)))

(DEFUN UNGRASP NIL
	(PRINC "~LETTING GO~")
	(TERPRI)
	(shrdlu-move "RELEASE"))

(DEFUN BLINK (X)
	(PRINC "~CHOOSING BLOCK ")
	(PRINC X)
	(PRINC "~")
	(TERPRI)
	(shrdlu-move (format nil "BLINK/~A" X)))

(defun CREATE (name type color dimx dimy dimz posx posy posz)
  (let ((create-string (format nil "CREATE/~A/~A/~A/~D/~D/~D/~D/~D/~D"
			       name type color
			       dimx dimy dimz
			       posx posy posz)))
    (princ create-string)
    (terpri)
    (shrdlu-move create-string)))

;;(defun create-display (record))