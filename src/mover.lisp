; Functions to report the movement of the hand and blocks.



(DEFUN MOVETO (X Y Z)
	(PRINC "~MOVING HAND TO ")
	(PRINC (LIST X Y Z))
	(PRINC "~")
	(TERPRI)
	(if remote-actions-p
	    (remote-action (format nil "MOVE/~D/~D/~D" X Y Z))))

(DEFUN GRASP (X)
	(PRINC "~GRASPING BLOCK ")
	(PRINC X)
	(PRINC "~")
	(TERPRI)
	;; Note that the ~A strips off the leading ':'
	(if remote-actions-p
	    (remote-action (format nil "GRASP/~A" X))))

(DEFUN UNGRASP NIL
	(PRINC "~LETTING GO~")
	(TERPRI)
	(if remote-actions-p
	    (remote-action "RELEASE")))

(DEFUN BLINK (X)
	(PRINC "~CHOOSING BLOCK ")
	(PRINC X)
	(PRINC "~")
	(TERPRI)
	(if remote-actions-p
	    (remote-action (format nil "BLINK/~A" X))))

(defun CREATE (name type color dimx dimy dimz posx posy posz)
  (let ((create-string (format nil "CREATE/~A/~A/~A/~D/~D/~D/~D/~D/~D"
			       name type color
			       dimx dimy dimz
			       posx posy posz)))
    (princ create-string)
    (terpri)
    (if remote-actions-p
	(remote-action create-string))))

;;(defun create-display (record))