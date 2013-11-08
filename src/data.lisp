;################################################################
;
;		DATA >
;
;   initial microplanner data base for the blocks world
;
;################################################################

(THFLUSH THASSERTION)

(SETQ ATABLE '((:B1 (110 100 0) (100 100 100))
	       (:B2 (110 100 100) (100 100 100))
	       (:B3 (400 0 0) (200 300 200))
	       (:B4 (640 640 1) (200 200 200))
	       (:B5 (500 100 200) (100 100 300))
	       (:B6 (0 300 0) (200 300 300))
	       (:B7 (0 240 300) (200 200 200))
	       (:B10 (300 640 0) (200 100 400))
	       (:BW1 (570 570 0) (10 400 300))
	       (:BW2 (570 570 0) (400 10 300))
	       (:BW3 (570 1200 0) (400 10 300))
	       (:BW4 (1200 570 0) (10 400 300))
	       (:BOX (600 600 0) (400 400 1))))

(SETQ DISPLAY-AS
'((:B1 \#DISPLAY \#BLOCK (110 100 0) (100 100 100) RED)
(:B2 \#DISPLAY \#PYRAMID (110 100 100 ) (100 100 100) GREEN)
(:B3 \#DISPLAY \#BLOCK (400 0 0) (200 200 200) GREEN)
(:B4 \#DISPLAY \#PYRAMID (640 640 1) (200 200 200) BLUE)
(:B5 \#DISPLAY \#PYRAMID (500 100 200) (100 100 300) RED)
(:B6 \#DISPLAY \#BLOCK (0 300 0) (200 300 300) RED)
(:B7 \#DISPLAY \#BLOCK (0 240 300) (200 250 200) GREEN)
(:B10 \#DISPLAY \#BLOCK (300 640 0) (200 100 400) BLUE)
(:HAND \#DISPLAY \#HAND (40 0 0) (0 0 0) WHITE)
(:TABLE \#DISPLAY \#TABLE (0 0 0) (1000 1000 0) BLACK)
(:BOX \#DISPLAY \#BOX (600 600 0) (376 376 300) WHITE)) )

(THADD '(\#IS :B1 \#BLOCK) NIL)

(THADD '(\#IS :B2 \#PYRAMID) NIL)

(THADD '(\#IS :B3 \#BLOCK) NIL)

(THADD '(\#IS :B4 \#PYRAMID) NIL)

(THADD '(\#IS :B5 \#PYRAMID) NIL)

(THADD '(\#IS :B6 \#BLOCK) NIL)

(THADD '(\#IS :B7 \#BLOCK) NIL)

(THADD '(\#IS :B10 \#BLOCK) NIL)

(THADD '(\#IS \#RED \#COLOR) NIL)

(THADD '(\#IS \#BLUE \#COLOR) NIL)

(THADD '(\#IS \#GREEN \#COLOR) NIL)

(THADD '(\#IS \#WHITE \#COLOR) NIL)

(THADD '(\#IS \#BLACK \#COLOR) NIL)

(THADD '(\#IS \#RECTANGULAR \#SHAPE) NIL)

(THADD '(\#IS \#ROUND \#SHAPE) NIL)

(THADD '(\#IS \#POINTED \#SHAPE) NIL)

(THADD '(\#IS :SHRDLU \#ROBOT) NIL)
(THADD '(\#IS :FRIEND \#PERSON) NIL)
(THADD '(\#IS :HAND \#HAND) NIL)
(THADD '(\#AT :B1 (100 100 0)) NIL)

(THADD '(\#AT :B2 (100 100 100)) NIL)

(THADD '(\#AT :B3 (400 0 0)) NIL)

(THADD '(\#AT :B4 (640 640 1)) NIL)
(THADD '(\#AT :B5 (500 100 200)) NIL)

(THADD '(\#AT :B6 (0 300 0)) NIL)

(THADD '(\#AT :B7 (0 240 300)) NIL)

(THADD '(\#AT :B10 (300 640 0)) NIL)

(THADD '(\#SUPPORT :B1 :B2) NIL)

(THADD '(\#SUPPORT :B3 :B5) NIL)

(THADD '(\#SUPPORT :B6 :B7) NIL)
(THADD '(\#CLEARTOP :B2) NIL)
(THADD '(\#CLEARTOP :B4) NIL)
(THADD '(\#CLEARTOP :B5) NIL)
(THADD '(\#CLEARTOP :B7) NIL)
(THADD '(\#CLEARTOP :B10) NIL)

(THADD '(\#MANIP :B1) NIL)

(THADD '(\#MANIP :B2) NIL)

(THADD '(\#MANIP :B3) NIL)

(THADD '(\#MANIP :B4) NIL)

(THADD '(\#MANIP :B5) NIL)

(THADD '(\#MANIP :B6) NIL)

(THADD '(\#MANIP :B7) NIL)

(THADD '(\#MANIP :B10) NIL)

(THADD '(\#SUPPORT :TABLE :B1) NIL)

(THADD '(\#SUPPORT :TABLE :B3) NIL)

(THADD '(\#SUPPORT :BOX :B4) NIL)

(THADD '(\#SUPPORT :TABLE :B10) NIL)

(THADD '(\#SUPPORT :TABLE :B6) NIL)

(THADD '(\#SUPPORT :TABLE :BOX) NIL)

(THADD '(\#AT :BOX (600 600 0)) NIL)

(THADD '(\#IS :BOX \#BOX) NIL)

(THADD '(\#IS :TABLE \#TABLE) NIL)

(THADD '(\#CONTAIN :BOX :B4) NIL)

(THADD '(\#SHAPE :B1 \#RECTANGULAR) NIL)

(THADD '(\#SHAPE :B3 \#RECTANGULAR) NIL)

(THADD '(\#SHAPE :B2 \#POINTED) NIL)

(THADD '(\#SHAPE :B4 \#POINTED) NIL)

(THADD '(\#SHAPE :B5 \#POINTED) NIL)
(THADD '(\#SHAPE :B6 \#RECTANGULAR) NIL)

(THADD '(\#SHAPE :B7 \#RECTANGULAR) NIL)

(THADD '(\#SHAPE :B10 \#RECTANGULAR) NIL)

(THADD '(\#COLOR :B1 \#RED) NIL)

(THADD '(\#COLOR :B2 \#GREEN) NIL)

(THADD '(\#COLOR :B3 \#GREEN) NIL)

(THADD '(\#COLOR :B4 \#BLUE) NIL)

(THADD '(\#COLOR :B5 \#RED) NIL)

(THADD '(\#COLOR :B6 \#RED) NIL)

(THADD '(\#COLOR :B7 \#GREEN) NIL)

(THADD '(\#COLOR :B10 \#BLUE) NIL)

(THADD '(\#COLOR :BOX \#WHITE) NIL)

(THADD '(\#COLOR :TABLE \#BLACK) NIL)

(THADD '(\#CALL :SHRDLU SHRDLU) NIL)
(THADD '(\#CALL :FRIEND YOU) NIL)

(SETQ HANDAT (SETQ HANDAT2 '(40 0 0)))

(SETQ THTIME 0)

(THFLUSH HISTORY)

(STARTHISTORY)

(SETQ PLAN NIL)

(MAPC (FUNCTION (LAMBDA (X Y) (SETF (GET X 'COLOR) (LIST Y))))
      '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10)
      '(CB1 CB2 CB3 CB4 CB5 CB6 CB7 CB10))

