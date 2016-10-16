
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
	(cond ((nth (cdr pos) (nth (car pos) track-env )) (nil))
		(t (t))
	)
  "check if there is an obstacle at position pos of the track"
  t)

(defun isGoalp (st) 
	(cond ( (equal (nth (cdr st-pos) (nth (car st-pos ) st-track )) 'E  ) (t))
		(t (nil))
	)
  "check if st is a goal state"
  t)

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (make-STATE :POS '(3 16)
	      :VEL '(1 3)
	      :ACTION act
	      :COST -100))

