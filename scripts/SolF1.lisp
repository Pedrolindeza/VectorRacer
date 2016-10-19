
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
	 (not (nth (second pos) 
         (nth (first pos) 
             (track-env track))))
)

(defun isGoalp (st) 
	(equal (find  (state-pos st) (track-endpositions (state-track st)) :test #'equal ) (state-pos st) ))

(defun nextState (st act)
	(setf
		(state-action st) act) st)
;	      :VEL '(1 3)
 ; (make-STATE :POS '(3 16)
;	      :ACTION act
;	      :COST -100))

