
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
             (track-env track)
         )
        )
  )
)

(defun isGoalp (st) 
	  (equal 
	  	(find  
	  		(state-pos st) 
	  		(track-endpositions
	  			(state-track st)) :test #'equal 
	  	) 
	  	(state-pos st) 
	  )

)

(defun nextState (st act)
	(setq cost 1)
	(setq velnul (list 0 0))
	(setq  vel (list  (+ (first (state-vel st)) (first act) ) (+ (second (state-vel st)) (second act) ) )  )
	(setq pos (list (+ (first (state-pos st)) (first vel)) (+ (second (state-pos st)) (second vel))  ))
	(setq teststate (make-STATE :POS pos
	  		      				:VEL vel
	  		      				:ACTION act
	  		      				:COST cost
	  		      				:TRACK (state-track st)
	  		      				:OTHER nil))

	(cond ( (not (isObstaclep  pos (state-track st)))
			(cond ( (isGoalp teststate) 
					(setq cost -100)
				  )
			)
		  )
		  (t (setq cost 20 vel velnul) )
	)
		(setq teststate (make-STATE :POS pos
	  		      				:VEL vel
	  		      				:ACTION act
	  		      				:COST cost
	  		      				:TRACK (state-track st)
	  		      				:OTHER nil))


)
