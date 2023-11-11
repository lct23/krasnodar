(uiop:define-package #:app/games/registry
  (:use #:cl)
  (:import-from #:app/models/game
                #:game)
  (:import-from #:serapeum
                #:dict))
(in-package #:app/games/registry)


(defparameter *registry*
  (dict "learn-names" 'app/games/guess-name::make-guess-name-widget))

(defparameter *default-game*
  'app/games/guess-name::make-guess-name-widget)


(defun make-game-widget (game)
  (check-type game game)
  (funcall
   (gethash (app/models/game::game-widget-name game)
            *registry*
            *default-game*)))
