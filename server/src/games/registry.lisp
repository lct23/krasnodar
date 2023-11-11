(uiop:define-package #:app/games/registry
  (:use #:cl)
  (:import-from #:app/models/game
                #:game-widget-name
                #:game)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:app/games/guess-name)
  (:import-from #:app/games/guess-position))
(in-package #:app/games/registry)


(defparameter *registry*
  (dict "learn-names" 'app/games/guess-name::make-guess-name-widget
        "guess-position" 'app/games/guess-position::make-guess-position-widget))

(defparameter *default-game*
  'app/games/guess-name::make-guess-name-widget)


(defun make-game-widget (game)
  (check-type game game)
  (funcall
   (gethash (game-widget-name game)
            *registry*
            *default-game*)))
