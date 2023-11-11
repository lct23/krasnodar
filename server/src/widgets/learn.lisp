(uiop:define-package #:app/widgets/learn
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/board
                #:period-knowledge)
  (:import-from #:app/widgets/document
                #:make-document-widget)
  (:import-from #:app/models/knowledge
                #:knowledge-game
                #:knowledge-document)
  (:import-from #:mito
                #:save-dao
                #:object-id)
  (:import-from #:app/models/board-progress
                #:game-score
                #:game-finished-at
                #:game-results
                #:questionnaire-results
                #:period-knowledge-progress)
  (:import-from #:app/widgets/questionnaire
                #:make-questionnaire-widget)
  (:import-from #:app/games/guess-name
                #:make-guess-name-widget)
  (:import-from #:app/games/registry
                #:make-game-widget)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:app/models/game
                #:game-title)
  (:import-from #:local-time
                #:now)
  (:import-from #:reblocks/response
                #:redirect))
(in-package #:app/widgets/learn)


(defwidget learn-widget ()
  ((period-knowledge-progress :initarg :period-knowledge-progress
                              :type period-knowledge-progress
                              :reader period-knowledge-progress)))


(defun make-learn-widget (period-knowledge-progress)
  (make-instance 'learn-widget
                 :period-knowledge-progress period-knowledge-progress))


(defmethod render ((widget learn-widget))
  (with-html
    (let* ((progress (period-knowledge-progress widget))
           (period-knowledge (period-knowledge progress))
           (questionnaire-results (questionnaire-results progress))
           (game-results (game-results progress))
           (knowledge (app/models/board::knowledge period-knowledge))
           (document (knowledge-document knowledge))
           (game (knowledge-game knowledge)))

      (cond
        (game
         (:h1 :class "mt-8 mb-4 font-bold text-xl text-center"
              (fmt "Игра \"~A\""
                   (game-title game)))
         (let ((game-widget (make-game-widget game)))
           ;; Обработчик завершения игры
           (event-emitter:on :continue game-widget
                             (lambda (game-score)
                               (log:info "Сотрудник прошёл игру, сохраняем прогресс.")
                               (when game-results
                                 (setf (game-finished-at game-results)
                                       (now))
                                 (setf (game-score game-results)
                                       game-score)
                                 (save-dao game-results)
                                 (redirect "/"))))
           (render game-widget)))
        (t
         (render (make-document-widget (object-id document)))

         (:h1 :class "mt-8 mb-4 font-bold text-xl"
              "Проверка знаний")

         (render (make-questionnaire-widget questionnaire-results)))))))

