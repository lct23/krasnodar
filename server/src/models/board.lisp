(uiop:define-package #:app/models/board
  (:use #:cl)
  (:import-from #:mito
                #:object-id
                #:find-dao
                #:create-dao
                #:select-dao)
  (:import-from #:app/models/department
                #:department)
  (:import-from #:sxql
                #:where
                #:order-by)
  (:import-from #:app/models/knowledge
                #:knowledge))
(in-package #:app/models/board)


(defclass board ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :accessor board-title)
   (department :col-type (or :null department)
               :initform nil
               :initarg :department
               :accessor board-department))
  (:metaclass mito:dao-table-class))


(defclass board-period ()
  ((board :col-type board
          :initarg :board
          :reader board)
   (title :col-type :text
          :initform ""
          :initarg :title
          :reader period-title)
   (from-day :col-type :integer
             :initform ""
             :initarg :from-day
             :reader period-from-day
             :documentation "День, начиная с которого стартует период. 0 - первый день сотрудника на работе.")
   (to-day :col-type :integer
           :initform ""
           :initarg :to-day
           :reader period-to-day
           :documentation "День, до которого длится период, включительно. К примеру, у первой недели будет from-day 0 и to-day 6."))
  (:documentation "Этап онбординга выражающийся в днях с начала прохождения. К этапам привязаны отдельные задания.")
  (:metaclass mito:dao-table-class))


(defclass period-knowledge ()
  ((period :col-type board-period
          :initarg :period
           :reader period)
   (knowledge :col-type knowledge
              :initarg :knowledge
              :reader knowledge))
  (:documentation "Связь между знанием и периодом онбординга")
  (:metaclass mito:dao-table-class))


(defparameter *default-periods*
  (list (list "1 день"                0        0)
        (list "1 неделя"              1        6)
        (list "1 месяц"               6       30)
        (list "3 месяца"             31     (* 3 30))
        (list "6 месяцев"  (1+  (* 3 30))   (* 6 30))
        (list "12 месяцев" (1+  (* 6 30))  (* 12 30))
        (list "18 месяцев" (1+ (* 12 30))  (* 18 30))
        (list "24 месяцев" (1+ (* 18 30))  (* 24 30))))


(defun create-board ()
  (let ((board (create-dao 'board
                           :title "")))
    (loop for (title from to) in *default-periods*
          do (create-dao 'board-period
                         :board board
                         :title title
                         :from-day from
                         :to-day to))
    (values board)))


(defun get-boards ()
  (select-dao 'board
    (order-by :title)))

(defun get-board (id)
  (find-dao 'board :id id))


(defun board-periods (board)
  (select-dao 'board-period
    (where (:= :board_id (object-id board)))
    (order-by :from-day)))


(defun period-knowledges (period)
  (select-dao 'period-knowledge
    (where (:= :period_id (object-id period)))
    (order-by :id)))


(defun bind-knowledge-to-period (period knowledge)
  (check-type period board-period)
  (check-type knowledge knowledge)
  (mito:create-dao 'period-knowledge
                   :period period
                   :knowledge knowledge))


(defun remove-knowledge-from-period (period-knowledge)
  (check-type period-knowledge period-knowledge)
  (mito:delete-dao period-knowledge))

