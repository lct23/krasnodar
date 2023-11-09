(uiop:define-package #:app/pages/dashboard/hr
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/tables/table
                #:column
                #:make-table)
  (:import-from #:app/pages/utils
                #:title))
(in-package #:app/pages/dashboard/hr)


(defwidget hr-dashboard-widget ()
  ())


(defun make-hr-dashboard-widget ()
  (make-instance 'hr-dashboard-widget))


(defmethod render ((widget hr-dashboard-widget))
  (title "Дашборд HR")

  (with-html
    ;; Статистика по кадрам
    (:div :class "flex size-xl"
          (:div :class "mr-4"
                "Сотрудников: 150")
          (:div :class "mr-4"
                "Отделов: 5")
          (:div :class "mr-4"
                "Менторов: 6"))
    ;; Тут будут графики
    (:div :class "flex"
          (:img :src "https://placekitten.com/400/300")
          (:img :src "https://placekitten.com/400/300")
          (:img :src "https://placekitten.com/400/300"))

    ;; А это статистика по прохождению онбординга
    (render (make-table (list (column "Этап")
                              (column "1 день")
                              (column "1 неделя")
                              (column "1 месяц")
                              (column "3 месяца")
                              (column "6 месяцев")
                              (column "12 месяцев")
                              (column "18 месяцев")
                              (column "24 месяца"))
                        (list (mapcar #'princ-to-string
                                      (list "Участников" 2 1 6 11 23 6 30 44))
                              (mapcar #'princ-to-string
                                      (list "Успех" "44%" "56%" "11%" "67%" "95%" "97%" "98%" "98%"))
                              (mapcar #'princ-to-string
                                      (list "Провалы" 1 0 0 0 0 0 2 3))
                              (mapcar #'princ-to-string
                                      (list "Задержки" 1 0 2 5 0 0 2 3)))))))

