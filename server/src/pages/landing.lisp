(uiop:define-package #:app/pages/landing
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui
                #:ui-widget)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:reblocks-ui2/tables/table
                #:append-data
                #:recalculate-cells
                #:current-row
                #:column
                #:make-table)
  (:import-from #:40ants-pg/utils
                #:all-objects-iterator)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:common/utils
                #:format-datetime-msk
                #:format-datetime)
  (:import-from #:reblocks/page
                #:page-metadata
                #:current-page)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  (:import-from #:reblocks-ui2/containers/tabs
                #:tabs-widget
                #:make-tabs-widget)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:reblocks-auth/models
                #:get-nickname
                #:anonymous-p)
  (:import-from #:app/widgets/new-document-form)
  (:import-from #:app/widgets/document-list)
  (:import-from #:app/widgets/document)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:app/widgets/user-list))
(in-package #:app/pages/landing)


(defwidget landing-page (websocket-widget)
  ((name :initform nil
         :accessor user-name)))


(defun make-landing-page ()
  (make-instance 'landing-page))


(defmethod render ((widget landing-page))
  (with-html
    (:div :class "landing-container fixed w-screen h-screen"
          (:div :class "logo-and-text flex flex-col gap-8"
                (:img :class "logo"
                      :src "https://storage.yandexcloud.net/hrzero-files/landing-logo-small.png")
                (:div :class "subheader1"
                      "Глубокая адаптация сотрудников")
                (:div :class "subheader2"
                      "Автоматический онбординг" (:br)
                      "с персонализированным графиком" (:br)
                      "обучения на 2 года"))

          (:div :class "login-button rounded-full bg-gradient-to-b from-[#3B82F6] to-[#00358C] text-white font-bold text-4xl px-20 py-8"
                (:a :href "/login"
                    "Войти")))))


(defmethod get-dependencies ((widget landing-page))
  (list*
   (reblocks-lass:make-dependency
     `(.landing-page
       :display flex
       :flex-direction column
       (.landing-container
        :background-image "url(https://storage.yandexcloud.net/hrzero-files/landing-bg-small.jpg)"
        :background-repeat no-repeat
        :background-size cover
        (.logo-and-text
         :position relative
         :left 133px
         :top 526px
         (.logo :max-width 600px)
         (.subheader1 :font-size 4rem
                      :font-weight bold
                      :color "#FF008A")
         (.subheader2 :font-size 2rem
                      :font-style italic
                      :color "#00358C"))
        (.login-button
         :position absolute
         :right 133px
         :top 100px))))
   (call-next-method)))
