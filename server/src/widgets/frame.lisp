(uiop:define-package #:app/widgets/frame
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks-ui
                #:ui-widget)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/vars
                #:yandex-metrika-code
                #:*text-color*
                #:*dark-background*)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/models/roles
                #:hr-p)
  (:import-from #:reblocks-auth/models
                #:get-current-user)
  (:import-from #:str
                #:starts-with-p)
  (:export #:make-page-frame))
(in-package #:app/widgets/frame)


(defwidget frame-widget ()
  ((content :initarg :content
            :reader content)
   (wide :initarg :wide
         :initform nil
         :reader widep)))


(defun make-page-frame (content &key wide)
  (make-instance 'frame-widget
                 :content content
                 :wide wide))


(defun make-yandex-metrika-code ()
  (let ((code (yandex-metrika-code)))
    (cond
      (code
       (fmt "
<!-- Yandex.Metrika counter -->
<script type=\"text/javascript\" >
   (function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
   m[i].l=1*new Date();
   for (var j = 0; j < document.scripts.length; j++) {if (document.scripts[j].src === r) { return; }}
   k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
   (window, document, \"script\", \"https://mc.yandex.ru/metrika/tag.js\", \"ym\");

   ym(~A, \"init\", {
        clickmap:true,
        trackLinks:true,
        accurateTrackBounce:true,
        webvisor:true
   });
</script>
<noscript><div><img src=\"https://mc.yandex.ru/watch/~A\" style=\"position:absolute; left:-9999px;\" alt=\"\" /></div></noscript>
<!-- /Yandex.Metrika counter -->
"
                     code
                     code))
      ;; When no counter in settings
      (t
       ""))))


(defmethod render ((widget frame-widget))
  (with-html
    (:raw (make-yandex-metrika-code))
    (:header)

    ;; sidebar
    (render-sidebar)
    
    (:div :class (if (widep widget)
                     "w-full px-8"
                     "w-full px-8 py-4"
                     )
          (render (content widget)))))


(defmethod reblocks/widget:get-css-classes ((widget frame-widget))
  (list "flex"))


(defmethod get-dependencies ((widget frame-widget)))


(defun render-sidebar ()
  (with-html
    (flet ((item (path title)
             (let* ((current (or (string-equal path (reblocks/request:get-path))
                                 (starts-with-p (concatenate 'string path "/")
                                                (reblocks/request:get-path))))
                    (active-item-class
                      "pl-4 p-1 mr-4 bg-blue-500 text-white rounded-e-full hover:bg-blue-600 hover:shadow-xl hover:scale-105 focus:outline-none focus:shadow-outline hover:ring-2 hover:ring-blue-600")
                    (inactive-item-class
                      "pl-4 p-1 mr-4 bg-white text-black rounded-e-full hover:bg-white hover:text-blue-400 hover:shadow-xl hover:scale-105 focus:outline-none focus:shadow-outline hover:ring-2 hover:ring-white")
                    (class (if current
                               active-item-class
                               inactive-item-class)))
               (:li :class class
                    (:a :class "w-full"
                        :href path
                        title))))
           (title (text)
             (:h1 :class "font-bold mt-4 mb-2 ml-2 mr-2"
                  text)))
      (let ((user (get-current-user)))
        (:div :class
              "w-30 border-r-0 border-blue-400 shadow-xl whitespace-nowrap min-h-screen"

              (:div :class "border-b-2 p-4"
                    (:a :href "/"
                        :class "text-4xl font-bold text-blue-700 hover:text-blue-500 transition-colors duration-500 animate-pulse"
                        (:span :class "bg-gradient-to-r from-blue-500 to-blue-800 text-transparent bg-clip-text"
                               "HR Zero")))

              (:div :class "py-4"
                    (cond
                      (user
                       (:ul
                        (item "/" "Дашборд")
                        (item "/kb" "База знаний"))
                   
                       ;; (title "Мои задачи")
                       ;; (:ul
                       ;;  (item "/calendar" "Календарь")
                       ;;  (item "/progress" "Мой прогресс"))

                       (when (hr-p user)
                         (title "HR")
                         (:ul
                          (item "/departments" "Отделы")
                          (item "/personal" "Сотрудники")
                          (item "/boards" "Онбординги")))

                       (title "Временные странички")
                   
                       (:ul
                        (item "/playground" "Песочница")
                        (item "/switch" "Переключить учётку")))
                      (t
                       (:ul
                        (item "/" "Лэндинг")
                        ;; TODO: убрать для прода
                        (item "/switch" "Переключить учётку"))))))))))
