(ns cljswidget.core)

(set! *print-meta* true)

(defprotocol WidgetServer
  (render [this])
  (script-fragment [this]))

(defprotocol WidgetClient
  (init [this el]))


(defn add-self-param
  "Adds a self parameter to a method implementation."
  [[fn-name params & body]]
  `(~fn-name [~'_ ~@params] ~@body))

(defn gen-script-fragment
  [client-fn-id widget-params]
  (str
   "<script type=\"text/javascript\">"
   client-fn-id "("
   "\"" (clojure.string/join ",\"" widget-params) "\""
   ");"
   "</script>"))

(defn gen-server-impl
  [name params methods client-fn-id] 
  (let [corrected-methods (map add-self-param methods)]
    `(defn ~name
       ~params
       (reify WidgetServer
              (script-fragment [_]
                 (gen-script-fragment ~(str client-fn-id) ~params))
              ~@corrected-methods))))

(defn gen-client-impl
  [name params methods client-fn-id]
  (let [corrected-methods (map add-self-param methods)]
    `(defn
       ^:export name
       ~params
       (reify WidgetClient
              ~@corrected-methods))))

(defn emit!
  [file s]
  (spit file s :append true))

(defmacro defwidget
  [name params
   [_ & server-methods]
   [_ & client-methods]]
  (let [client-fn-id (gensym "_")]
    (->> (gen-client-impl name params client-methods client-fn-id)
         str
         (spit "widgets.cljs"))
    (gen-server-impl name params server-methods client-fn-id)))

(defmacro insert-client-code
  []
  (slurp "widgets.cljs"))

(defwidget button
   [title] 
   (server     
       (render []
           (str "<button>" title "</button>")))
   (client
       (init [el]
           (.button js/jQuery ...)
           (dommy/listen! :click ...))))