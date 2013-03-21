(ns band-names.server
  (:refer-clojure)
  (:use [band-names.core :only [select-band-name generate-band-name]]
        [clojure.string :exclude [reverse replace]]
        [clojure.math.numeric-tower]
        [net.cgrand.enlive-html]
        [compojure.core])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(def index (html-resource "www/index.html"))

(defn band-name-template []
  (let [band-names (zipmap ["negative" "positive"]
                           (shuffle [[(generate-band-name) :false]
                                     [(select-band-name) :real]]))]
    (emit* (transform index [:.band]
                      (clone-for [[color [band validity]] band-names]
                                 (do-> (add-class color)
                                       (content {:tag :p :attr {} :content band})))))))

(defroutes app-routes
  (GET "/" [] (band-name-template))
  (route/resources "/"))

(def app
  (handler/site app-routes))
