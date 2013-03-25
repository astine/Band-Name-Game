(ns band-names.server
  (:refer-clojure)
  (:use [band-names.core :only [select-band-name generate-band-name]]
        [clojure.string :exclude [reverse replace]]
        [clojure.math.numeric-tower]
        [net.cgrand.enlive-html]
        [compojure.core]
        [ring.middleware.session]
        [ring.util.response])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(def index (html-resource "www/index.html"))
(def game (html-resource "www/game.html"))
(def results (html-resource "www/score.html"))

(defn score-template [count-right count]
  (let [score (ceil (* 100 (/ count-right count)))]
    (emit* (at results [:#results]
               (content (list {:tag :p :content (cond (< 90 score) ["Congratulations!"]
                                                      (<= 50 score 90) ["Good job!"]
                                                      :else [""])}
                              {:tag :p :content `("Score:" {:tag :em :content [~score]} "%")}))))))

(defn band-name-template [count-right count-wrong count band-names]
  (emit* (at game [:.band]
             (clone-for [[color [band validity]] band-names]
                        (do-> (add-class color)
                              (content {:tag :p :content band})
                              (set-attr :href (str "/answer?choice=" color
                                                   "&count=" count))))
             [:#counter]
             (content (list (str "Right: " count-right)
                            {:tag :br}
                            (str "Wrong: " count-wrong))))))

(defn band-name-create [{session :session}]
  (if (and session (:count session) (> (:count session) 10))
    (redirect (str "/score?count-right=" (:count-right session) 
                   "&count=" (:count session)))
    (let [band-names (zipmap ["negative" "positive"]
                             (shuffle [[(generate-band-name) false]
                                       [(select-band-name) true]]))]
      (-> (response (band-name-template (or (:count-right session) 0)
                                        (or (:count-wrong session) 0)
                                        (inc (or (:count session) 0))
                                        band-names))
          (assoc :session (assoc session :band-names band-names))))))

(defn band-name-pick [choice count {session :session}]
  (if (> (read-string count) (or (:count session) 0))
    (let [correct? (second ((:band-names session) choice))]
      (-> (redirect "/game")
          (assoc :session (assoc session
                            :count (read-string count)
                            :count-right (if correct? 
                                           (inc (or (:count-right session) 0)) 
                                           (or (:count-right session) 0))
                            :count-wrong (if (not correct?) 
                                           (inc (or (:count-wrong session) 0)) 
                                           (or (:count-wrong session) 0))))))
    (response (band-name-template (or (:count-right session) 0)
                                  (or (:count-wrong session) 0)
                                  (or (:count session) 0)
                                  (:band-names session)))))

(defroutes app-routes
  (GET "/" [] (-> (response (emit* (at index [:#start-link] 
                                       (content {:tag :p :content "Start Game"}))))
                  (assoc :session {})))
  (GET "/game" [:as req] (band-name-create req))
  (GET "/answer" [choice count :as req] (band-name-pick choice count req))
  (GET "/restart" [:as req] (band-name-create (assoc req :session {})))
  (GET "/score" [count-right count] (response (score-template (read-string count-right) 
                                                              (read-string count))))
  (route/resources "/"))

(def app
  (wrap-session 
   (handler/site app-routes)))
