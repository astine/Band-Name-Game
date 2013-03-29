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
            [compojure.route :as route])
  (:import (javax.crypto Cipher KeyGenerator SecretKey)
           (javax.crypto.spec SecretKeySpec)
           (java.security SecureRandom)
           (java.net URLEncoder)
           (org.apache.commons.codec.binary Base64)))

(def index (html-resource "www/index.html"))
(def game (html-resource "www/game.html"))
(def results (html-resource "www/score.html"))

(defn bytes [string]
  (.getBytes string "UTF-8"))

(defn base64 [binary]
  (Base64/encodeBase64String binary))

(defn debase64 [string]
  (Base64/decodeBase64 string))

(defn get-raw-key [seed]
  (let [keygen (KeyGenerator/getInstance "AES")
        sr (SecureRandom/getInstance "SHA1PRNG")]
    (.setSeed sr (bytes seed))
    (.init keygen 128 sr)
    (.. keygen generateKey getEncoded)))

(defn get-cipher [mode seed]
  (let [key-spec (SecretKeySpec. (get-raw-key seed) "AES")
        cipher (Cipher/getInstance "AES")]
    (.init cipher mode key-spec)
    cipher))

(defn encrypt [text key]
  (let [bytes (bytes text)
        cipher (get-cipher Cipher/ENCRYPT_MODE key)]
    (base64 (.doFinal cipher bytes))))

(defn decrypt [text key]
  (let [cipher (get-cipher Cipher/DECRYPT_MODE key)]
    (String. (.doFinal cipher (debase64 text)))))

(defn serialize [value]
  (encrypt (str value) "asdf"))

(defn deserialize [value]
  (read-string (decrypt value "asdf")))

(def number-of-questions 10)

(defn score-template [{count-right :count-right count :count}]
  (let [score (ceil (* 100 (/ count-right count)))]
    (emit* (at results [:#results]
               (content (list {:tag :p :content (cond (< 90 score) ["Congratulations!"]
                                                      (<= 50 score 90) ["Good job!"]
                                                      :else [""])}
                              {:tag :p :content `("Score:" {:tag :em :content [~score]} "%")}))))))

(defn band-name-template [count-right count-wrong count correct? band-names]
  (emit* (at game [:.band]
             (clone-for [[color [band validity]] band-names]
                        (do-> (add-class color)
                              (content {:tag :p :content band})
                              (set-attr :href (str "/answer?choice=" color
                                                   "&count=" count))))
             [:#counter]
             (content (list (when (> count 1)
                              (if correct?
                                (rand-nth ["Good job!" "Correct!" "Right!" "You got it!"])
                                (rand-nth ["Nope!" "Not right." "Try again!" "Not that time."])))
                            {:tag :br}
                            {:tag :br}
                            (case (- number-of-questions count)
                              0 "One More!"
                              (1 2 3) (rand-nth ["Just a few more!" "Almost done!" "Really close!"])
                              (4 5) (rand-nth ["Another question!" "Having fun?" "You, are SO awesome!" "This is going really well!"])
                              9 "Let's Start"
                              ""))))))

(defn band-name-create [{session :session}]
  (if (and session (:count session) (> (:count session) number-of-questions))
    (redirect (str "/score?profile=" (URLEncoder/encode (serialize (dissoc session :band-names)))))
    (let [band-names (zipmap ["negative" "positive"]
                             (shuffle [[(generate-band-name) false]
                                       [(select-band-name) true]]))]
      (-> (response (band-name-template (or (:count-right session) 0)
                                        (or (:count-wrong session) 0)
                                        (inc (or (:count session) 0))
                                        (:correct? session)
                                        band-names))
          (assoc :session (assoc session :band-names band-names))))))

(defn band-name-pick [choice count {session :session}]
  (if (> (read-string count) (or (:count session) 0))
    (let [correct? (second ((:band-names session) choice))]
      (-> (redirect "/game")
          (assoc :session (assoc session
                            :correct? correct?
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
                                  (:correct? session)
                                  (:band-names session)))))

(defroutes app-routes
  (GET "/" [] (-> (response (emit* (at index [:#start-link] 
                                       (content {:tag :p :content "Start Game"}))))
                  (assoc :session {})))
  (GET "/game" [:as req] (band-name-create req))
  (GET "/answer" [choice count :as req] (band-name-pick choice count req))
  (GET "/restart" [:as req] (band-name-create (assoc req :session {})))
  (GET "/score" [profile] (score-template (deserialize profile)))
  (route/resources "/"))

(def app
  (wrap-session 
   (handler/site app-routes)))
