(ns band-names.core
  (:use [clojure.string :exclude [reverse replace]]
        [clojure.math.numeric-tower]
        [inflections.core :exclude [capitalize]]))

(defn slurp-lines [file]
  (split-lines (slurp file)))

(def band-names (sort (distinct (rest (slurp-lines "resources/band_names.txt")))))

(def nouns (slurp-lines "resources/nouns/91K nouns.txt"))

(def adjectives (slurp-lines "resources/adjectives/28K adjectives.txt"))

(def verbs (slurp-lines "resources/verbs/31K verbs.txt"))

(def adverbs (slurp-lines "resources/adverbs/6K adverbs.txt"))

(def prepositions (slurp-lines "resources/prepositions/prepositions.txt"))

(defmacro rand-do [& forms]
  `(case (rand-int ~(count forms))
     ~@(mapcat list (iterate inc 0) forms)))

(defn adjective-noun-band-name []
  (join " "
        (map capitalize
             (concat (repeatedly (rand-int 3) #(rand-nth adjectives))
                     [(rand-nth nouns)]))))

(defn verb-noun-band-name []
  (join " "
        (map capitalize
             [(rand-nth verbs)
              (rand-nth nouns)])))
 
(defn noun-number-band-name []
  (str (capitalize (rand-nth nouns))
       " "
       (rand-do
        (capitalize (clojure.pprint/cl-format false "~R" (inc (rand-int 99))))
        (inc (rand-int 99)))))

(defn noun-preposition-noun-band-name []
  (str (capitalize (rand-nth nouns))
       ", "
       (rand-nth prepositions)
       " "
       (capitalize (rand-nth nouns))))

(def misspellings [[#"(ck|c)" "k"]
                   ["s" "z"]
                   ["through" "thru"]
                   ["ew" "u"]
                   ["oo" "ew"]
                   ["le" "al"]
                   ["ea" "ee"]
                   ["qu" "kw"]
                   ["qu" "qw"]
                   ["ate" "8"]
                   ["great" "gr8"]
                   ["ks" "x"]
                   ["x" "ks"]
                   ["you" "u"]
                   ["your" "yur"]
                   ["wh" "w"]
                   ["what" "wut"]])

(defn misspell [phrase]
  (reduce #(apply clojure.string/replace (cons %1 %2))
          phrase
          (distinct (repeatedly (inc (rand-int (dec (count misspellings))))
                                #(rand-nth misspellings)))))

(defn generate-band-name []
  (rand-do (adjective-noun-band-name)
           (adjective-noun-band-name)
           (adjective-noun-band-name)
           (misspell (adjective-noun-band-name))
           (noun-preposition-noun-band-name)
           (noun-preposition-noun-band-name)
           (verb-noun-band-name)
           (verb-noun-band-name)
           (str "The " (plural (verb-noun-band-name)))
           (str "The " (plural (adjective-noun-band-name)))
           (str "The " (plural (noun-preposition-noun-band-name)))
           (noun-number-band-name)))


(defn quiz-name []
  (let [real-name (rand-nth band-names)
        false-name (generate-band-name)]
    (print (str "Which band is real? \n"))
    (doseq [[index name]
            (map list [1 2] (shuffle (list real-name false-name)))]
      (print (str index " " name "\n")))))
