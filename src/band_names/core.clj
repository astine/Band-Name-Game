(ns band-names.core
  (:refer-clojure :exclude [rand-int rand-nth rand])
  (:use [band-names.syllables :exclude [slurp-lines]]
        [clojure.string :exclude [reverse replace]]
        [clojure.math.numeric-tower]
        [roul.random]
        [inflections.core :exclude [capitalize]]))

(defn slurp-lines [file]
  (split-lines (slurp file)))

(def band-names (sort (distinct (rest (slurp-lines "resources/band_names.txt")))))
(def band-names-set (set band-names))

(def nouns (slurp-lines "resources/nouns/91K nouns.txt"))

(def adjectives (slurp-lines "resources/adjectives/28K adjectives.txt"))

(def verbs (slurp-lines "resources/verbs/31K verbs.txt"))

(def adverbs (slurp-lines "resources/adverbs/6K adverbs.txt"))

(def prepositions (slurp-lines "resources/prepositions/prepositions.txt"))

(def male-names (slurp-lines "resources/names/male-first-names.txt"))
(def female-names (slurp-lines "resources/names/female-first-names.txt"))
(def last-names (slurp-lines "resources/names/last-names.txt"))

(def rap-first-names (slurp-lines "resources/rap-names/first-names.txt"))
(def rap-middle-names (slurp-lines "resources/rap-names/middle-names.txt"))
(def rap-last-names (slurp-lines "resources/rap-names/last-names.txt"))

(defmacro rand-do [& forms]
  (let [forms (partition 2 forms)
        weights (map first forms)
        forms (map second forms)]
    `(case (rand-nth-weighted ~(vec (map vector (iterate inc 0) weights)))
       ~@(mapcat list (iterate inc 0) forms))))

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
        1 (capitalize (clojure.pprint/cl-format false "~R" (inc (rand-int 99))))
        1 (inc (rand-int 99)))))

(defn noun-preposition-noun-band-name []
  (str (capitalize (rand-nth nouns))
       ", "
       (rand-nth prepositions)
       " "
       (capitalize (rand-nth nouns))))

(defn musician-name []
  (str (capitalize (rand-do
                    1 (rand-nth male-names)
                    1 (rand-nth female-names)))
       " "
       (capitalize (rand-nth last-names))))

(defn rapper-name []
  (join " "
        (map capitalize
             (remove nil?
                     [(rand-do
                       1 (rand-nth rap-first-names)
                       1 nil)
                      (rand-do
                       1 (rand-nth rap-middle-names)
                       1 nil)
                      (rand-nth rap-last-names)]))))

(def misspellings [[#"(ck|c)" "k"]
                   ["in" "n"]
                   ["in" "'n"]
                   ["own" "one"]
                   ["one" "own"]
                   ["ea" "ee"]
                   ["ti" "sh"]
                   ["s" "z"]
                   ["y " "i "]
                   [#"y$" "i"]
                   ["i" "y"]
                   ["r" "rr"]
                   ["g" "gg"]
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
                   ["x" "xxx"]
                   ["you" "u"]
                   ["your" "yur"]
                   ["wh" "w"]
                   ["what" "wut"]])

(defn misspell [phrase] ;TODO make these case independent
  (reduce #(apply clojure.string/replace (cons %1 %2))
          phrase
          (distinct (repeatedly (inc (rand-int (dec (count misspellings))))
                                #(rand-nth misspellings)))))

(defn band-name-exists? [band-name]
  (boolean (band-names-set band-name)))

(defn band-name-available? [band-name]
  (if (not (band-name-exists? band-name))
    band-name
    false))

(defn generate-band-name []
  (or (band-name-available?
       (rand-do 3 (adjective-noun-band-name)
                1 (misspell (adjective-noun-band-name))
                2 (noun-preposition-noun-band-name)
                2 (verb-noun-band-name)
                3 (musician-name)
                1 (rapper-name)
                1 (str "The " (plural (verb-noun-band-name)))
                1 (str "The " (plural (adjective-noun-band-name)))
                1 (str "The " (plural (noun-preposition-noun-band-name)))
                1 (noun-number-band-name)))
      (generate-band-name)))


(defn quiz-name []
  (let [real-name (rand-nth band-names)
        false-name (generate-band-name)]
    (print (str "Which band is real? \n"))
    (doseq [[index name]
            (map list [1 2] (shuffle (list real-name false-name)))]
      (print (str index " " name "\n")))))

