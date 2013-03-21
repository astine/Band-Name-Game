(ns band-names.core
  (:refer-clojure :exclude [rand-int rand-nth rand])
  (:use [band-names.syllables :exclude [slurp-lines]]
        [clojure.string :exclude [reverse replace]]
        [clojure.math.numeric-tower]
        [roul.random]
        [inflections.core :exclude [capitalize]]))

(defn slurp-lines [file]
  (split-lines (slurp file)))

(defn slurp-weighted-lines [file]
  (map #(let [[n s] (split % #"\s" 2)]
          [s (read-string n)])
       (slurp-lines file)))

(def band-names (sort (distinct (rest (slurp-lines "resources/band_names.txt")))))
(def band-names-set (set band-names))

(def nouns (slurp-weighted-lines "resources/nouns/nouns.txt"))

(def adjectives (slurp-weighted-lines "resources/adjectives/adjectives.txt"))

(def verbs (slurp-weighted-lines "resources/verbs/verbs.txt"))

(def adverbs (slurp-weighted-lines "resources/adverbs/adverbs.txt"))

(def prepositions (slurp-weighted-lines "resources/prepositions/prepositions.txt"))

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

(defn capitalize-words [words]
  (join " " (map capitalize (split words #"\s+"))))

(defn capitalized? [word]
  (boolean (re-seq #"^[A-Z]" word)))

(def syllable-count-distribution
  [[0 1]
   [1 3]
   [2 6]
   [3 15]
   [4 28]
   [5 10]
   [6 6]
   [7 3]
   [8 1]
   [9 0.5]
   [10 0.2]
   [11 0.2]
   [12 0.2]
   [13 0.15]
   [14 0.1]])


(defn merge-band-name [{:keys [base-word adjectives
                               verb adverbs
                               preposition article?
                               plural? band?]
                        :as band-name}]
  (join " " 
        (remove nil?
                (concat (if band?
                          ["The"]
                          (when article?
                            (if plural?
                              [(rand-do 2 "The" 1 "A")]
                              ["The"])))
                        (map capitalize adverbs)
                        [(when verb (capitalize verb))]
                        (map capitalize adjectives)
                        [(capitalize-words (if (and plural? (not preposition))
                                       (plural base-word)
                                       (singular base-word)))]
                        [(when preposition (plural preposition))]
                        (if band?
                          ["Band"])))))

(defn generate-preposition []
  (str (rand-nth-weighted prepositions)
       " "
       (capitalize (rand-nth-weighted nouns))))

(defn band-name-syllables [band-name]
  (count-syllables (merge-band-name band-name)))
                            
(defn basic-band-name []
  (let [syllables (rand-nth-weighted syllable-count-distribution)
        band-name {:base-word (rand-nth-weighted nouns)}]
    (loop [band-name band-name]
      (if (> syllables (band-name-syllables band-name))
        (rand-do 2 (recur (assoc band-name :adjectives 
                                 (cons (rand-nth-weighted adjectives)
                                       (:adjectives band-name))))
                 2 (if (:verb band-name)
                     (recur (assoc band-name :adverbs
                                   (cons (rand-nth-weighted adverbs)
                                         (:adverbs band-name))))
                     (recur (assoc band-name :verb (rand-nth-weighted verbs))))
                 2 (recur (assoc band-name :preposition (generate-preposition)))
                 2 (if (:article? band-name)
                     (recur band-name)
                     (recur (assoc band-name :article? true)))
                 2 (if (:plural? band-name)
                     (recur band-name)
                     (recur (assoc band-name :plural? true)))
                 1 (if (:band? band-name)
                     (recur band-name)
                     (recur (assoc band-name :band? true))))
        (merge-band-name band-name)))))
    

(defn musician-name []
  (str (capitalize (rand-do
                    1 (rand-nth male-names)
                    1 (rand-nth female-names)))
       " "
       (capitalize (rand-nth last-names))))

(defn musician-band-name []
  (let [syllables (rand-nth-weighted syllable-count-distribution)
        band-name {:base-word (musician-name)}]
    (loop [band-name band-name]
      (if (> syllables (band-name-syllables band-name))
        (rand-do 0.5 (recur (assoc band-name :adjectives 
                                 (cons (rand-nth-weighted adjectives)
                                       (:adjectives band-name))))
                 1 (if (:verb band-name)
                     (recur (assoc band-name :adverbs
                                   (cons (rand-nth-weighted adverbs)
                                         (:adverbs band-name))))
                     (recur (assoc band-name :verb (rand-nth-weighted verbs))))
                 2 (recur (assoc band-name :preposition (generate-preposition)))
                 5 (if (:band? band-name)
                     (recur band-name)
                     (recur (assoc band-name :band? true))))
        (merge-band-name band-name)))))

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

(def misspellings [[#"(ck|c|C|Ck)" "k"]
                   [#"[Ii]n" "n"]
                   [#"[Ii]n" "'n"]
                   [#"(.)own" "$1one"]
                   [#"(.)one" "$1own"]
                   [#"[Ee]a" "ee"]
                   [#"(.)[Tt]i" "$1sh"]
                   [#"^and$" "n"]
                   [#"[Ss]" "z"]
                   [#"[Yy] " "i "]
                   [#"[Yy]$" "i"]
                   [#"[Ii]" "y"]
                   [#"[Rr]" "rr"]
                   [#"(.)g" "$1gg"]
                   [#"[Tt]hrough" "thru"]
                   [#"[Ee]w" "u"]
                   [#"[Oo]o" "ew"]
                   [#"[Ll]e$" "al"]
                   [#"[Uu]e$" "u"]
                   [#"[Ee]a" "ee"]
                   [#"[Qq]u" "kw"]
                   [#"[Qq]u" "qw"]
                   [#"[Aa]te" "8"]
                   [#"[Gg]reat" "gr8"]
                   [#"[Kk]s" "x"]
                   [#"[Xx]" "ks"]
                   [#"[Xx]" "xxx"]
                   [#"[Yy]ou" "U"]
                   [#"[Yy]our" "Yur"]
                   [#"[Ww]h" "W"]
                   [#"[Ww]hat" "Wut"]])

(def leetz [[#"[Ee]" "3"]
            [#"[Aa]" "@"]
            [#"[Aa]" "4"]
            [#"[Oo]" "0"]
            [#"[Tt]" "7"]
            [#"[Gg]" "9"]
            [#"[Ii]" "1"]
            [#"[Hh]" "|-|"]
            [#"[Kk]" "|<"]
            [#"[Kk]" "|("]
            [#"[Yy]" "`/"]
            [#"[Vv]" "\\\\/"]
            [#"[Ww]" "\\\\/\\\\/"]
            [#"[Dd]" "c|"]
            [#"[Mm]" "/\\\\/\\\\"]
            [#"[Mm]" "|\\\\/|"]
            [#"[Nn]" "|\\\\|"]])

(defn misspell [phrase & [thesaurus intensity]] 
  (let [thesaurus (or thesaurus misspellings)
        intensity (or intensity (rand))]
    (join " "
          (for [word (split phrase #"\s+")]
            (let [new-word (reduce #(apply clojure.string/replace (cons %1 %2))
                                   (lower-case word)
                                   (take (* intensity (count thesaurus))
                                         (shuffle thesaurus)))]
              (if (capitalized? word)
                (capitalize new-word)
                (lower-case new-word)))))))
          
(defn band-name-exists? [band-name]
  (boolean (band-names-set band-name)))

(defn band-name-available? [band-name]
  (if (not (band-name-exists? band-name))
    band-name
    false))

(defn generate-band-name []
  (or (band-name-available?
       (rand-do 9 (basic-band-name)
                9 (misspell (basic-band-name) misspellings 0.2)
                1 (misspell (basic-band-name) misspellings 0.8)
                0.3 (misspell (basic-band-name) leetz 0.7)
                9 (musician-name)
                7 (musician-band-name)
                3 (rapper-name)))
      (generate-band-name)))

(defn select-band-name []
  (rand-nth band-names))
