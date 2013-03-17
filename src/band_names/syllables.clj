(ns band-names.syllables
  (:refer-clojure :exclude [replace])
  (:use [clojure.string :exclude [reverse]]
        [clojure.math.numeric-tower]
        [incanter.core :exclude [abs sqrt]]
        [inflections.core :exclude [capitalize]]))

(defn slurp-lines [file]
  (split-lines (slurp file)))

(def single-syl-nouns (set (slurp-lines "resources/nouns/1syllablenouns.txt")))
(def double-syl-nouns (set (slurp-lines "resources/nouns/2syllablenouns.txt")))
(def triple-syl-nouns (set (slurp-lines "resources/nouns/3syllablenouns.txt")))
(def four-syl-nouns (set (slurp-lines "resources/nouns/4syllablenouns.txt")))

(def single-syl-verbs (set (slurp-lines "resources/verbs/1syllableverbs.txt")))
(def double-syl-verbs (set (slurp-lines "resources/verbs/2syllableverbs.txt")))
(def triple-syl-verbs (set (slurp-lines "resources/verbs/3syllableverbs.txt")))
(def four-syl-verbs (set (slurp-lines "resources/verbs/4syllableverbs.txt")))

(def single-syl-adjectives (set (slurp-lines "resources/adjectives/1syllableadjectives.txt")))
(def double-syl-adjectives (set (slurp-lines "resources/adjectives/2syllableadjectives.txt")))
(def triple-syl-adjectives (set (slurp-lines "resources/adjectives/3syllableadjectives.txt")))
(def four-syl-adjectives (set (slurp-lines "resources/adjectives/4syllableadjectives.txt")))

(def single-syl-adverbs (set (slurp-lines "resources/adverbs/1syllableadverbs.txt")))
(def double-syl-adverbs (set (slurp-lines "resources/adverbs/2syllableadverbs.txt")))
(def triple-syl-adverbs (set (slurp-lines "resources/adverbs/3syllableadverbs.txt")))
(def four-syl-adverbs (set (slurp-lines "resources/adverbs/4syllableadverbs.txt")))

;------------

(defn simple-syllable-count [word]
  (let [{count :count last-vowel? :last-vowel?}
        (reduce #(if (#{\A \E \É \I \O \U \Y \a \e \é \i \o \u \y} %2)
                   (if (not (:last-vowel? %1))
                     (assoc %1 :count (inc (:count %1)) :last-vowel? true)
                     (assoc %1 :last-vowel? false))
                   (assoc %1 :last-vowel? false))
                {:count 0}
                word)]
    (max
     (if (#{\E \e} (last word))
       (dec count)
       count)
     1)))

(defn count-syllables [phrase]
  (reduce 
   +
   (for [word (split (replace phrase #"\W" "") #"\s+")]
     (cond (or (single-syl-nouns word)
               (single-syl-verbs word)
               (single-syl-adjectives word)
               (single-syl-adverbs word))
           1
           (or (double-syl-nouns word)
               (double-syl-verbs word)
               (double-syl-adjectives word)
               (double-syl-adverbs word))
           2
           (or (triple-syl-nouns word)
               (triple-syl-verbs word)
               (triple-syl-adjectives word)
               (triple-syl-adverbs word))
           3
           (or (four-syl-nouns word)
               (four-syl-verbs word)
               (four-syl-adjectives word)
               (four-syl-adverbs word))
           4
           :else (simple-syllable-count word)))))
   
