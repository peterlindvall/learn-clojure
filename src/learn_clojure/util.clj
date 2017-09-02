(ns learn-clojure.util)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn dups [coll]
  "Return duplicates in collection"
  (let [freqs (frequencies coll)]
    (filter #(< 1 (freqs %)) coll)))
