(ns learn-clojure.util)

(defn in?
  "Returns true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn getDups [coll]
  "Return duplicates in collection"
  (let [freqs (frequencies coll)]
    (filter #(< 1 (freqs %)) coll)))
