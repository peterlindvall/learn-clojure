(ns learn-clojure.core
  (:require [clojure.math.numeric-tower :as math]))

;Data structures

(def gridSize 9)

(defn createBoard []
  [[1 1 3 4 5 6 7 8 9]                                      ;r0 c0
   [1 2 3 4 5 6 7 8 9]                                      ;r0 c1
   [1 3 3 4 5 6 7 8 9]                                      ;r0 c2
   [2 1 3 4 5 6 7 8 9]                                      ;r1 c0
   [2 2 3 4 5 6 7 8 9]                                      ;r1 c1
   [2 3 3 4 5 6 7 8 9]                                      ;r1 c2
   [3 1 2 4 5 6 7 8 9]                                      ;r2 c0
   [3 2 3 4 5 6 7 8 9]                                      ;r2 c1
   [3 3 3 4 5 6 7 8 9]                                      ;r2 c2
   ])

(def mBoard (createBoard))
(def empty 0)

(defn getBoard []
  mBoard
  )

(defn getCell [r, c, board]
  "Returns a vector with all digits in cell. 0-indexed"
  ;ToDo Make grid size independent
  (assert (and (> r -1) (> c -1)))
  (assert (and (< r 3) (< c 3)))
  (nth board (+ (* r 3) c))
  )

(defn getValue [r c pos]
  (nth (nth (getBoard) r) c)
  )

(defn isValidCellVerbose? [cell]
  "Is cell valid; 9 entries w/ values 1-9, no duplicates"
  (let [values (count cell)]
    (println values " values"))
  (let [uniqueVals (count (set cell))]
    (println uniqueVals " unique values"))
  (let [tooBig (count (filter #(< 9 %) cell))]
    (println tooBig " too big values"))
  (let [tooSmall (count (filter #(> 1 %) cell))]
    (println tooSmall " too small values"))
  )

(defn isCellComplete? [cell gridSize]
  "does cell contain gridSize unique values larger than 0 and not larger than gridsize"
  (if (= gridSize (count cell))
    (if (= gridSize (count (set cell)))
      (if (= 0 (count (filter #(< gridSize %) cell)))
        (if (= 0 (count (filter #(> 1 %) cell)))
          true false)))))

(defn isCellValid? [cell gridSize]
  "does cell contain valid values"
  (if (>= gridSize (count cell))
    (if (= gridSize (count (set cell)))
      (if (= 0 (count (filter #(< gridSize %) cell)))
        (if (= 0 (count (filter #(> 1 %) cell)))
          true false)))))



(defn createValidSell [])

(defn dups [coll]
  "Return duplicates in collection"
  (let [freqs (frequencies coll)]
    (filter #(< 1 (freqs %)) coll)))

(macroexpand `#(< 1 (freqs %)))
(getBoard)
(getCell 0 2 mBoard)
(count (getCell 0 2 mBoard))
(def cellSet (set (getCell 0 2 mBoard)))
(count cellSet)
(count (frequencies (getCell 0 2 mBoard)))
(isCellComplete? (getCell 0 2 mBoard) gridSize)
(getCell 2 0 mBoard)
(isCellComplete? (getCell 2 0 mBoard) gridSize)
(isCellComplete? (getCell 2 0 mBoard) gridSize)
(isCellComplete? (getCell 2 0 mBoard) gridSize)
(set (getCell 2 0 mBoard))


;-------------------------------------
;Diverse

(def answer -1)
(defn guess [my_guess]
  (assert (> answer 0))
  (def attempts (+ attempts 1))
  (if (= my_guess answer)
    (if (< attempts 4) "You made it in less than four attempts. Bravo!" "Correct!")
    (if (< my_guess answer) "Too small!" "Too big")))
(defn reset []
  (def attempts 0)
  (def answer (int (+ 1 (* (rand) 10))))
  "Guess a number between 1 and 10")

(reset)
(guess 1)
(guess 2)
(guess 3)
(guess 4)
(guess 5)
(guess 6)
(guess 7)
(guess 8)
(guess 9)
(guess 10)

