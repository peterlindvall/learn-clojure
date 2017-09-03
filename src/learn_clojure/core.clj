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

(defn createEmptyBoard []
  [[0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]])
(createEmptyBoard)

(defn createMediumBoard []
  [[0 0 5 0 0 0 9 0 0]
   [0 9 6 0 0 0 0 0 0]
   [0 8 0 0 0 7 4 0 6]
   [0 8 4 1 0 0 6 0 0]
   [2 0 0 0 0 7 0 0 0]
   [0 0 0 5 4 0 3 0 0]
   [0 0 0 0 0 6 5 7 0]
   [0 0 9 0 3 5 0 0 0]
   [0 0 0 0 7 0 0 2 4]])


(defn createInitialState []
  (createMediumBoard))

;defonce locks. Not good for debug purposes during development
(def state-atom (atom {:board (createInitialState)
                       :initialized? false}))

(defn clearBoard []
  (swap! state-atom assoc :board (createEmptyBoard)))

(swap! state-atom assoc :initialized? false)
(get-in @state-atom [:initialized?])
(swap! state-atom assoc :initialized? true)
(get-in @state-atom [:initialized?])

(get-in @state-atom [:board])
(clearBoard)
(get-in @state-atom [:board])

(defn undoMove []
  ;ToDo Add listener to state changes and save all boards
  )

(def notSet 0)

(defn getBoard []
  (get-in @state-atom [:board])
  )
(getBoard)

(defn getCell [r, c, board]
  "Returns a vector with all digits in cell. 0-indexed"
  (nth board (+ (* r 3) c))
  )

(defn getValue [r c]
  (nth (nth (@state-atom :board) r) c)
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

(defn isCellComplete? [cell]
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

(defn removeBlanks [vect]
  "Returns a vector without blanks"
  (filter #(> 1 %) vect))


(defn getRow [rix board]
  "Returns the row at rownum. Zero indexed"
  ;ToDo - broken. Write test!
  (let [cell1 (getCell rix 0 board) ;mod 3
        cell2 (getCell rix 1 board)
        cell3 (getCell rix 2 board)]
    (vector (nth cell1 0)(nth cell1 1)(nth cell1 2)
            (nth cell2 0)(nth cell1 2)(nth cell2 2)
            (nth cell3 0)(nth cell3 1)(nth cell3 2))))

(defn isRowValid? [rix board]
  "Returns true if row contains no duplicates"
  (let [row (removeBlanks (getRow rix board))]
    (= (count row) (count (set row)))
    ))

(defn getColumn [colix board]
  "Returns the col at colnum. Zero indexed"
  (let [cell1 (getCell 0 colix board)
        cell2 (getCell 1 colix board)
        cell3 (getCell 2 colix board)]
    (vector (nth cell1 0)(nth cell1 1)(nth cell1 2)
            (nth cell2 0)(nth cell1 2)(nth cell2 2)
            (nth cell3 0)(nth cell3 1)(nth cell3 2))))

(defn isColumnValid? [colnum board]
  "returns true if column contains no duplicates"
 ; (let )
  true)


(defn createCompleteCell []
  "Fill cell with random values"
  ;vector with all digs
  (let [store [1 2 3 4 5 6 7 8 9]
        lcell []
        dig (rand-int (count store))]
    (loop [x 8]
      (when (>= x 0)
        (conj lcell (nth store (rand-int (count store))))
        ;(println (conj cell (nth store (rand-int (count store )))))
        (println lcell)

        ))
    lcell
    ))

(def cell (createCompleteCell))
(println cell)


(defn printBoard []
  (println (getRow 0 (@state-atom :board)))
  (println (getRow 1 (@state-atom :board)))
  (println (getRow 2 (@state-atom :board)))
  (println (getRow 3 (@state-atom :board)))
  (println (getRow 4 (@state-atom :board)))
  (println (getRow 5 (@state-atom :board)))
  (println (getRow 6 (@state-atom :board)))
  (println (getRow 7 (@state-atom :board)))
  (println (getRow 8 (@state-atom :board)))
  )
(get-in @state-atom [:board])
(printBoard)




    (loop [x 10]
  (when (> x 1)
    (println x)
    (recur (- x 2))))


(macroexpand `#(< 1 (freqs %)))
(getBoard)
(getCell 0 2 mBoard)
(count (getCell 0 2 mBoard))
(def cellSet (set (getCell 0 2 mBoard)))
(count cellSet)
(count (frequencies (getCell 0 2 mBoard)))
(isCellComplete? (getCell 0 2 mBoard))
(getCell 2 0 mBoard)
(isCellComplete? (getCell 2 0 mBoard))
(isCellComplete? (getCell 2 0 mBoard))
(isCellComplete? (getCell 2 0 mBoard))
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

