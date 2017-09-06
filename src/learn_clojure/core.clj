(ns learn-clojure.core
  (:require [clojure.math.numeric-tower :as math]))

;Data structures
(comment
    c0 c1 c2 c3 c4 c5 c6 c7 c8
  -----------------------------
  r0 .......|........|........|
  r1 cell00 | cell01 | cell02 |
  r2 .......|........|........|
  -----------------------------
  r3........|........|........|
  r4 cell10 | cell11 | cell12 |
  r5........|........|........|
  -----------------------------
  r6........|........|........|
  r7 cell20 | cell21 | cell22 |
  r8........|........|........|
  -----------------------------

    Note! A vector in the board datastructure represents a cell, not a row on the board
  )

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
;(createEmptyBoard)

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

(def gridSize 9)


;defonce locks. Not good for debug purposes during development
(def state-atom (atom {:board        (createInitialState)
                       :initialized? false}))

(defn clearBoard []
  (swap! state-atom assoc :board (createEmptyBoard)))

(swap! state-atom assoc :initialized? false)
(get-in @state-atom [:initialized?])
(swap! state-atom assoc :initialized? true)
(get-in @state-atom [:initialized?])

(@state-atom :board)
;(clearBoard)
(get-in @state-atom [:board])

(defn undoMove []
  ;ToDo Add listener to state changes and save all boards
  )

(def notSet 0)

;(defn getBoard [] (get-in @state-atom [:board]))
(defn getBoard [] (@state-atom :board))

(defn getCell [r, c, board]
  "Returns a vector with all digits in cell. 0-indexed"
  (nth board (+ (* r 3) c)))


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
  "Returns tru if cell contain gridSize unique values larger than 0 and not larger than gridsize"
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
  (filter #(< 0 %) vect))


(defn getRow [rix board]
  "Returns the row at rownum. Zero indexed"
  ;r 0 1 2 -> cell 00, 01, 02
  ;r 3 4 5 -> cell 10, 11, 12
  ;r 6 7 8 -> cell 20, 21, 22
  (let [cell1 (getCell (unchecked-divide-int rix 3) 0 board)
        cell2 (getCell (unchecked-divide-int rix 3) 1 board)
        cell3 (getCell (unchecked-divide-int rix 3) 2 board)]
    (vector (nth cell1 (* (mod rix 3) 3)) (nth cell1 (+ (* (mod rix 3) 3) 1)) (nth cell1 (+ (* (mod rix 3) 3) 2))
            (nth cell2 (* (mod rix 3) 3)) (nth cell2 (+ (* (mod rix 3) 3) 1)) (nth cell2 (+ (* (mod rix 3) 3) 2))
            (nth cell3 (* (mod rix 3) 3)) (nth cell3 (+ (* (mod rix 3) 3) 1)) (nth cell3 (+ (* (mod rix 3) 3) 2)))))

(defn isRowValid? [rix board]
  "Returns true if row contains no duplicates"
  (let [row (removeBlanks (getRow rix board))]
    (= (count row) (count (set row)))
    ))

(defn getValue [r c]
  (let [board (@state-atom :board)
        row (getRow r board)]
    (nth row c)))


(defn getColumn [colix board]
  "Returns the col at colnum. Zero indexed"
  ;col 0 1 2 -> cell 00. 10. 20
  ;col 3 4 5 -> cell 01, 11, 21
  ;col 6 7 8 -> cell 02, 12, 22
  (let [cell1 (getCell 0 (unchecked-divide-int colix 3) board)
        cell2 (getCell 1 (unchecked-divide-int colix 3) board)
        cell3 (getCell 2 (unchecked-divide-int colix 3) board)]
    (vector (nth cell1 (mod colix 3)) (nth cell1 (+ (mod colix 3) 3)) (nth cell1 (+ (mod colix 3) 6))
            (nth cell2 (mod colix 3)) (nth cell2 (+ (mod colix 3) 3)) (nth cell2 (+ (mod colix 3) 6))
            (nth cell3 (mod colix 3)) (nth cell3 (+ (mod colix 3) 3)) (nth cell3 (+ (mod colix 3) 6)))))

(defn isColumnValid? [colix board]
  "returns true if column contains no duplicates"
  (let [col (removeBlanks (getColumn colix board))]
    ;(println col " set: " (set col))
    (= (count col) (count (set col)))))

;(dedupe [1 2 3 4])

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
;(println cell)


(defn printBoard [board]
  (println (getRow 0 board))
  (println (getRow 1 board))
  (println (getRow 2 board))
  (println (getRow 3 board))
  (println (getRow 4 board))
  (println (getRow 5 board))
  (println (getRow 6 board))
  (println (getRow 7 board))
  (println (getRow 8 board))
  )

(reduce
  (fn [primes number]
    (if (some zero? (map (partial mod number) primes))
      primes
      (conj primes number)))
  [2]
  (take 1000 (iterate inc 3)))

;(take 1000 (iterate inc 3))

(defn myFirstUseOfReduce [rowix board]
  (let [row (getRow rowix board)]
    ;        strRow (clojure.string/join " " row)]
    (reduce (fn [strRow value]
              (if (= value 0)
                (conj strRow "#")
                (conj strRow (str value))
                ))
            []
            row)))

(defn getRowAsString [rix board]
  (let [row (getRow rix board)
        rowStr (str "|" (clojure.string/replace (clojure.string/join row) #"..." #(str %1 \|)))
        rowStrWSpace (clojure.string/replace rowStr #"0" " ")]
    (clojure.string/replace rowStrWSpace #"." #(str %1 " "))
    )
  )

(defn printBoardOld [board]
  (let [horBar "-------------------------"]
    (println horBar)
    (println (getRowAsString 0 board))
    (println (getRowAsString 1 board))
    (println (getRowAsString 2 board))
    (println horBar)
    (println (getRowAsString 3 board))
    (println (getRowAsString 4 board))
    (println (getRowAsString 5 board))
    (println horBar)
    (println (getRowAsString 6 board))
    (println (getRowAsString 7 board))
    (println (getRowAsString 8 board))
    (println horBar)
    ))

(defn printBoard [board]
  (let [horBar "-------------------------"]
    (loop [r 0]
      (if (get #{0 3 6} r)
        (println horBar))
      (println (getRowAsString r board))
      (when (< r 8)
        (recur (inc r))))
    (println horBar)
    )
  )

;(time (printBoard (@state-atom :board)))
;(time (printBoard learn-clojure.core-test/solvedBoard))

;(println (getRowAsString 3 (@state-atom :board)))
;(println (getRow 3 (@state-atom :board)))
;(println (clojure.string/replace (clojure.string/join " " (getRow 0 (get-in @state-atom [:board]))) #"0" " "))
;(printBoard (get-in @state-atom [:board]))

(loop [x 10]
  (when (> x 1)
    (println x)
    (recur (- x 2))))


(macroexpand `#(< 1 (freqs %)))
;(getBoard)

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

