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

(defonce gridSize 9)


;defonce locks. Not good for debug purposes during development
(def state-atom (atom {:board          (createInitialState)
                       :initialized?   false
                       :previousBoards []}))

(defn clear-board! []
  (swap! state-atom assoc :board (createEmptyBoard)))

;(swap! state-atom assoc :initialized? false)
;(get-in @state-atom [:initialized?])
;(@state-atom :initialized?)
;(swap! state-atom assoc :initialized? true)

(defn undo-move! []
  ;ToDo Add listener to state changes that saves all previous boards
  )

(def no-value 0)

;(defn get-board [] (get-in @state-atom [:board]))
(defn get-board [] (@state-atom :board))

(defn get-cell [r, c, board]
  "Returns a vector with all digits in cell. 0-indexed"
  (nth board (+ (* r 3) c)))


(defn is-cell-complete? [cell]
  "Return true if cell contains 9 unique values between 1..9"
  (if (and (= 9 (count cell)) (= 9 (count (set cell))))
    (if (every? (fn [x] (and (< x 10) (> x 0))) cell)
      true false)))

(defn remove-blanks [vect]
  "Returns a vector without blanks"
  (filter #(not= no-value %) vect))

(defn get-row [rix board]
  "Returns the row at rownum. Zero indexed"
  ;r 0 1 2 -> cell 00, 01, 02
  ;r 3 4 5 -> cell 10, 11, 12
  ;r 6 7 8 -> cell 20, 21, 22
  (let [cell1 (get-cell (unchecked-divide-int rix 3) 0 board)
        cell2 (get-cell (unchecked-divide-int rix 3) 1 board)
        cell3 (get-cell (unchecked-divide-int rix 3) 2 board)
        cellOffset (mod rix 3)]
    (vector (nth cell1 (* cellOffset 3)) (nth cell1 (+ (* cellOffset 3) 1)) (nth cell1 (+ (* cellOffset 3) 2))
            (nth cell2 (* cellOffset 3)) (nth cell2 (+ (* cellOffset 3) 1)) (nth cell2 (+ (* cellOffset 3) 2))
            (nth cell3 (* cellOffset 3)) (nth cell3 (+ (* cellOffset 3) 1)) (nth cell3 (+ (* cellOffset 3) 2)))))

(defn isRowValid? [rix board]
  "Returns true if row contains no duplicates"
  (let [row (remove-blanks (get-row rix board))]
    (= (count row) (count (set row)))
    ))

(defn is-cell-valid? [cell]
  "Return true if cell does not contain duplicate values"
  (let [cellValues (remove-blanks cell)]
    (= (count cellValues) (count (set cellValues)))))

(defn get-value [r c]
  (let [board (@state-atom :board)
        row (get-row r board)]
    (nth row c)))

(defn get-cell-rc-from-board-rc [r c]
  (let []
    ))

(defn set-value! [r c val]
  (let [board (@state-atom :board)
        rcVector (get-cell-rc-from-board-rc r c)
        cell (get-cell (first rcVector) (last rcVector) board)]
    ;set value in cell
    ()
    ;set cell in board

    ;set board in state-atom

    (swap! state-atom assoc :board (board))
    ))

(defn get-column [colix board]
  "Returns the col at colnum. Zero indexed"
  ;col 0 1 2 -> cell 00. 10. 20
  ;col 3 4 5 -> cell 01, 11, 21
  ;col 6 7 8 -> cell 02, 12, 22
  (let [cell1 (get-cell 0 (unchecked-divide-int colix 3) board)
        cell2 (get-cell 1 (unchecked-divide-int colix 3) board)
        cell3 (get-cell 2 (unchecked-divide-int colix 3) board)]
    (vector (nth cell1 (mod colix 3)) (nth cell1 (+ (mod colix 3) 3)) (nth cell1 (+ (mod colix 3) 6))
            (nth cell2 (mod colix 3)) (nth cell2 (+ (mod colix 3) 3)) (nth cell2 (+ (mod colix 3) 6))
            (nth cell3 (mod colix 3)) (nth cell3 (+ (mod colix 3) 3)) (nth cell3 (+ (mod colix 3) 6)))))

(defn is-column-valid? [colix board]
  "returns true if column contains no duplicates"
  (let [col (remove-blanks (get-column colix board))]
    ;(println col " set: " (set col))
    (= (count col) (count (set col)))))

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

(comment
  (reduce
    (fn [primes number]
      (if (some zero? (map (partial mod number) primes))
        primes
        (conj primes number)))
    [2]
    (take 1000 (iterate inc 3)))
  )

(comment
  (defn myFirstUseOfReduce [rowix board]
    (let [row (get-row rowix board)]
      ;        strRow (clojure.string/join " " row)]
      (reduce (fn [strRow value]
                (if (= value 0)
                  (conj strRow "#")
                  (conj strRow (str value))
                  ))
              []
              row)))
  )

(defn get-row-as-string [rix board]
  (let [row (get-row rix board)
        rowStr (str "|" (clojure.string/replace (clojure.string/join row) #"..." #(str %1 \|)))
        rowStrWSpace (clojure.string/replace rowStr #"0" " ")]
    (clojure.string/replace rowStrWSpace #"." #(str %1 " "))
    )
  )

(defn print-board [board]
  (let [horBar "-------------------------"]
    (loop [r 0]
      (if (get #{0 3 6} r)
        (println horBar))
      (println (get-row-as-string r board))
      (when (< r 8)
        (recur (inc r))))
    (println horBar)
    )
  )

;(time (printBoard (@state-atom :board)))
;(time (printBoard learn-clojure.core-test/solvedBoard))
;(println (getRow 3 (@state-atom :board)))
;(println (getRowAsString 3 (@state-atom :board)))
;(println (clojure.string/replace (clojure.string/join " " (getRow 0 (get-in @state-atom [:board]))) #"0" " "))
;(printBoard (get-in @state-atom [:board]))
#_(defn -main [& args]
    ;;   "Application entry point"
    ;;   (comment Do app initialization here))
    (loop [x 10]
      (when (> x 1)
        (println x)
        (recur (- x 2))))
    )
;(macroexpand `#(< 1 (freqs %)))

;-------------------------------------
;Diverse

(def counter (atom 0))

(defn -main [& args]
  (let [answer (+ 1 (rand-int 10))]
    (println "Guess a number between 1 and 10")
    (print "-> ")(flush)
    (loop []
      (swap! counter inc)
      (def guess (Integer/parseInt (re-find #"[0-9]*" (read-line))))
      (if (= guess answer)
        (if (< @counter 4)
          (println "You made it in" @counter "attempts. Bravo!")
          (println "That is correct!")))
      (if (< guess answer) (print "That is too small!\n-> "))
      (if (> guess answer) (print "That is too big!\n-> "))
      (flush)
      (when (not= guess answer)
        (recur)))))

