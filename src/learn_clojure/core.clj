(ns learn-clojure.core
  (:require [clojure.math.numeric-tower :as math]
            [learn-clojure.util :as util]
            [clojure.set :as set]))

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
   [0 6 0 0 7 0 0 2 4]])


(defn createInitialState []
  (createMediumBoard))

(defonce gridSize 9)


;defonce locks for redefines
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



(defn get-cell
  "Returns a vector with all digits in cell. 0-indexed"
  ([cell-nbr board]
   (get-cell (unchecked-divide-int cell-nbr 3) (mod cell-nbr 3) board))
  ([r, c, board]
   (nth board (+ (* r 3) c))))


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

(defn is-row-valid? [rix board]
  "Returns true if row contains no duplicates"
  (let [row (remove-blanks (get-row rix board))]
    (= (count row) (count (set row)))
    ))

(defn is-cell-valid? [cell-ix board]
  "Return true if cell does not contain duplicate values"
  (let [cellValues (remove-blanks (get-cell cell-ix board))]
    (= (count cellValues) (count (set cellValues)))))

(defn get-value [board r c]
  (let [row (get-row r board)]
    (nth row c)))

(defn get-cell-rc-from-board-rc [r c]
  ;row 0-2 -> cell 0-2
  ;col 0-2 -> cell 0, 3, 6
  ;get-cell-rc-from-board-rc 1 1 -> cell 0, dig 4
  (vector () ()
    ))

(defn set-value! [r c val]
  ;ToDo write function
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
    (= (count col) (count (set col)))))

(defn create-complete-cell [input-cell]
  "Returns a randomly filled valid cell. Takes an empty or not empty cell"
  ;ToDo difference of sets is convenient but does set preserve order of elements at all times?
  (loop [store (vec (set/difference (set [1 2 3 4 5 6 7 8 9]) (set input-cell)))
         cell input-cell
         ix (rand-int (count store))]
    (if (empty? store)
      cell
      (recur (filter #(not= (nth store ix) %) store)
             (if (util/in? cell no-value)                   ;cell has zeroes
               ((comp vec flatten conj)
                 (vector (first (split-with (fn [x] (> x 0)) cell)))
                 (nth store ix)
                 (subvec cell (+ 1 (.indexOf cell 0))))
               (conj cell (nth store ix)))
             (rand-int (- (count store) 1))))))

(defn is-valid? [validator-fn board]
  "Returns true if validator-fn returns true for all"
  ;ToDo Can this func be replace with reduce?
  (loop [ix 0]
    (if (not (validator-fn ix board))
      false
      (if (< ix 8)
        (recur (inc ix))
        true))))

(defn is-board-valid? [board]
  "Returns true if no cell, row or column has duplicate values"
  ;  (and (are-cells-valid? board) (are-rows-valid? board) (are-cols-valid? board))
  (and (is-valid? is-cell-valid? board) (is-valid? is-row-valid? board) (is-valid? is-column-valid? board))
  )

(is-board-valid? (get-board))

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
;(println (clojure.string/replace (clojure.string/join " " (get-row 0 (@state-atom :board))) #"0" " "))
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
;  (def my-atom-map (atom {:map {}}))
;(swap! my-atom-map assoc-in [:map (keyword "1")] 9))

;-------------------------------------
; Guessing game
(def guess-state (atom {}))

(defn initialize-state []
  (swap! guess-state assoc :answer (+ 1 (rand-int 10)))
  (swap! guess-state assoc :counter 0)
  (swap! guess-state assoc :guesses {})
  (add-watch guess-state :guess-watch
             (fn [key atom old-state new-state]
               ;(prn "-- Atom Changed --")
               ;(prn "key" key)
               ;(prn "Keyword is " (keyword (str (:counter new-state))))
               ;(prn "old-state" old-state)
               ;(prn "new-state" new-state)
               (if (not= (old-state :counter) (new-state :counter))
                 (swap! guess-state assoc-in [:guesses (keyword (str (:counter new-state)))] (new-state :guess))))))

(defn -main [& args]
  (initialize-state)
  (println "Guess a number between 1 and 10")
  (print "-> ") (flush)
  (loop []
    (swap! guess-state assoc :guess (Integer/parseInt (re-find #"[0-9]*" (read-line))))
    (swap! guess-state update-in [:counter] inc)
    (if (= (@guess-state :answer) (@guess-state :guess))
      (do
        (println "You guessed" (vals(@guess-state :guesses)))
        (if (< (@guess-state :counter) 4)
          (println "You made it in" (@guess-state :counter) "attempts. Bravo!")
          (println "That is correct!"))
        ))
      (if (< (@guess-state :guess) (@guess-state :answer)) (print "That is too small!\n-> "))
      (if (> (@guess-state :guess) (@guess-state :answer)) (print "That is too big!\n-> "))
      (flush)
      (when (not= (@guess-state :guess) (@guess-state :answer))
        (recur))))




;------------------------------------

(defn absolute-sqrt [x]
  (clojure.math.numeric-tower/sqrt (* x x))
  )

(defn absolute [x]
  (if (< x 0) (* x -1) x))

(defn square [x]
  (* x x)
  )

(defn benchmark [func]
  (loop [x 1]
    (func x)
    (when (< x 1000000)
      (recur (inc x)))))

(time (benchmark absolute))
(time (benchmark absolute-sqrt))
