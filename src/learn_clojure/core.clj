(ns learn-clojure.core
  (:require [clojure.math.numeric-tower :as math]
            [learn-clojure.util :as util]
            [clojure.set :as set]
            [ysera.test :refer [is is= is-not]]))

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

  Note! A vector in the board data structure represents a cell, not a row on the board
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

(def state-atom (atom {:board          (createMediumBoard)
                       :initialized?   false
                       :previousBoards []}))

(defn update-board! [board]
  (swap! state-atom assoc :board board))

(defn clear-board! []
  (swap! state-atom assoc :board (createEmptyBoard)))

(defn reset-board! []
  (swap! state-atom assoc :board (createMediumBoard)))

;(swap! state-atom assoc :initialized? false)
;(get-in @state-atom [:initialized?])
;(@state-atom :initialized?)
;(swap! state-atom assoc :initialized? true)

(defn undo-move! []
  ;ToDo Add listener to state changes that saves all previous boards
  )

(defonce no-value 0)

;(defn get-board [] (get-in @state-atom [:board]))
(defn get-board [] (@state-atom :board))
(@state-atom :board)

(defn get-cell
  "Returns a cell from the board"
  ([board, cell-nbr]
   (get-cell board (unchecked-divide-int cell-nbr 3) (mod cell-nbr 3)))
  ([board, r, c]
   (nth board (+ (* r 3) c))))

(defn is-cell-complete? [cell]
  "Return true if cell contains 9 unique values between 1..9"
  (if (and (= 9 (count cell)) (= 9 (count (set cell))))
    (if (every? (fn [x] (and (< x 10) (> x 0))) cell)
      true false)))

#_(defn is-cell-complete? [cell]
    "Return true if cell contains 9 unique values between 1..9"
    ;works fine but is 10% slower, possibly because two iterations over the vector
    (and
      (= 9 (count cell))
      (= 9 (count (set cell)))
      (< (apply max cell) 10)
      (> (apply min cell) 0))
    )

(defn remove-blanks
  {:test (fn [] (is (= (remove-blanks [0 1 0 2 0 3 0 0 4]) [1 2 3 4])))}
  [value-vect]
  "Returns a vector without blanks"
  {:pre [(vector? value-vect)]}
  (filter #(not= no-value %) value-vect))

(defn get-row [rix board]
  "Returns the row at rownum. Zero indexed"
  ;r 0 1 2 -> cell 00, 01, 02
  ;r 3 4 5 -> cell 10, 11, 12
  ;r 6 7 8 -> cell 20, 21, 22
  (let [cell-row (unchecked-divide-int rix 3)
        cell1 (get-cell board cell-row 0)
        cell2 (get-cell board cell-row 1)
        cell3 (get-cell board cell-row 2)
        cellOffset (mod rix 3)]
    (vector (nth cell1 (* cellOffset 3)) (nth cell1 (+ (* cellOffset 3) 1)) (nth cell1 (+ (* cellOffset 3) 2))
            (nth cell2 (* cellOffset 3)) (nth cell2 (+ (* cellOffset 3) 1)) (nth cell2 (+ (* cellOffset 3) 2))
            (nth cell3 (* cellOffset 3)) (nth cell3 (+ (* cellOffset 3) 1)) (nth cell3 (+ (* cellOffset 3) 2)))))

(defn is-row-valid? [rix board]
  "Returns true if row contains no duplicates"
  (let [row (remove-blanks (get-row rix board))]
    (= (count row) (count (set row)))))

(defn is-cell-valid? [cell-ix board]
  "Return true if cell does not contain duplicate values"
  (let [cellValues (remove-blanks (get-cell board cell-ix))]
    (= (count cellValues) (count (set cellValues)))))

(defn get-value [board r c]
  (let [row (get-row r board)]
    (nth row c)))

(defn board-pos-to-cell-pos [r c]
  "returns the cell position for any given board position"
  (vector (unchecked-divide-int r 3) (unchecked-divide-int c 3)))

(defn board-pos-to-cell-ix [r c]
  (let [cellpos (board-pos-to-cell-pos r c)]
    (+ (* (first cellpos) 3) (second cellpos))))

(defn board-pos-to-value-ix [r c]
  "returns the index the board pos has in the cell"
  (+ (* (mod r 3) 3) (mod c 3)))

(defn set-value-in-cell [cell pos val]
  "returns a new cell with val set at given position"
  ((comp vec flatten conj)
    (subvec cell 0 pos)
    (vector val)
    (subvec cell (+ 1 pos))))

(defn set-cell-in-board [board pos cell]
  "returns a new cell with val set at given position"
  (apply conj (subvec board 0 pos) cell (subvec board (+ pos 1)))
  )

(defn set-value-in-board [board r c val]
  "returns a board with the new value set at the given position"
  (let [cellix (board-pos-to-cell-ix r c)
        cell (get-cell board cellix)]
    (set-cell-in-board board cellix (set-value-in-cell cell (board-pos-to-value-ix r c) val))
    ))

(defn get-column [colix board]
  "Returns the col at colnum. Zero indexed"
  ;col 0 1 2 -> cell 00. 10. 20
  ;col 3 4 5 -> cell 01, 11, 21
  ;col 6 7 8 -> cell 02, 12, 22
  (let [cell-col (unchecked-divide-int colix 3)
        cell1 (get-cell board 0 cell-col)
        cell2 (get-cell board 1 cell-col)
        cell3 (get-cell board 2 cell-col)
        offset (mod colix 3)]
    (vector (nth cell1 offset) (nth cell1 (+ offset 3)) (nth cell1 (+ offset 6))
            (nth cell2 offset) (nth cell2 (+ offset 3)) (nth cell2 (+ offset 6))
            (nth cell3 offset) (nth cell3 (+ offset 3)) (nth cell3 (+ offset 6)))))

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
  "Returns true if validator-fn returns true for the board"
  ;ToDo Can this func be replaced with reduce?
  (loop [ix 0]
    (if (not (validator-fn ix board))
      false
      (if (< ix 8)
        (recur (inc ix))
        true))))

(defn is-board-valid? [board]
  "Returns true if no cell, row or column has duplicate values"
  (and (is-valid? is-cell-valid? board) (is-valid? is-row-valid? board) (is-valid? is-column-valid? board))
  )

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

#_(defn find-valid-move
    "Returns a collection of of valid moves (boardrow, boardcol, digit)"
    ;ToDo
    {:test (fn []
             (let [board (createMediumBoard)]
               (is (= (find-valid-move board) [0 4 6]))))}
    [board]
    (let [freq-map (frequencies (remove-blanks (flatten board)))
          sorted-freq-map (into (sorted-map-by (fn [key1 key2]
                                                 (compare [(get freq-map key2) key2]
                                                          [(get freq-map key1) key1]))) freq-map) ;sort map on values
          value-list (keys sorted-freq-map)
          value (first value-list)
          cell (get-cell board 0)
          ]
      (println cell)
      (loop [pos 0
             valid-positions []]
        (if (and (= (nth cell pos) no-value) (is-board-valid? (set-value-in-board board (unchecked-divide-int pos 3) (mod pos 3) value))
                 (when (< pos 8)
                   (recur (inc pos) (conj valid-positions pos)))
                 (println valid-positions))
          )
        [0 4 6]
        )))

;  (find-valid-move (get-board))

(def m2 {1 2 3 4})
(hash-map 1 2 3 4)
;-----------------
;play soduko

;(print-board (get-board))
;(clear-board!)
;(reset-board!)

(defn play-around [cell-nbr]
  (let [newCell (create-complete-cell (get-cell (get-board) cell-nbr))
        newBoard (set-cell-in-board (get-board) cell-nbr newCell)]
    (when (is-board-valid? newBoard)
      (update-board! newBoard))))

(defn brute-force [cell-nbr]
  (loop [counter 0]
    (if (play-around cell-nbr)
      (println "Counter: " counter)
      (recur (inc counter))))
  (print-board (get-board)))

;(println (find-valid-move (get-board)))
;  (time (brute-force 5))
(update-board! (set-value-in-board (@state-atom :board) 0 8 2))
(is-board-valid? (get-board))
;(print-board (get-board))
;--------------------

;(type(read-line))

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
(defn create-state
  {:test (fn []
           (is= (create-state 0 :counter 2 :guesses {:1 5 :2 7})
                {:answer  0
                 :counter 2
                 :guesses {:1 5 :2 7}})
           (is (= (create-state 5)
                  {:answer  5
                   :counter 0
                   :guesses {}})))}

  [correct-answer & key-vals]
  (reduce (fn [a [k v]]
            (assoc a k v))
          ;initial value of a
          {:answer  correct-answer
           :counter 0
           :guesses {}}
          ;collection of vs
          (partition 2 key-vals)))

;(partition 2 [1 2 3 4 5])

(defn get-map-skip-last
  "Returns a map without the last entry in input map"
  {:test (fn []
           (let [m {:1 1 :2 2 :3 3}]
             (is (= (get-map-skip-last m) {:1 1 :2 2}))))}
  [map]
  {:pre [(map? map)]}
  (zipmap (take (- (count map) 1) (keys map)) (vals map))
  )


(defn undo
  {:test (fn []
           (is= (undo (create-state 0)) (create-state 0))
           (is= (-> (create-state 0 :counter 2 :guesses {:1 5 :2 7})
                    (undo)
                    (:counter))
                1)
           (is= (:guesses (undo (create-state 0 :counter 2 :guesses {:1 5 :2 7})))
                {:1 5})
           )}
  [state]
  (-> state
      (update :counter (fn [counter] (max 0 (dec counter))))
      (update :guesses get-map-skip-last)))

(defn guess
  {:test (fn []
           (is= (-> (create-state 0 :counter 2 :guesses {:1 5 :2 7})
                    (guess 6)
                    (:counter))
                3)
           (is= (-> (create-state 0 :counter 2 :guesses {:1 5 :2 7})
                    (guess 6)
                    (:guesses))
                {:1 5 :2 7 :3 6})
           )}
  [state value]
  (as-> state $
        (update $ :counter inc)
        (assoc-in $ [:guesses (keyword (str (:counter $)))] value)))

;------------------
(def guess-state (atom (create-state (+ 1 (rand-int 10)))))

(defn undo! [] (swap! guess-state undo))

(defn guess! [value] (swap! guess-state guess value))

(defn process-input-string
  {:test (fn []
           (is (= (process-input-string "4") 4))
           (is (= (process-input-string "r4") "r"))
           (is (= (process-input-string "q\n") "q"))
           )}
  [str]
  (let [int-str (re-find #"[0-9]*" str)
        str-str (re-find #"[a-zA-Z]*" str)]
    (if (> (count int-str) 0)
      (Integer/parseInt int-str)
      str-str)))

(defn -main [& args]
  (println "Guess a number between 1 and 10")
  (print "-> ") (flush)
  (loop [input (process-input-string (read-line))]
    ;(println "before " @guess-state)
    (cond (= input "u")
          (undo!)

          (string? input)
          (print "\n->")

          :else
          (do (println "You entered a digit")
              (guess! input)
              (cond (= (:answer @guess-state) input)
                    (do (println "You guessed" (vals (:guesses @guess-state)))
                        (if (< (:counter @guess-state) 4)
                          (println "You made it in" (:counter @guess-state) "attempts. Bravo!")
                          (println "That is correct!")))

                    (< input (:answer @guess-state))
                    (print "That is too small!\n-> ")

                    (> input (@guess-state :answer))
                    (print "That is too big!\n-> "))))
    (flush)
    (when-not (= input (:answer @guess-state))
      (recur (process-input-string (read-line))))))


#_(defn add-guess-watch []
    (add-watch guess-state :guess-watch
               (fn [key atom old-state new-state]
                 (if (not= (old-state :counter) (new-state :counter))
                   (swap! guess-state assoc-in [:guesses (keyword (str (:counter new-state)))] (new-state :guess))))))

#_(defn initialize-state! []
    (swap! guess-state assoc :answer (+ 1 (rand-int 10)))
    (swap! guess-state assoc :counter 0)
    (swap! guess-state assoc :guesses {})
    (add-guess-watch))

#_(defn increase-guess-counter! []
    (swap! guess-state update-in [:counter] inc))

#_(defn update-guess! [guess]
    (swap! guess-state assoc :guess guess))

#_(defn undo-last-guess! []
    (let [guesses (@guess-state :guesses)
          counter (@guess-state :counter)]
      (remove-watch guess-state :guess-watch)
      (print "I see you are regretful")
      (swap! guess-state assoc :guesses (get-map-skip-last guesses))
      (swap! guess-state update-in [:counter] dec)
      )
    (add-guess-watch)
    )


#_(defn -main [& args]
    (initialize-state!)
    (println "Guess a number between 1 and 10")
    (print "-> ") (flush)
    (loop [input (process-input-string (read-line))]
      ;(println "before " @guess-state)
      (if (string? input)
        (do
          ;(println "You entered a character")
          (if (= input "u")
            (undo-last-guess!)
            )
          (print "\n->")
          )
        (do
          ;(println "You entered a digit")
          (update-guess! input)
          (increase-guess-counter!)
          (if (= (@guess-state :answer) (@guess-state :guess))
            (do
              (println "You guessed" (vals (@guess-state :guesses)))
              (if (< (@guess-state :counter) 4)
                (println "You made it in" (@guess-state :counter) "attempts. Bravo!")
                (println "That is correct!"))
              ))
          (if (< (@guess-state :guess) (@guess-state :answer)) (print "That is too small!\n-> "))
          (if (> (@guess-state :guess) (@guess-state :answer)) (print "That is too big!\n-> "))
          ))
      (flush)
      ;(println "after " @guess-state)
      (when-not (= (@guess-state :guess) (@guess-state :answer))
        (recur (process-input-string (read-line))))))


;1 miljon människor känner obehag minst 1 ggn per månad när de tänker på arbetet.

;------------------------------------

(defn absolute-sqrt [x]
  (clojure.math.numeric-tower/sqrt (* x x))
  )

(defn absolute
  {:test (fn [] (is (= (absolute -42) 42)))}
  [x]
  {:pre [(integer? x)]}
  (if (< x 0) (* x -1) x))

(defn square
  {:test (fn [] (is (= (square 4) 16)))}
  [x]
  (* x x)
  )

#_(defn benchmark [func]
    (loop [x 1]
      (func x)
      (when (< x 1000000)
        (recur (inc x)))))

;(time (benchmark absolute))
;(time (benchmark absolute-sqrt))
