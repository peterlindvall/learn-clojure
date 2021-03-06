(ns learn-clojure.core-test
  (:require [clojure.test :refer :all]
            [learn-clojure.core :refer :all]
            [learn-clojure.util :refer :all]))


(defonce solvedBoard [[5 3 4 6 7 2 1 9 8]
                      [6 7 8 1 9 5 3 4 2]
                      [9 1 2 3 4 8 5 6 7]
                      [8 5 9 4 2 6 7 1 3]
                      [7 6 1 8 5 3 9 2 4]
                      [4 2 3 7 9 1 8 5 6]
                      [9 6 1 2 8 7 3 4 5]
                      [5 3 7 4 1 9 2 8 6]
                      [2 8 4 6 3 5 1 7 9]])


(deftest is-cell-complete-test
  (let [board [[1 2 3 4 5 6 7 8 9]                          ;r0 c0 (correct)
               [1 2 3 4 5 6 7 8]                            ;r0 c1 (value missing)
               [1 2 0 4 5 6 7 8 9]                          ;r0 c2 (invalid value, to small)
               [1 2 3 4 5 6 7 8 10]                         ;r1 c0 (invalid value, to large)
               [2 2 3 4 5 6 7 8 9]                          ;r1 c1 (has duplicate value)
               [1 2 3 4 5 6 7 8 9 10]                       ;r1 c2 (too many values)
               [1 2 3 4 5 6 7 8 9]                          ;r2 c0
               [3 2 3 4 5 6 7 8 9]                          ;r2 c1
               [3 3 3 4 5 6 7 8 9]]]                        ;r2 c2

    (is (is-cell-complete? (nth board 0)))
    (is (not (is-cell-complete? (nth board 1))))
    (is (not (is-cell-complete? (nth board 2))))
    (is (not (is-cell-complete? (nth board 3))))
    (is (not (is-cell-complete? (nth board 4))))
    (is (not (is-cell-complete? (nth board 5))))))

(deftest get-cell-test
  (let [cell00 [1 2 3 4 5 6 7 8 9]
        cell01 [11 12 13 14 15 16 17 18 19]
        cell02 [21 22 23 24 25 26 27 28 29]
        cell10 [31 32 33 34 35 36 37 38 39]
        cell11 [41 42 43 44 45 46 47 48 49]
        cell12 [51 52 53 54 55 56 57 58 59]
        cell20 [61 62 63 64 65 66 67 68 69]
        cell21 [71 72 73 74 75 76 77 78 79]
        cell22 [81 82 83 84 85 86 87 88 89]
        board [cell00 cell01 cell02
               cell10 cell11 cell12
               cell20 cell21 cell22]]
    (is (= (get-cell board 0 0) cell00))
    (is (= (get-cell board 0 1) cell01))
    (is (= (get-cell board 1 2) cell12))
    (is (= (get-cell board 2 2) cell22))
    (is (= (get-cell board 0) cell00))
    (is (= (get-cell board 1) cell01))
    (is (= (get-cell board 5) cell12))
    (is (= (get-cell board 8) cell22))
    ))


(deftest get-row-test
  (let [board [[1 2 3 4 5 6 7 8 9]
               [11 12 13 14 15 16 17 18 19]
               [21 22 23 24 25 26 27 28 29]
               [31 32 33 34 35 36 37 38 39]
               [41 42 43 44 45 46 47 48 49]
               [51 52 53 54 55 56 57 58 59]
               [61 62 63 64 65 66 67 68 69]
               [71 72 73 74 75 76 77 78 79]
               [81 82 83 84 85 86 87 88 89]]]
    (is (= (get-row 0 board) [1 2 3 11 12 13 21 22 23]))
    (is (= (get-row 3 board) [31 32 33 41 42 43 51 52 53]))
    (is (= (get-row 8 board) [67 68 69 77 78 79 87 88 89]))
    ))

(deftest get-column-test
  (let [board solvedBoard
        col1 (get-column 0 board)
        col4 (get-column 3 board)
        col9 (get-column 8 board)]
    (print-board board)
    (is (= col1 [5 6 1 8 4 7 9 2 3]))
    (is (= col4 [6 1 3 7 8 9 5 4 2]))
    (is (= col9 [2 8 7 3 1 6 4 5 9]))
    ))

(deftest is-column-valid?-test
  (let [validBoard solvedBoard
        invalidBoard [[5 3 4 6 7 2 1 9 8]
                      [6 8 7 1 9 5 3 4 2]
                      [9 1 2 4 3 8 5 6 7]
                      [8 5 9 4 2 6 7 1 3]
                      [7 6 1 8 5 3 2 9 4]
                      [4 2 3 7 9 1 8 5 6]
                      [9 5 1 0 8 7 4 3 6]
                      [5 3 4 7 1 9 2 8 6]
                      [9 8 4 6 3 5 1 7 2]]
        ]
    ;(println "Invalid board")
    ;(printBoard invalidBoard)
    (is (is-column-valid? 0 validBoard))
    (is (is-column-valid? 2 validBoard))
    (is (is-column-valid? 3 validBoard))
    (is (is-column-valid? 6 validBoard))
    (is (is-column-valid? 8 validBoard))
    (is (not (is-column-valid? 1 invalidBoard)))
    (is (not (is-column-valid? 3 invalidBoard)))
    (is (not (is-column-valid? 7 invalidBoard)))
    (is (not (is-column-valid? 8 invalidBoard)))
    ))

(deftest is-row-valid?-test
  (let [validBoard solvedBoard
        invalidBoard [[5 3 4 6 7 2 1 9 8]
                      [6 8 7 1 9 5 3 4 2]
                      [9 6 2 4 3 8 5 1 7]
                      [8 6 9 4 2 5 7 1 3]
                      [7 6 1 8 5 3 2 9 4]
                      [4 2 3 7 9 1 3 5 6]
                      [9 5 1 2 8 7 4 8 6]
                      [5 3 4 7 1 9 2 8 6]
                      [9 8 4 6 3 5 1 7 2]]]
    ;(println (getRow 5 invalidBoard))
    (is (is-row-valid? 0 validBoard))
    (is (is-row-valid? 2 validBoard))
    (is (is-row-valid? 5 validBoard))
    (is (is-row-valid? 8 validBoard))
    (is (not (is-row-valid? 0 invalidBoard)))
    (is (not (is-row-valid? 2 invalidBoard)))
    (is (not (is-row-valid? 5 invalidBoard)))
    (is (not (is-row-valid? 8 invalidBoard)))
    ))

(deftest remove-blanks-test
  (let [vectorWithZeroes [0 1 2 3 0 4 5 0]]
    (is (= (remove-blanks vectorWithZeroes) [1 2 3 4 5]))
    )
  )

(deftest is-cell-valid?-test
  (let [
        validBoard [[5 3 4 6 7 2 1 9 8]]
        invalidBoard [[5 3 4 6 7 8 1 9 8]
                      [6 8 0 1 9 6 3 4 2]
                      [9 7 2 4 3 8 5 1 7]]]
    (is (is-cell-valid? 0 validBoard))
    (is (not (is-cell-valid? 0 invalidBoard)))
    (is (not (is-cell-valid? 0 invalidBoard)))
    (is (not (is-cell-valid? 0 invalidBoard)))))

(deftest create-complete-cell-test
  (let [cell1 (create-complete-cell [])
        cell2 (create-complete-cell [5 3])
        cell3 (create-complete-cell [0 0 0 0 0 0 0 0 0])
        cell4 (create-complete-cell [0 3 0 6 0])]
    (is (is-cell-complete? cell1))
    (is (is-cell-complete? cell2))
    (is (is-cell-complete? cell3))
    (is (is-cell-complete? cell4))
    ))

(deftest get-value-test
  (is (= (get-value solvedBoard 0 0) 5))
  (is (= (get-value solvedBoard 4 3) 8))
  (is (= (get-value solvedBoard 3 4) 6))
  )

(deftest board-pos-to-cell-pos-test
  (is (= (board-pos-to-cell-pos 1 1) [0 0]))
  (is (= (board-pos-to-cell-pos 2 7) [0 2]))
  (is (= (board-pos-to-cell-pos 3 4) [1 1]))
  (is (= (board-pos-to-cell-pos 7 7) [2 2]))
  )

(deftest board-pos-to-cell-ix-test
  (is (= (board-pos-to-cell-ix 1 1) 0))
  (is (= (board-pos-to-cell-ix 3 4) 4))
  (is (= (board-pos-to-cell-ix 6 2) 6))
  (is (= (board-pos-to-cell-ix 7 7) 8))
  )

(deftest board-pos-to-value-ix-test
  (is (= (board-pos-to-value-ix 1 1) 4))
  (is (= (board-pos-to-value-ix 2 2) 8))
  (is (= (board-pos-to-value-ix 3 3) 0))
  (is (= (board-pos-to-value-ix 4 4) 4))
  (is (= (board-pos-to-value-ix 5 5) 8))
  (is (= (board-pos-to-value-ix 6 6) 0))
  (is (= (board-pos-to-value-ix 8 8) 8))
  )

(deftest set-value-in-cell-test
  (is (= (set-value-in-cell [0 0 0 0 0 0] 0 1) [1 0 0 0 0 0]))
  (is (= (set-value-in-cell [0 0 0 0 0 0] 2 1) [0 0 1 0 0 0]))
  (is (= (set-value-in-cell [0 0 0 0 0 0] 5 1) [0 0 0 0 0 1]))
  )

(deftest set-cell-in-board-test
  (let [before-board [[1 2 3 4 5 6 7 8 9]
                      [11 12 13 14 15 16 17 18 19]
                      [21 22 23 24 25 26 27 28 29]
                      [31 32 33 34 35 36 37 38 39]
                      [41 42 43 44 45 46 47 48 49]]
        cell [91 92 93 94 95 96 97 98 99]
        after-board0 [cell
                      [11 12 13 14 15 16 17 18 19]
                      [21 22 23 24 25 26 27 28 29]
                      [31 32 33 34 35 36 37 38 39]
                      [41 42 43 44 45 46 47 48 49]]
        after-board1 [[1 2 3 4 5 6 7 8 9]
                      [11 12 13 14 15 16 17 18 19]
                      [21 22 23 24 25 26 27 28 29]
                      [31 32 33 34 35 36 37 38 39]
                      cell]
        after-board2 [[1 2 3 4 5 6 7 8 9]
                      [11 12 13 14 15 16 17 18 19]
                      [21 22 23 24 25 26 27 28 29]
                      cell
                      [41 42 43 44 45 46 47 48 49]]]
    (is (= (set-cell-in-board before-board 0 cell) after-board0));first
    (is (= (set-cell-in-board before-board 4 cell) after-board1));last
    (is (= (set-cell-in-board before-board 3 cell) after-board2));middle
    ))
;------------------------------

(deftest abs-test
  (is (= (absolute 4) 4))
  (is (= (absolute -4) 4))
  (is (= (absolute -100) 100))
  )
