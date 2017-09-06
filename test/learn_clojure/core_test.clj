(ns learn-clojure.core-test
  (:require [clojure.test :refer :all]
            [learn-clojure.core :refer :all]
            [learn-clojure.util :refer :all]))


(def solvedBoard [[5 3 4 6 7 2 1 9 8]
                  [6 7 8 1 9 5 3 4 2]
                  [9 1 2 3 4 8 5 6 7]
                  [8 5 9 4 2 6 7 1 3]
                  [7 6 1 8 5 3 9 2 4]
                  [4 2 3 7 9 1 8 5 6]
                  [9 6 1 2 8 7 3 4 5]
                  [5 3 7 4 1 9 2 8 6]
                  [2 8 4 6 3 5 1 7 9]])


(deftest isCellComplete-test
  (let [board [[1 2 3 4 5 6 7 8 9]                          ;r0 c0 (correct)
               [1 2 3 4 5 6 7 8]                            ;r0 c1 (value missing)
               [1 2 0 4 5 6 7 8 9]                          ;r0 c2 (invalid value, to small)
               [1 2 3 4 5 6 7 8 10]                         ;r1 c0 (invalid value, to large)
               [2 2 3 4 5 6 7 8 9]                          ;r1 c1 (has duplicate value)
               [1 2 3 4 5 6 7 8 9 10]                       ;r1 c2 (too many values)
               [1 2 3 4 5 6 7 8 9]                          ;r2 c0
               [3 2 3 4 5 6 7 8 9]                          ;r2 c1
               [3 3 3 4 5 6 7 8 9]]]                        ;r2 c2

    (is (isCellComplete? (nth board 0)))
    (is (not (isCellComplete? (nth board 1))))
    (is (not (isCellComplete? (nth board 2))))
    (is (not (isCellComplete? (nth board 3))))
    (is (not (isCellComplete? (nth board 4))))
    (is (not (isCellComplete? (nth board 5))))))

(deftest getCell-test
  (let [board [[1 2 3 4 5 6 7 8 9]                          ;r0 c0
               [1 2 3 4 5 6 7 8]                            ;r0 c1
               [1 2 0 4 5 6 7 8 9]                          ;r0 c2
               [1 2 3 4 5 6 7 8 10]                         ;r1 c0
               [2 2 3 4 5 6 7 8 9]                          ;r1 c1
               [3 3 3 4 5 6 7 8 9]]]                        ;r1 c2

    (is (= (getCell 0 0 board) [1 2 3 4 5 6 7 8 9]))
    (is (= (getCell 1 1 board) [2 2 3 4 5 6 7 8 9]))
    (is (= (getCell 1 2 board) [3 3 3 4 5 6 7 8 9]))
    ))

(deftest getCell-test
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
    (is (= (getCell 0 0 board) cell00))
    (is (= (getCell 0 1 board) cell01))
    (is (= (getCell 1 2 board) cell12))
    (is (= (getCell 2 2 board) cell22))
    ))


(deftest getRow-test
  (let [board [[1 2 3 4 5 6 7 8 9]
               [11 12 13 14 15 16 17 18 19]
               [21 22 23 24 25 26 27 28 29]
               [31 32 33 34 35 36 37 38 39]
               [41 42 43 44 45 46 47 48 49]
               [51 52 53 54 55 56 57 58 59]
               [61 62 63 64 65 66 67 68 69]
               [71 72 73 74 75 76 77 78 79]
               [81 82 83 84 85 86 87 88 89]]]
    (is (= (getRow 0 board) [1 2 3 11 12 13 21 22 23]))
    (is (= (getRow 3 board) [31 32 33 41 42 43 51 52 53]))
    (is (= (getRow 8 board) [67 68 69 77 78 79 87 88 89]))
    ))

(deftest getColumn-test2
  (let [board solvedBoard
        col1 (getColumn 0 board)
        col4 (getColumn 3 board)
        col9 (getColumn 8 board)]
    (printBoard board)
    (is (= col1 [5 6 1 8 4 7 9 2 3]))
    (is (= col4 [6 1 3 7 8 9 5 4 2]))
    (is (= col9 [2 8 7 3 1 6 4 5 9]))
    ))

(deftest isColumnValid?-test
  (let [validBoard solvedBoard
        invalidBoard [[5 3 4 6 7 2 1 9 8]
                      [6 8 7 1 9 5 3 4 2]
                      [9 1 2 4 3 8 5 6 7]
                      [8 5 9 4 2 6 7 1 3]
                      [7 6 1 8 5 3 2 9 4]
                      [4 2 3 7 9 1 8 5 6]
                      [9 5 1 2 8 7 4 3 6]
                      [5 3 4 7 1 9 2 8 6]
                      [9 8 4 6 3 5 1 7 2]]
        ;col (getColumn 3 invalidBoard)
        ]
    ;(println "Invalid column? " col " #" (count col))
    (is (isColumnValid? 0 validBoard))
    (is (isColumnValid? 2 validBoard))
    (is (isColumnValid? 3 validBoard))
    (is (isColumnValid? 6 validBoard))
    (is (isColumnValid? 8 validBoard))
    (is (not (isColumnValid? 1 invalidBoard)))
    (is (not (isColumnValid? 3 invalidBoard)))
    (is (not (isColumnValid? 7 invalidBoard)))
    (is (not (isColumnValid? 8 invalidBoard)))
    ))

(deftest isRowValid?-test
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
    (println (getRow 5 invalidBoard))
    (is (isRowValid? 0 validBoard))
    (is (isRowValid? 2 validBoard))
    (is (isRowValid? 5 validBoard))
    (is (isRowValid? 8 validBoard))
    (is (not (isRowValid? 0 invalidBoard)))
    (is (not (isRowValid? 2 invalidBoard)))
    (is (not (isRowValid? 5 invalidBoard)))
    (is (not (isRowValid? 8 invalidBoard)))
    ))

(deftest removeBlanks-test
  (let [vectorWithZeroes [1 2 3 0 4 5 0]
        vectorWithoutZeroes (removeBlanks vectorWithZeroes)]
    (is (= vectorWithoutZeroes [1 2 3 4 5]))
    )
  )