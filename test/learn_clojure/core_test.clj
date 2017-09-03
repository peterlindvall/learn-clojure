(ns learn-clojure.core-test
  (:require [clojure.test :refer :all]
            [learn-clojure.core :refer :all]
            [learn-clojure.util :refer :all]))

(deftest isCellComplete-test
  (let [board [[1 2 3 4 5 6 7 8 9] ;r0 c0 (correct)
              [1 2 3 4 5 6 7 8 ] ;r0 c1 (value missing)
              [1 2 0 4 5 6 7 8 9] ;r0 c2 (invalid value, to small)
              [1 2 3 4 5 6 7 8 10] ;r1 c0 (invalid value, to large)
              [2 2 3 4 5 6 7 8 9] ;r1 c1 (has duplicate value)
              [1 2 3 4 5 6 7 8 9 10] ;r1 c2 (too many values)
              [1 2 3 4 5 6 7 8 9] ;r2 c0
              [3 2 3 4 5 6 7 8 9] ;r2 c1
              [3 3 3 4 5 6 7 8 9]]] ;r2 c2

    (is (isCellComplete? (nth board 0)))
    (is (not (isCellComplete? (nth board 1))))
    (is (not (isCellComplete? (nth board 2))))
    (is (not (isCellComplete? (nth board 3))))
    (is (not (isCellComplete? (nth board 4))))
    (is (not (isCellComplete? (nth board 5))))))

(deftest getCell-test
  (let [board [[1 2 3 4 5 6 7 8 9] ;r0 c0
               [1 2 3 4 5 6 7 8 ] ;r0 c1
               [1 2 0 4 5 6 7 8 9] ;r0 c2
               [1 2 3 4 5 6 7 8 10] ;r1 c0
               [2 2 3 4 5 6 7 8 9] ;r1 c1
               [3 3 3 4 5 6 7 8 9]]] ;r1 c2

    (is (= (getCell 0 0 board) [1 2 3 4 5 6 7 8 9]))
    (is (= (getCell 1 1 board) [2 2 3 4 5 6 7 8 9]))
    (is (= (getCell 1 2 board) [3 3 3 4 5 6 7 8 9]))
    ))

(deftest getCell-test
  (let [cell00 [1 2 3 4 5 6 7 8 9]
        cell11 [2 2 3 4 5 6 7 8 9]
        cell12 [3 3 3 4 5 6 7 8 9]
        board [cell00                          ;r0 c0
               [1 2 3 4 5 6 7 8]                            ;r0 c1
               [1 2 0 4 5 6 7 8 9]                          ;r0 c2
               [1 2 3 4 5 6 7 8 10]                         ;r1 c0
               cell11                          ;r1 c1
               cell12]]                        ;r1 c2

    (is (= (getCell 0 0 board) cell00))
    (is (= (getCell 1 1 board) cell11))
    (is (= (getCell 1 2 board) cell12))
    ))