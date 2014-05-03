(ns game-of-life.core
  (:require [clojure.set :as set]))
 
(def example-world #{[2 1] [-1 2] [1 4]})
 
(defn neighbours [[x y]] 
  #{[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
    [(dec x) y]                   [(inc x) y]
    [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]})

(defn count-live-neighbours [world coords]
  (count (set/intersection world (neighbours coords))))
 
(defn survives? [world coords]
  (let [n (count-live-neighbours world coords)]
    (or 
     (and (contains? world coords) (or (= n 2) (= n 3)))
     (= n 3))))
 
(defn extents [world]
  (let [[min-x max-x min-y max-y] (->> [(map first world) (map last world)]
                                       (map (juxt #(apply min %) #(apply max %)))
                                       (apply concat))]
    [(range (dec min-x) (+ 2 max-x))
     (range (dec min-y) (+ 2 max-y))]))
 
(defn next-gen [world]
  (set (let [[xs ys] (extents world)]
         (for [x xs y ys :when (survives? world [x y])]
           [x y]))))
 
(def expected [#{[1 2] [2 2] [3 2]} 
               #{[2 1] [2 2] [2 3]} 
               #{[1 2] [2 2] [3 2]} 
               #{[2 1] [2 2] [2 3]} 
               #{[1 2] [2 2] [3 2]}])
 
(def actual (take 5 (iterate next-gen #{[1 2] [2 2] [3 2]})))
 
(= expected actual)
