(ns aoc18.day10
  (:require [clojure.string :as str]))

(defn get-input [fname] (str/split-lines (slurp fname)))
(defn parse-input-line [line]
  (let [[_ xs ys vxs vys] (re-find #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>" line)]
    {:x  (Integer/parseInt xs)
     :y  (Integer/parseInt ys)
     :xs (Integer/parseInt vxs)
     :ys (Integer/parseInt vys)
     }))

(def input (map parse-input-line (get-input "day10.small.txt")))
(def big-input (map parse-input-line (get-input "day10.txt")))

(defn lights-at-time [input time]
  (map (fn [{:keys [x y xs ys]}]
         {:x (+ x (* xs time)) :y (+ y (* ys time))})
       input))

(defn coord-pair-to-set-entry
  ([{:keys [x y]}]
   (coord-pair-to-set-entry x y))

  ([x y]
   (str x \, y)))

(defn get-bounds-for-lights [lights buffer]
  (let [x-min (- (:x (apply min-key :x lights)) buffer)
        x-max (+ buffer (:x (apply max-key :x lights)))

        y-min (- (:y (apply min-key :y lights)) buffer)
        y-max (+ buffer (:y (apply max-key :y lights)))]
    {:x-min x-min
     :x-max x-max
     :y-min y-min
     :y-max y-max}))

(defn print-lights [lights buffer scale]
  (let [lights (map (fn [{:keys [x y]}]
                      {:x (Math/round (float (/ x scale)))
                       :y (Math/round (float (/ y scale)))})
                    lights)
        {:keys [x-min x-max y-min y-max]} (get-bounds-for-lights lights buffer)
        coords-set (set (map coord-pair-to-set-entry lights))]
    (doseq [y (range y-min (inc y-max))
            x (range x-min (inc x-max))]
      (do
        (if (and (= x-min x) (not (= y y-min)))
          (println))
        (if (coords-set (coord-pair-to-set-entry x y))
          (print \#)
          (print \.))))
    (println)))

(defn print-lights-at-time [input time scale]
  (print-lights (lights-at-time input time) 2 scale))