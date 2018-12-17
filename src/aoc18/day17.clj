(ns aoc18.day17
  (:require [clojure.string :as str]))

(defn read-input [fname] (str/split-lines (slurp fname)))
(defn parse-input-line [line]
  (let [numbers-str (re-seq #"\d+" line)
        numbers (map #(Integer/parseInt %1) numbers-str)]
    (if (= \x (first line))
      {:x       (nth numbers 0)
       :y-start (nth numbers 1)
       :y-end   (nth numbers 2)}
      {:y       (nth numbers 0)
       :x-start (nth numbers 1)
       :x-end   (nth numbers 2)})))
(defn fname->clay-regions [fname]
  (map parse-input-line (read-input fname)))

(defn clay-regions->bounds [regions]
  {:x-min (apply min (remove nil? (concat (map :x regions) (map :x-star regions))))
   :x-max (apply max (remove nil? (concat (map :x regions) (map :x-end regions))))

   :y-min (apply min (remove nil? (concat (map :y regions) (map :y-start regions))))
   :y-max (apply max (remove nil? (concat (map :y regions) (map :y-end regions))))})

(defn region->coords [{:keys [x y x-start x-end y-start y-end]}]
  (let [x-start (or x-start x)
        x-end (or x-end x)
        y-start (or y-start y)
        y-end (or y-end y)]
    (for [y (range y-start (inc y-end))
          x (range x-start (inc x-end))]
      {:x x :y y})))

(defn clay-regions->coords [regions]
  (apply concat (map region->coords regions)))

(defn sort-coords [coords]
  (sort-by :y (sort-by :x coords)))

(defn normalize-coords [{:keys [x-min y-min]} coords]
  (map (fn [{:keys [x y]}]
         {:x (- x x-min) :y (- y y-min)}) coords))

(defn clay-regions->grid [regions]
  (let [{:keys [x-min x-max y-min y-max] :as bounds} (clay-regions->bounds regions)
        x-min (- x-min 2)                                   ; buffer of 2 on the sides
        x-max (+ x-max 3)                                   ; buffer of 2 + 1 for Zero indexing
        y-max (inc y-max)                                   ; Zero indexing
        y-min 0                                             ; because we have our fountain at 0
        width (- x-max x-min)
        height (- y-max y-min)

        coords (clay-regions->coords regions)
        coords (sort-coords coords)
        coords (normalize-coords {:x-min x-min :y-min y-min} coords)

        grid (vec (repeat height (vec (repeat width \.))))
        ]
    (assoc-in (reduce (fn [grid {:keys [x y]}]
                        (assoc-in grid [y x] \#))
                      grid
                      coords) [0 (- 500 x-min)] \+)))

(def RESET "\033[0m")

(def BGRED "\033[41m")
(def BGGREEN "\033[42m")

(def WHITE "\033[0;37m")
(def RED "\033[0;31m")
(def REDBRIGHT "\033[0;91m")

(defn print-grid-char [char]
  (if (= \# char) (str RED char RESET) char))

(defn print-grid [grid]
  (println (str/join \newline (map (fn [row]
                                     (str/join (map print-grid-char row))) grid))))

(defn fname->grid [fname]
  (-> fname
      fname->clay-regions
      clay-regions->grid))