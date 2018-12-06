(ns aoc18.day3
  (:require [clojure.string :refer [split-lines join]]
            [clojure.set :refer [difference]]))

(defn get-input [file-name] (split-lines (slurp file-name)))

(defn get-coords-from-input [line]
  (let [[_ id xs ys ws hs] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
        x (Integer/parseInt xs)
        y (Integer/parseInt ys)
        w (Integer/parseInt ws)
        h (Integer/parseInt hs)]
    [id x y (- (+ x w) 1) (- (+ y h) 1)]))
(defn get-rectangles [input]
  (map get-coords-from-input input))

(defn get-grid [x y] (vec (repeat y (vec (repeat x [])))))

(defn add-rect-to-grid [grid [id x1 y1 x2 y2]]
  (map-indexed (fn [y row]
                 (if (or (< y y1) (> y y2))
                   row
                   (map-indexed (fn [x col]
                                  (if (or (< x x1) (> x x2))
                                    col
                                    (conj col id))) row))) grid))

(defn find-common-area-in-grid [grid]
  (reduce + 0 (flatten (map (fn [row]
                              (map (fn [col]
                                     (if (> (count col) 1)
                                       1
                                       0)) row)) grid))))

(defn find-overlapping-plan-ids [grid]
  (set (flatten (map (fn [row]
                       (map (fn [col]
                              (if (> (count col) 1)
                                col
                                [])) row)) grid))))

(defn get-all-plan-ids [rects] (set (map first rects)))

(defn find-non-overlapping-plan-ids [rects grid]
  (let [overlapping-ids (find-overlapping-plan-ids grid)
        all-ids (get-all-plan-ids rects)]
    (difference all-ids overlapping-ids)))

(defn print-grid [grid spaces]
  (map (fn [row]
         (println (map (fn [col]
                         (format (str "%" spaces "s") (join "," col))) row))) grid))

(defn day3-main [input-fname [gw gh]]
  (let [grid (get-grid gw gh)
        input (get-input input-fname)
        rectangles (get-rectangles input)
        filled-grid (reduce add-rect-to-grid grid rectangles)]
    (println "Common area: " (find-common-area-in-grid filled-grid))))

(defn day3-main-2 [input-fname [gw gh]]
  (let [grid (get-grid gw gh)
        input (get-input input-fname)
        rectangles (get-rectangles input)
        filled-grid (reduce add-rect-to-grid grid rectangles)]
    (println "Unique plans: " (find-non-overlapping-plan-ids rectangles filled-grid))))