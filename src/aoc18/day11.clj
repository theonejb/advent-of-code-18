(ns aoc18.day11
  (:require [clojure.string :refer [join]]
            [clojure.pprint :refer [pprint]]))

(defn get-power-for-cell-at-coords [[x y] grid-serial]
  (let [x (inc x)                                           ; Because coords are ZERO based
        y (inc y)

        rack-id (+ x 10)
        power-level (* rack-id y)
        power-level (+ power-level grid-serial)
        power-level (* power-level rack-id)
        power-level (mod (int (/ power-level 100)) 10)]
    (- power-level 5)))

(defn new-grid [height width]
  (vec (repeat height (vec (repeat width 0)))))

(defn get-coords [x-max y-max]
  (for [y (range y-max)
        x (range x-max)]
    [x y]))

(defn get-filled-grid [height width grid-serial]
  (let [grid (new-grid height width)
        coords (get-coords height width)]
    (reduce (fn [grid [x y]]
              (assoc-in grid [y x] (get-power-for-cell-at-coords [x y] grid-serial)))
            grid coords)))

(defn get-value-from-table-at-coord [table [x y]]
  (get-in table [y x] 0))

(defn get-sum-to-add-at-coord [sat [x y]]
  ; Equation is I(x, y - 1) + I(x - 1, y) - I(x - 1, y - 1) where I is value of SAT at coords
  (let [a (get-value-from-table-at-coord sat [x (dec y)])
        b (get-value-from-table-at-coord sat [(dec x) y])
        c (get-value-from-table-at-coord sat [(dec x) (dec y)])]
    (- (+ a b) c)))

(defn grid->summed-area-table [grid]
  (let [height (count grid)
        width (count (first grid))
        sat (vec (repeat height (vec (repeat width 0))))
        sat (assoc-in sat [0 0] (get-in grid [0 0]))
        coords (get-coords width height)]
    (reduce (fn [sat [x y]]
              (let [value-to-add (get-sum-to-add-at-coord sat [x y])
                    grid-value-at-coord (get-in grid [y x])]
                (assoc-in sat [y x] (+ grid-value-at-coord value-to-add))))
            sat
            coords)))

(defn sat->sum-at-coord
  ([sat [xa ya] [xb yb]]
    ; [xa ya] is the top left edge and [xb yb] is the bottom right
    ; The formula used is:
    ; I(xb, yb) - I(xa - 1, yb) - I(xb, ya - 1) + I(xa - 1, ya - 1)
   (let [a (get-value-from-table-at-coord sat [xb yb])
         b (get-value-from-table-at-coord sat [(dec xa) yb])
         c (get-value-from-table-at-coord sat [xb (dec ya)])
         d (get-value-from-table-at-coord sat [(dec xa) (dec ya)])]
     (+ (- a b c) d))))

(defn sat->sum-for-matrix [sat [x y] width height]
  ; [x y] is the coordinate for the top left edge
  (sat->sum-at-coord sat
                     [x y]
                     [(dec (+ x width)) (dec (+ y height))]))

(defn grid-sat->matrix-power-at-coord [sat [x y] matrix-size]
  {:x     x
   :y     y
   :size  matrix-size
   :power (sat->sum-for-matrix sat [x y] matrix-size matrix-size)})

(defn max-power-at-matrix-size [sat matrix-size]
  (let [height (count sat)
        width (count (first sat))
        all-coords (get-coords (- width matrix-size) (- height matrix-size))
        max-power (apply max-key :power
                         (map #(grid-sat->matrix-power-at-coord sat %1 matrix-size) all-coords))]
    (assoc (assoc max-power :x (inc (:x max-power))) :y (inc (:y max-power)))))

(defmacro get-time [expr]
  `(let [start# (. System nanoTime)
         ret# ~expr
         spent-time# (/ (double (- (. System nanoTime) start#)) 1000000.0)]
     [spent-time# ret#]))

(defn find-max-power [sat]
  (apply max-key :power (for [matrix-size (range 1 300)]
                          (max-power-at-matrix-size sat matrix-size))))

(defn part-1 []
  (let [grid (get-filled-grid 300 300 2187)
        grid-sat (grid->summed-area-table grid)
        [time-spent result] (get-time (max-power-at-matrix-size grid-sat 3))]
    (println "Result " result)
    (println "Time spent " (/ time-spent 1000) "s")))

(defn part-2 []
  (let [grid (get-filled-grid 300 300 2187)
        grid-sat (grid->summed-area-table grid)
        [time-spent result] (get-time (find-max-power grid-sat))]
    (println "Result " result)
    (println "Time spent " (/ time-spent 1000) "s")))
