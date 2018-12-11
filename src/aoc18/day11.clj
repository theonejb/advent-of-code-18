(ns aoc18.day11
  (:require [clojure.string :refer [join]]))

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

(defn print-grid [grid]
  (println (map (fn [row]
                  (str (vec (map #(format "%-4s" %1) row)) "\n")) grid)))

(defn coord->matrix-coords [[x y] matrix-size]
  (for [y (range y (+ matrix-size y))
        x (range x (+ matrix-size x))]
    [x y]))

(defn coord->matrix-edge-coords [[x-orig y-orig] matrix-size]
  (filter (fn [[x y]]
            (or (= x (dec (+ x-orig matrix-size)))
                (= y (dec (+ y-orig matrix-size)))))
          (coord->matrix-coords [x-orig y-orig] matrix-size)))

(defn print-matrix-at-coords [[x-start y-start] grid matrix-size]
  (let [matrix-coords (coord->matrix-coords [x-start y-start] matrix-size)]
    (doseq [[x y] matrix-coords]
      (if (and (= x x-start) (not (= y y-start)))
        (println))
      (print (format "%-4s" (get-in grid [y x]))))))

(defn power-level-at-matrix [[x y] grid matrix-size]
  (let [matrix-coords (coord->matrix-coords [x y] matrix-size)]
    (reduce (fn [sum [x y]]
              (+ sum (get-in grid [y x]))) 0 matrix-coords)))

(defn power-level-at-matrix-with-cache [[x-orig y-orig] grid matrix-size cache]
  (let [matrix-coords (coord->matrix-edge-coords [x-orig y-orig] matrix-size)
        cached-power-level (cache [x-orig y-orig])]
    (reduce (fn [sum [x y]]
              (+ sum (get-in grid [y x]))) cached-power-level matrix-coords)))

(defn grid->matrix-powers [grid matrix-size]
  (let [x-max (inc (- 300 matrix-size))
        y-max (inc (- 300 matrix-size))]
    (for [x (range x-max)
          y (range y-max)]
      {:x     x
       :y     y
       :power (power-level-at-matrix [x y] grid matrix-size)
       :size  matrix-size})))

(defn grid->matrix-powers-with-cache [grid matrix-size cache]
  (let [x-max (inc (- 300 matrix-size))
        y-max (inc (- 300 matrix-size))

        cache-map (reduce (fn [map {:keys [x y power]}]
                            (assoc map [x y] power)) {} cache)]
    (for [x (range x-max)
          y (range y-max)]
      {:x     x
       :y     y
       :power (power-level-at-matrix-with-cache [x y] grid matrix-size cache-map)
       :size  matrix-size})))

(defn find-largest-power-matrix
  ([grid-serial]
   (apply max-key :power (grid->matrix-powers (get-filled-grid 300 300 grid-serial) 3))))

(defn grid->summed-table [grid]
  (let [height (count grid)
        width (count (first grid))
        summed-table (vec (repeat height (vec (repeat width 0))))
        summed-table (assoc-in summed-table [0 0] (get-in grid [0 0]))]
    (reduce (fn [table {:keys [x y sum] :as t}]
              (println (str "[" x ", " y "], "))
              (assoc-in table [y x] sum))
            summed-table
            (for [y (range height)
                  x (range width)]
              {:x   x
               :y   y
               :sum (reduce (fn [sum [x y]]
                              (+ sum (get-in grid [y x])))
                            0
                            (for [x1 (range (inc x))
                                  y1 (range (inc y))]
                              [x1 y1]))}))))

(defn get-value-from-summed-table [table {:keys [x y]}]
  (get-in table [y x]))

(defn summed-table->matrix-sum [summed-table [x-orig y-orig] matrix-size]
  (let [bottom-right {:x (+ x-orig (dec matrix-size))
                      :y (+ x-orig (dec matrix-size))}
        bottom-left {:x x-orig
                     :y (+ x-orig (dec matrix-size))}
        top-right {:x (+ x-orig (dec matrix-size))
                   :y y-orig}
        top-left {:x x-orig
                  :y y-orig}
        get-val (partial get-value-from-summed-table summed-table)]
    (- (+ (get-val bottom-right) (get-val top-left)) (get-val top-right) (get-val bottom-left))))

(defn grid->matrix-powers-with-summed-table [matrix-size summed-table]
  (let [x-max (inc (- 300 matrix-size))
        y-max (inc (- 300 matrix-size))]
    (for [x (range x-max)
          y (range y-max)]
      {:x     x
       :y     y
       :power (summed-table->matrix-sum summed-table [x y] matrix-size)
       :size  matrix-size})))

(defn find-largest-power-matrix-and-size [grid-serial]
  (let [grid (get-filled-grid 300 300 grid-serial)
        st (grid->summed-table grid)]
    (loop [matrix-size 1
           previous-powers (grid->matrix-powers-with-summed-table matrix-size st)
           previous-max (apply max-key :power previous-powers)]
      (println "Size " matrix-size)
      (println "Max " previous-max)
      (if (= matrix-size 301)
        previous-max
        (let [matrix-size (inc matrix-size)
              current-powers (grid->matrix-powers-with-summed-table matrix-size st)
              current-powers-max (apply max-key :power current-powers)
              new-max (max-key :power previous-max current-powers-max)]
          (recur matrix-size current-powers new-max))))))