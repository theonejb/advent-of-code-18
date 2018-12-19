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
  {:x-min-orig (apply min (remove nil? (concat (map :x regions) (map :x-star regions))))
   :x-max-orig (apply max (remove nil? (concat (map :x regions) (map :x-end regions))))

   :y-min-orig (apply min (remove nil? (concat (map :y regions) (map :y-start regions))))
   :y-max-orig (apply max (remove nil? (concat (map :y regions) (map :y-end regions))))

   ; Buffer on both sides and zero indexing fix. Plus y=0 because fountain at (500, 0)
   :x-min      (- (apply min (remove nil? (concat (map :x regions) (map :x-star regions)))) 2)
   :x-max      (+ (apply max (remove nil? (concat (map :x regions) (map :x-end regions)))) 3)

   :y-min      0
   :y-max      (inc (apply max (remove nil? (concat (map :y regions) (map :y-end regions)))))})

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
(def RED "\033[0;31m")
(def BLUE "\033[0;34m")

(def space \.)
(def wall \#)
(def water-moving \|)
(def water-standing \~)

(defn print-grid-char [char]
  (condp = char
    wall (str RED char RESET)
    water-moving (str BLUE char RESET)
    water-standing (str BLUE char RESET)
    char))

(defn print-state [{:keys [grid]}]
  (println (str/join \newline (map (fn [row]
                                     (str/join (map print-grid-char row))) grid))))

(defn fname->state [fname]
  (let [regions (fname->clay-regions fname)
        bounds (clay-regions->bounds regions)
        grid (clay-regions->grid regions)]
    {:grid grid :bounds bounds}))

(defn drop-water
  ([grid pos] (drop-water grid pos []))
  ([grid {:keys [x y] :as water-position} trail]
   (let [grid-height (count grid)
         x x
         y (inc y)
         next-pos {:x x :y y}
         next-pos-char (get-in grid [y x])]
     (cond
       (>= y grid-height) {:grid grid :position water-position :reason :bounds :trail trail}
       (not (= space next-pos-char)) {:grid grid :position water-position :reason :not-empty :trail trail}
       :default (recur (assoc-in grid [y x] water-moving) next-pos (conj trail next-pos))))))

(defn whats-to-the-left [grid {:keys [x y] :as pos}]
  (let [pos-char (get-in grid [y x])
        pos-below-char (get-in grid [(inc y) x])
        next-pos {:x (dec x) :y y}]
    (cond
      (= pos-char wall) {:what :wall :position pos}
      (and (= pos-char space) (= pos-below-char space)) {:what :drop :position pos}

      (> x (count (first grid))) {:what :dragons :position pos}
      (> y (count grid)) {:what :dragons :position pos}
      (< x 0) {:what :dragons :position pos}
      (< y 0) {:what :dragons :position pos}

      :default (recur grid next-pos))))

(defn whats-to-the-right [grid {:keys [x y] :as pos}]
  (let [pos-char (get-in grid [y x])
        pos-below-char (get-in grid [(inc y) x])
        next-pos {:x (inc x) :y y}]
    (cond
      (= pos-char wall) {:what :wall :position pos}
      (and (= pos-char space) (= pos-below-char space)) {:what :drop :position pos}

      (> x (count (first grid))) {:what :dragons :position pos}
      (> y (count grid)) {:what :dragons :position pos}
      (< x 0) {:what :dragons :position pos}
      (< y 0) {:what :dragons :position pos}

      :default (recur grid next-pos))))

(defn fill-with-standing-water [grid {:keys [x y]} end-x]       ; end-x is exclusive
  (if (= x end-x)
    grid
    (recur (assoc-in grid [y x] water-standing) {:x (inc x) :y y} end-x)))