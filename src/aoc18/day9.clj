(ns aoc18.day9
  (:require [clojure.string :as str]))

(defn get-inputs [fname] (str/split-lines (slurp fname)))
(defn parse-input-line [line]
  (let [[_ players marbles] (re-find #"(\d+) players; last marble is worth (\d+) points" line)]
    {:players (Integer/parseInt players) :marbles (Integer/parseInt marbles)}))

(def inputs (map parse-input-line (get-inputs "day9.small.txt")))

(defn move-clockwise [board from moves]
  (mod (+ moves from) board))

(defn move-anti-clockwise [board from moves]
  (mod (- from moves) board))

(defn next-move [next-marble current-marble-index board]
  (if (= 0 (mod next-marble 23))
    {:action :remove
     :index  (move-anti-clockwise board current-marble-index 7)}
    {:action :add
     :index  (move-clockwise board current-marble-index 1)}))

; Called before the board is changed so we get the correct marble to score with
(defn get-score-for-move [{:keys [action index]} board marble]
  (if (= action :remove)
    (println (- marble (nth board index)))
    0)
  (if (= action :remove)
    (+ marble (nth board index))
    0))

(defn perform-move-on-board [{:keys [action index]} next-marble board]
  (condp = action
    :add (let [[board-before board-after] (split-at (inc index) board)]
           (concat board-before [next-marble] board-after))
    :remove (let [[board-before board-after] (split-at (inc index) board)]
              (concat (drop-last 1 board-before) board-after))))

(defn get-current-marble-index-after-move [{:keys [action index]} board]
  (condp = action
    :add (inc index)
    :remove (mod index (count board))))

(defn print-game-state [board player current-marble]
  (println "[" player "] "
           (str/join " " (map-indexed (fn [index marble]
                                        (if (= index current-marble)
                                          (format "(%d)" marble)
                                          (format "%d" marble))) board))))

(defmacro get-time [expr]
  `(let [start# (. System nanoTime)
         ret# ~expr
         spent-time# (/ (double (- (. System nanoTime) start#)) 1000000.0)]
     [spent-time# ret#]))

(defn game-loop [{:keys [players marbles]}]
  (loop [board [0]
         board-size 1
         [player-turn & rest-players] (cycle (range 1 (inc players)))
         next-marble 1
         current-marble-index 0
         scores {}
         times {:next-move            0
                :score-for-move       0
                :board                0
                :current-marble-index 0
                :previous-score       0
                :new-score            0
                :scores               0
                :count                0}]
    (if (> next-marble marbles)
      [scores times]
      (let [[t-nm next-move] (get-time (next-move next-marble current-marble-index board))
            [t-sm score-for-move] (get-time (get-score-for-move next-move board next-marble))
            [t-b board] (get-time (perform-move-on-board next-move next-marble board))
            [t-mi current-marble-index] (get-time (get-current-marble-index-after-move next-move board))
            [t-psc prev-score-for-player] (get-time (scores player-turn 0))
            [t-nsc new-score-for-player] (get-time (+ score-for-move prev-score-for-player))
            [t-s scores] (get-time (assoc scores player-turn new-score-for-player))
            times (assoc times :next-move (+ (times :next-move) t-nm)
                               :score-for-move (+ (times :score-for-move) t-sm)
                               :board (+ (times :board) t-b)
                               :current-marble-index (+ (times :current-marble-index) t-mi)
                               :previous-score (+ (times :previous-score) t-psc)
                               :new-score (+ (times :new-score) t-nsc)
                               :scores (+ (times :scores) t-s)
                               :count (inc (times :count)))]
        (print \.)
        (recur board
               (if (= (:action next-move) :add)
                 (inc board-size)
                 (dec board-size))
               rest-players (inc next-marble)
               current-marble-index scores times)))))

(defn run-games [inputs]
  (for [game inputs]
    (let [[game-time [scores times]] (get-time (game-loop game))
          sorted-scores (sort-by second scores)
          avg-times (sort-by :avg-time (map (fn [[label total-time]] {:label label :avg-time (/ total-time (:count times))}) times))]
      (println "Time: " (/ game-time 1000) "s")
      (println "Game: " game)
      (println "Winner: " (last sorted-scores))
      (println "Times:")
      (doseq [{:keys [label avg-time]} avg-times]
        (println "\t" label " -> " avg-time "ms")))))