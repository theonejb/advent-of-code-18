(ns aoc18.day14
  (:require [clojure.string :as str]))

(def initial-state {:board [3 7] :elf1 0 :elf2 1})

(defn print-game [{:keys [board elf1 elf2]}]
  (println (str/join " " (map-indexed (fn [index recipe]
                                        (condp = index
                                          elf1 (str "(" recipe ")")
                                          elf2 (str "[" recipe "]")
                                          (str recipe)))
                                      board))))

(defn get-current-recipes [{:keys [board elf1 elf2]}]
  (map #(nth board %1) [elf1 elf2]))

(defn get-new-recipes [game]
  (let [current-recipes (get-current-recipes game)
        current-score (apply + current-recipes)
        new-recipes (map (comp #(Integer/parseInt %1) str) (str current-score))]
    new-recipes))

(defn add-new-recipes-to-board [{:keys [board] :as game}]
  (assoc game :board (concat board (get-new-recipes game))))

(defn advance-elfs-to-next-recipe [{:keys [board elf1 elf2] :as game}]
  (let [board-length (count board)

        elf1-score (nth board elf1)
        elf1-next-index (mod (+ elf1 (inc elf1-score)) board-length)

        elf2-score (nth board elf2)
        elf2-next-index (mod (+ elf2 (inc elf2-score)) board-length)]
    (assoc game :elf1 elf1-next-index :elf2 elf2-next-index)))

(defn tick-until-board-length [game max-board-length]
  (loop [{:keys [board] :as game} game
         i 0]
    (println "Iteration " i)
    (if (> (count board) max-board-length)
      game
      (recur (-> game add-new-recipes-to-board advance-elfs-to-next-recipe) (inc i)))))

(defn get-ten-recipes-after-recipes [game recipes-to-make]
  (let [game (tick-until-board-length game (+ 10 recipes-to-make))]
    (str/join (take 10 (drop recipes-to-make (:board game))))))