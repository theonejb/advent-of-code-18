(ns aoc18.day13
  (:require [clojure.string :as str]
            [clojure.pprint :as pp])
  (:import (java.util.concurrent TimeUnit)))

(defn get-input [fname] (str/split-lines (slurp fname)))

(defn get-board-bounds [board]
  {:y-max (count board)
   :x-max (count (first board))})

(defn get-coords [{:keys [x-max y-max]}]
  (for [y (range y-max)
        x (range x-max)]
    {:x x :y y}))

(defn input->board [input]
  (vec (map (fn [row]
              (vec row)) input)))

(def DIRECTION-UP \^)
(def DIRECTION-RIGHT \>)
(def DIRECTION-DOWN \v)
(def DIRECTION-LEFT \<)
(def RAIL-VERTICAL \|)
(def RAIL-HORIZONTAL \-)
(def RAIL-CORNER-TOP-LEFT \/)
(def RAIL-CORNER-BOTTOM-RIGHT \/)
(def RAIL-CORNER-TOP-RIGHT \\)
(def RAIL-CORNER-BOTTOM-LEFT \\)

(def cart-symbol-to-track {DIRECTION-UP    RAIL-VERTICAL
                           DIRECTION-RIGHT RAIL-HORIZONTAL
                           DIRECTION-DOWN  RAIL-VERTICAL
                           DIRECTION-LEFT  RAIL-HORIZONTAL})
(def is-cart-symbol? cart-symbol-to-track)

(defn board->carts [board]
  (let [coords (-> board get-board-bounds get-coords)]
    (remove
      nil?
      (map
        (fn [{:keys [x y]}]
          (let [current-char (get-in board [y x])
                is-cart? (is-cart-symbol? current-char)]
            (if is-cart?
              {:x x :y y :direction current-char :moves (cycle [:left :straight :right])}
              nil)))
        coords))))

(defn remove-carts-from-board [board]
  (let [coords (-> board get-board-bounds get-coords)]
    (reduce
      (fn [board {:keys [x y]}]
        (let [board-at-coord (get-in board [y x])]
          (assoc-in board [y x] (cart-symbol-to-track board-at-coord board-at-coord))))
      board
      coords)))

;033[41m  BG reg
(def BGRED "\033[41m")
(def BGGREEN "\033[42m")

(def WHITE "\033[0;37m")
(def RED "\033[0;31m")
(def REDBRIGHT "\033[0;91m")

(defn add-carts-to-board [board carts]
  (reduce (fn [board {:keys [x y] :as cart}]
            (let [rail-at-cart-position (get-in board [y x])]
              (assoc-in board [y x] (str WHITE BGRED (:direction cart) "\033[0m")))) board carts))

(defn print-cart [cart]
  (println (assoc (select-keys cart [:x :y :direction]) :intersection-move (first (:moves cart)))))

(defn print-board [board]
  (dorun (map (fn [row]
                (print (str (str/join row) \newline))) board)))

(defn print-game-state [{:keys [board carts]}]
  (print-board (add-carts-to-board board carts))
  (dorun (map print-cart carts))
  )

(defn fname->initial-state [fname]
  (let [board (-> fname get-input input->board)
        carts (board->carts board)
        cleared-board (remove-carts-from-board board)]
    {:board cleared-board :carts carts}))

(defn rotate-cart [{:keys [direction moves] :as cart} rail]
  (cond

    (and (= rail RAIL-CORNER-TOP-LEFT) (= direction DIRECTION-UP))
    (assoc cart :direction DIRECTION-RIGHT)
    (and (= rail RAIL-CORNER-TOP-LEFT) (= direction DIRECTION-LEFT))
    (assoc cart :direction DIRECTION-DOWN)

    (and (= rail RAIL-CORNER-TOP-RIGHT) (= direction DIRECTION-RIGHT))
    (assoc cart :direction DIRECTION-DOWN)
    (and (= rail RAIL-CORNER-TOP-RIGHT) (= direction DIRECTION-UP))
    (assoc cart :direction DIRECTION-LEFT)

    (and (= rail RAIL-CORNER-BOTTOM-RIGHT) (= direction DIRECTION-DOWN))
    (assoc cart :direction DIRECTION-LEFT)
    (and (= rail RAIL-CORNER-BOTTOM-RIGHT) (= direction DIRECTION-RIGHT))
    (assoc cart :direction DIRECTION-UP)

    (and (= rail RAIL-CORNER-BOTTOM-LEFT) (= direction DIRECTION-LEFT))
    (assoc cart :direction DIRECTION-UP)
    (and (= rail RAIL-CORNER-BOTTOM-LEFT) (= direction DIRECTION-DOWN))
    (assoc cart :direction DIRECTION-RIGHT)

    :default
    (let [rotation-direction (first moves)]
      (assoc cart
        :direction (rotation-direction ({DIRECTION-UP    {:left     DIRECTION-LEFT
                                                          :straight DIRECTION-UP
                                                          :right    DIRECTION-RIGHT}
                                         DIRECTION-RIGHT {:left     DIRECTION-UP
                                                          :straight DIRECTION-RIGHT
                                                          :right    DIRECTION-DOWN}
                                         DIRECTION-DOWN  {:left     DIRECTION-RIGHT
                                                          :straight DIRECTION-DOWN
                                                          :right    DIRECTION-LEFT}
                                         DIRECTION-LEFT  {:left     DIRECTION-DOWN
                                                          :straight DIRECTION-LEFT
                                                          :right    DIRECTION-UP}}
                                         direction))
        :moves (rest moves)))
    ))

(defn update-cart-direction-on-moving-to-rail [cart rail]
  (condp = rail
    RAIL-VERTICAL cart
    RAIL-HORIZONTAL cart
    (rotate-cart cart rail)))

(def cart-move-functions
  {DIRECTION-UP    (fn [{:keys [x y] :as cart}] (assoc cart :y (dec y)))
   DIRECTION-RIGHT (fn [{:keys [x y] :as cart}] (assoc cart :x (inc x)))
   DIRECTION-DOWN  (fn [{:keys [x y] :as cart}] (assoc cart :y (inc y)))
   DIRECTION-LEFT  (fn [{:keys [x y] :as cart}] (assoc cart :x (dec x)))})

(defn move-cart [board {:keys [direction] :as cart}]
  (let [cart-at-new-location ((cart-move-functions direction) cart)
        {:keys [x y]} cart-at-new-location
        rail (get-in board [y x])
        cart-with-correct-direction (update-cart-direction-on-moving-to-rail cart-at-new-location rail)]
    cart-with-correct-direction))

(defn are-carts-colliding [{:keys [x y]} {x1 :x y1 :y}]
  (and (= x x1) (= y y1)))

(defn get-colliding-carts [carts cart-to-check]
  (filter (partial are-carts-colliding cart-to-check) carts))

(defn remove-colliding-carts [carts cart-to-check]
  (remove (partial are-carts-colliding cart-to-check) carts))

(defn do-tick [game]
  (loop [[cart & rest-carts] (:carts game)
         updated-carts []]
    (if (nil? cart)
      (assoc game :carts updated-carts)
      (let [cart (move-cart (:board game) cart)
            collisions (concat (get-colliding-carts updated-carts cart) (get-colliding-carts rest-carts cart))
            updated-carts-without-collision (remove-colliding-carts updated-carts cart)
            old-carts-without-collision (remove-colliding-carts rest-carts cart)]
        (if (not-empty collisions)
          (do
            (println "Carts collided")
            (dorun (map print-cart collisions))
            (recur old-carts-without-collision updated-carts-without-collision))
          (recur rest-carts (conj updated-carts cart)))))))

(defn do-ticks-until-last-cart [game]
  (if (< (count (:carts game)) 2)
    (print-game-state game)
    (do
      ;(. TimeUnit/MILLISECONDS sleep 200)
      ;(println "\n\nNew loop\n\n")
      ;(print-game-state game)
      ;(flush)
      (recur (do-tick game)))))