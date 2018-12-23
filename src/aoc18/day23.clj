(ns aoc18.day23
  (:require [clojure.string :as str]))

(defn long-str [& strings] (str/join "\n" strings))

(def test-input (long-str
                  "pos=<0,0,0>, r=4"
                  "pos=<1,0,0>, r=1"
                  "pos=<4,0,0>, r=3"
                  "pos=<0,2,0>, r=1"
                  "pos=<0,5,0>, r=3"
                  "pos=<0,0,3>, r=1"
                  "pos=<1,1,1>, r=1"
                  "pos=<1,1,2>, r=1"
                  "pos=<1,3,1>, r=1"))

(defn parse-bot-pos [line]
  (let [[x y z radius] (re-seq #"-?\d+" line)]
    {:x (Integer/parseInt x) :y (Integer/parseInt y) :z (Integer/parseInt z) :r (Integer/parseInt radius)}))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-bot-pos)))

(defn mdistance [{:keys [x y z]} {x1 :x y1 :y z1 :z}]
  (+ (Math/abs ^int (- x x1))
     (Math/abs ^int (- y y1))
     (Math/abs ^int (- z z1))))

(defn is-in-range? [bot1 bot2]
  (<= (mdistance bot1 bot2) (:r bot1)))

(defn find-bots-in-range [bot all-bots]
  (filter #(is-in-range? bot %1) all-bots))

(defn add-bots-in-range [all-bots]
  (map (fn [bot]
         (assoc bot :in-range (find-bots-in-range bot all-bots))) all-bots))

(defn fname->bots [fname]
  (-> fname
      slurp
      parse-input
      add-bots-in-range))