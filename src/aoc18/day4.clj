(ns aoc18.day4
  (:require [clojure.string :refer [split-lines trim]]
            [java-time :refer [local-date local-date-time as]]))

(defn get-input [fname] (split-lines (slurp fname)))

(defn parse-date [date-string]
  (local-date-time "yyyy-MM-dd HH:mm" date-string))

(defn parse-event-message [message]
  (let [event-message (trim message)]
    (cond
      (= event-message "falls asleep") [:sleep]
      (= event-message "wakes up") [:wake]
      :else [:shift (second (re-matches #"Guard #(\d+) begins shift" event-message))])))

(defn parse-input-line [line]
  (let [[_ date-string rest-of-line] (re-matches #"^\[(.+?)\] (.+)$" line)]
    [(parse-date date-string) (parse-event-message rest-of-line)]))

(defn get-sleep-diary-for-guard [guard-id diaries]
  (get diaries guard-id (repeat 60 [])))

(defn process-diary-entry [sleep-time wake-time minute entry]
  (if (and (>= minute (as sleep-time :minute-of-hour))
           (< minute (as wake-time :minute-of-hour)))
    (conj entry (java-time/format "MM/dd" sleep-time))
    entry))

(defn add-events-to-diary [[sleep-event wake-event :as events] diary]
  (if (nil? sleep-event)
    diary
    (let [[sleep-time _] sleep-event
          [wake-time _] wake-event
          updated-diary (map-indexed #(process-diary-entry sleep-time wake-time %1 %2) diary)]
      (recur (drop 2 events) updated-diary))))

(defn process-events [events]
  (loop [[shift-start-event-time shift-start-event] (first events)
         rest-events (rest events)
         sleep-diaries {}]
    (if (nil? shift-start-event-time)
      sleep-diaries
      (let [guard-id (second shift-start-event)
            guard-sleep-diary (get-sleep-diary-for-guard guard-id sleep-diaries)
            this-guard-events (take-while #(not (= :shift (first (second %1)))) rest-events)
            remaining-events (drop-while #(not (= :shift (first (second %1)))) rest-events)]
        (recur
          (first remaining-events) (rest remaining-events)
          (assoc sleep-diaries guard-id (add-events-to-diary this-guard-events guard-sleep-diary)))))))

(defn get-amount-slept [sleep-diaries]
  (reduce (fn [sleep-counts [k v]]
            (assoc sleep-counts k (reduce + 0 (map count v)))) {} sleep-diaries))

(defn get-most-asleep-minute [diary]
  (first (last (sort-by second (map-indexed (fn [minute diary-entry] [minute (count diary-entry)]) diary)))))

(defn main [fname]
  (let [input (get-input fname)
        events (map parse-input-line input)
        sorted-events (sort-by first events)
        sleep-diaries (process-events sorted-events)
        amounts-slept (get-amount-slept sleep-diaries)
        longest-sleeper (first (last (sort-by second (seq amounts-slept))))
        most-slept-minute (get-most-asleep-minute (get sleep-diaries longest-sleeper))]
    sleep-diaries))