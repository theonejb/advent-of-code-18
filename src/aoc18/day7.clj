(ns aoc18.day7
  (:require [clojure.string :as str]))


(defn get-input [fname] (str/split-lines (slurp fname)))

(defn parse-input-line [line]
  (let [[_ dependency-step dependent-step] (re-find #"Step (.) must be finished before step (.) can begin." line)]
    {:dependency dependency-step
     :dependant  dependent-step}))

(defn build-empty-dependency-list [steps-info]
  (reduce (fn [list {:keys [dependency dependant]}]
            (assoc (assoc list dependency []) dependant [])) {} steps-info))

(defn build-dependency-lists [steps-info]
  (reduce (fn [dependency-list {:keys [dependency dependant]}]
            (let [current-dependency-list (get dependency-list dependant [])
                  new-dependency-list (conj current-dependency-list dependency)]
              (assoc dependency-list dependant new-dependency-list)))
          (build-empty-dependency-list steps-info) steps-info))

(defn get-next-possible-steps [dependency-list]
  (map first (sort (filter #(= 0 (count (second %1))) dependency-list))))

(defn find-next-step [dependency-list]
  (first (get-next-possible-steps dependency-list)))

(defn mark-dependency-completed [dependency-list completed-step]
  (reduce (fn [new-list [dependant depends-on-list]]
            (if (= dependant completed-step)
              new-list
              (assoc new-list dependant (vec (remove #(= completed-step %1) depends-on-list)))))
          {} dependency-list))

(defn mark-multiple-steps-completed [dependency-list completed-steps]
  (reduce mark-dependency-completed (mark-dependency-completed dependency-list (first completed-steps)) completed-steps))

(defn build-sleigh [dependency-list]
  (loop [steps []
         list dependency-list]
    (if (empty? list)
      steps
      (let [next-step (find-next-step list)
            new-list (mark-dependency-completed list next-step)]
        (recur (conj steps next-step) new-list)))))

(defn get-work-ended-list-at-time [time work-list]
  {:steps-completed (map :step (filter #(> time (:end-time %1)) work-list))
   :new-work-list   (remove #(> time (:end-time %1)) work-list)})

(defn step->work-item [step time fixed-seconds-per-task]
  ; because step is a string and int needs a char
  {:end-time (+ time fixed-seconds-per-task (- (int (first step)) (int \A)))
   :step     step})

(defn remove-currently-in-progress-steps [work-list steps]
  (let [steps-in-progress (map :step work-list)
        pred (set steps-in-progress)]
    (remove pred steps)))

(defn add-new-work [dependency-list current-work-list num-work-to-add t fixed-seconds-per-task]
  (let [steps-without-dependencies (get-next-possible-steps dependency-list)
        steps-that-can-start (remove-currently-in-progress-steps current-work-list steps-without-dependencies)
        steps-to-start (take num-work-to-add steps-that-can-start)
        new-work-items (map #(step->work-item %1 t fixed-seconds-per-task) steps-to-start)
        new-work-list (into current-work-list new-work-items)]
    new-work-list))

(defn build-sleigh-parallel [dependency-list num-workers fixed-seconds-per-task]
  (loop [list dependency-list
         t 0
         work-list []
         completed-steps []]
    (if (empty? list)
      [(dec t) completed-steps]
      (let [{:keys [steps-completed new-work-list]} (get-work-ended-list-at-time t work-list)
            list (mark-multiple-steps-completed list steps-completed)
            completed-steps (into completed-steps steps-completed)]
        (if (< (count new-work-list) num-workers)
          (recur list (inc t) (add-new-work list new-work-list (- num-workers (count new-work-list)) t fixed-seconds-per-task) completed-steps)
          (recur list (inc t) new-work-list completed-steps))))))

(def input (get-input "day7.txt"))
(def steps-info (map parse-input-line input))
(def dependency-list (build-dependency-lists steps-info))
;(def steps-to-build (build-sleigh dependency-list))
;(println (apply str steps-to-build))
