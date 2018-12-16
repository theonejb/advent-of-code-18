(ns aoc18.day16
  (:require [clojure.string :as str]))

(defn long-str [& strings]
  (str/join "\n" strings))

(def test-input (long-str
                  "Before: [3, 2, 1, 1]"
                  "9 2 1 2"
                  "After:  [3, 2, 2, 1]"))

(defn parse-register-values [input]
  (vec (map #(Integer/parseInt %1) (re-seq #"\d+" input))))

(defn parse-instruction [input]
  (let [[opcode a b c] (str/split input #" ")]
    {:opcode (Integer/parseInt opcode) :a (Integer/parseInt a) :b (Integer/parseInt b) :c (Integer/parseInt c)}))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (loop [parsed []
           [before instruction after & rest-lines :as all-lines] lines]
      (if (empty? all-lines)
        parsed
        (if (str/includes? before "Before")
          (recur (conj parsed {:registers-before (parse-register-values before)
                               :instruction      (parse-instruction instruction)
                               :registers-after  (parse-register-values after)})
                 rest-lines)
          (recur parsed (rest all-lines)))))))

(defn initialize-cpu
  ([] (initialize-cpu [0 0 0 0]))
  ([registers]
   {:registers registers}))

(defn op-r [f]
  (fn [{:keys [registers] :as cpu} {:keys [a b c]}]
    (let [a-val (nth registers a)
          b-val (nth registers b)
          result (f a-val b-val)]
      (assoc cpu :registers (assoc registers c result)))))
(defn op-i [f]
  (fn [{:keys [registers] :as cpu} {:keys [a b c]}]
    (let [a-val (nth registers a)
          b-val b
          result (f a-val b-val)]
      (assoc cpu :registers (assoc registers c result)))))

(defn set-r [{:keys [registers] :as cpu} {:keys [a c]}]
  (let [a-val (nth registers a)]
    (assoc cpu :registers (assoc registers c a-val))))
(defn set-i [{:keys [registers] :as cpu} {:keys [a c]}]
  (let [a-val a]
    (assoc cpu :registers (assoc registers c a-val))))

(defn gtir [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val a
        b-val (nth registers b)
        result (if (> a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))
(defn gtri [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val (nth registers a)
        b-val b
        result (if (> a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))
(defn gtrr [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val (nth registers a)
        b-val (nth registers b)
        result (if (> a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))

(defn eqir [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val a
        b-val (nth registers b)
        result (if (= a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))
(defn eqri [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val (nth registers a)
        b-val b
        result (if (= a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))
(defn eqrr [{:keys [registers] :as cpu} {:keys [a b c]}]
  (let [a-val (nth registers a)
        b-val (nth registers b)
        result (if (= a-val b-val) 1 0)]
    (assoc cpu :registers (assoc registers c result))))

(def opcode->opcode-symbol
  {0  :eqir
   1  :seti
   2  :eqri
   3  :eqrr
   4  :addi
   5  :setr
   6  :gtrr
   7  :gtri
   8  :muli
   9  :bori
   10 :bani
   11 :borr
   12 :gtir
   13 :banr
   14 :addr
   15 :mulr})

(def found-opcodes
  (set (map second opcode->opcode-symbol)))

(def opcode->f
  {:addr (op-r +)
   :addi (op-i +)

   :mulr (op-r *)
   :muli (op-i *)

   :banr (op-r bit-and)
   :bani (op-i bit-and)

   :borr (op-r bit-or)
   :bori (op-i bit-or)

   :setr set-r
   :seti set-i

   :gtir gtir
   :gtri gtri
   :gtrr gtrr

   :eqir eqir
   :eqri eqri
   :eqrr eqrr})

(defn find-matching-opcodes [{:keys [registers-before instruction registers-after] :as input}]
  (let [cpu (initialize-cpu registers-before)]
    {:opcode       (:opcode instruction)
     :behaves-like (remove nil? (for [[opcode f] opcode->f]
                                  (let [cpu (f cpu instruction)]
                                    (if (= (:registers cpu) registers-after) opcode nil))))}))

(defn load-instructions [fname]
  (map parse-instruction (str/split-lines (slurp fname))))

(defn run-prog [prog]
  (let [cpu (initialize-cpu)]
    (reduce (fn [cpu {:keys [opcode] :as instruction}]
              (let [opcode-symbol (opcode->opcode-symbol opcode)
                    opcode-f (opcode->f opcode-symbol)]
                (opcode-f cpu instruction)))
            cpu
            prog)))