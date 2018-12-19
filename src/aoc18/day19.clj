(ns aoc18.day19
  (:require [clojure.string :as str]))

(defn long-str [& strings]
  (str/join "\n" strings))

(def test-input (long-str
                  "#ip 0"
                  "seti 5 0 1"
                  "seti 6 0 2"
                  "addi 0 1 0"
                  "addr 1 2 3"
                  "setr 1 0 0"
                  "seti 8 0 4"
                  "seti 9 0 5"))

(def input (long-str
             "#ip 3"
             "addi 3 16 3"
             "seti 1 7 1"
             "seti 1 7 5"
             "mulr 1 5 4"
             "eqrr 4 2 4"
             "addr 4 3 3"
             "addi 3 1 3"
             "addr 1 0 0"
             "addi 5 1 5"
             "gtrr 5 2 4"
             "addr 3 4 3"
             "seti 2 2 3"
             "addi 1 1 1"
             "gtrr 1 2 4"
             "addr 4 3 3"
             "seti 1 5 3"
             "mulr 3 3 3"
             "addi 2 2 2"
             "mulr 2 2 2"
             "mulr 3 2 2"
             "muli 2 11 2"
             "addi 4 2 4"
             "mulr 4 3 4"
             "addi 4 2 4"
             "addr 2 4 2"
             "addr 3 0 3"
             "seti 0 8 3"
             "setr 3 8 4"
             "mulr 4 3 4"
             "addr 3 4 4"
             "mulr 3 4 4"
             "muli 4 14 4"
             "mulr 4 3 4"
             "addr 2 4 2"
             "seti 0 7 0"
             "seti 0 9 3"))

(defn parse-instruction [input]
  (let [[opcode a b c] (str/split input #" ")]
    {:opcode (keyword opcode) :a (Integer/parseInt a) :b (Integer/parseInt b) :c (Integer/parseInt c)}))

(defn parse-input-lines [input-lines]
  (let [ip-register (second (str/split (first input-lines) #" "))
        lines (rest input-lines)]
    {:ip-register (Integer/parseInt ip-register) :prog (map parse-instruction lines)}))

(defn initialize-cpu [{:keys [ip-register prog]}]
  {:registers   [0 0 0 0 0 0]
   :ip          0
   :prog        prog
   :ip-register ip-register})

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

(def ri->name
  {0 "A"
   1 "B"
   2 "C"
   3 "IP"
   4 "E"
   5 "F"})

(defn print-instruction [{:keys [opcode a b c]}]
  (case opcode
    :addr (println (ri->name c) "=" (ri->name a) "+" (ri->name b))
    :addi (println (ri->name c) "=" (ri->name a) "+" b)

    :mulr (println (ri->name c) "=" (ri->name a) "x" (ri->name b))
    :muli (println (ri->name c) "=" (ri->name a) "x" b)

    :banr (println (ri->name c) "=" (ri->name a) "&" (ri->name b))
    :bani (println (ri->name c) "=" (ri->name a) "&" b)

    :borr (println (ri->name c) "=" (ri->name a) "|" (ri->name b))
    :bori (println (ri->name c) "=" (ri->name a) "|" b)

    :setr (println (ri->name c) "=" (ri->name a))
    :seti (println (ri->name c) "=" a)

    :gtir (println "if" a ">" (ri->name b) "then" (ri->name c) "= 1 else" (ri->name c) "= 0")
    :gtri (println "if" (ri->name a) ">" b "then" (ri->name c) "= 1 else" (ri->name c) "= 0")
    :gtrr (println "if" (ri->name a) ">" (ri->name b) "then" (ri->name c) "= 1 else" (ri->name c) "= 0")

    :eqir (println "if" a "=" (ri->name b) "then" (ri->name c) "= 1 else" (ri->name c) "= 0")
    :eqri (println "if" (ri->name a) "=" b "then" (ri->name c) "= 1 else" (ri->name c) "= 0")
    :eqrr (println "if" (ri->name a) "=" (ri->name b) "then" (ri->name c) "= 1 else" (ri->name c) "= 0")
    ))

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


(defn fname->cpu [fname]
  (-> fname
      slurp
      str/split-lines
      parse-input-lines
      initialize-cpu))

(defn input->cpu [input]
  (-> input
      str/split-lines
      parse-input-lines
      initialize-cpu))

(defn cpu-tick [{:keys [registers ip prog ip-register] :as cpu}]
  (let [instruction (nth prog ip)
        f (opcode->f (:opcode instruction))
        registers (assoc registers ip-register ip)
        cpu (assoc cpu :registers registers)
        cpu (f cpu instruction)
        ip (nth (:registers cpu) ip-register)]
    (assoc cpu :ip (inc ip))))

(defn run-prog [cpu-state]
  (loop [{:keys [ip prog] :as cpu} cpu-state
         n 0]
    (println n " => " (select-keys cpu [:registers :ip]))
    (if (>= ip (count prog))
      cpu
      (recur (cpu-tick cpu) (inc n)))))