(ns aoc18.day21
  (:require [clojure.string :as str]))

(defn long-str [& strings]
  (str/join "\n" strings))

(def input (long-str
             "#ip 3"
             "seti 123 0 1"
             "bani 1 456 1"
             "eqri 1 72 1"
             "addr 1 3 3"
             "seti 0 0 3"
             "seti 0 7 1"
             "bori 1 65536 4"
             "seti 3798839 3 1"
             "bani 4 255 5"
             "addr 1 5 1"
             "bani 1 16777215 1"
             "muli 1 65899 1"
             "bani 1 16777215 1"
             "gtir 256 4 5"
             "addr 5 3 3"
             "addi 3 1 3"
             "seti 27 6 3"
             "seti 0 2 5"
             "addi 5 1 2"
             "muli 2 256 2"
             "gtrr 2 4 2"
             "addr 2 3 3"
             "addi 3 1 3"
             "seti 25 3 3"
             "addi 5 1 5"
             "seti 17 1 3"
             "setr 5 6 4"
             "seti 7 8 3"
             "eqrr 1 0 5"
             "addr 5 3 3"
             "seti 5 6 3"))

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

(defn run-prog [cpu-state break-at]
  (loop [{:keys [ip prog] :as cpu} cpu-state
         n 0]
    (if (= (:ip cpu-state) 28)
      (println n "=>" (:registers cpu)))
    (if (>= ip (count prog))
      cpu
      (if (and (not (nil? break-at)) (break-at cpu))
        cpu
        (recur (cpu-tick cpu) (inc n))))))

(defn print-prog [{:keys [prog]}]
  (dorun (map-indexed (fn [i p]
                        (print (str "[" i "] => "))
                        (print-instruction p)) prog)))