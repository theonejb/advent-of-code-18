(ns aoc18.day12
  (:require [clojure.string :as str]))

(defn long-str [& strings] (str/join "\n" strings))
(def test-input (long-str
                  "initial state: #..#.#..##......###...###"
                  ""
                  "...## => #"
                  "...## => ."
                  "...## => ."
                  "..#.. => #"
                  ".#... => #"
                  ".#.#. => #"
                  ".#.## => #"
                  ".##.. => #"
                  ".#### => #"
                  "#.#.# => #"
                  "#.### => #"
                  "##.#. => #"
                  "##.## => #"
                  "###.. => #"
                  "###.# => #"
                  "####. => #"))

(def my-input (long-str
                "initial state: #.#.#..##.#....#.#.##..##.##..#..#...##....###..#......###.#..#.....#.###.#...#####.####...#####.#.#"
                ""
                "..#.. => ."
                "#...# => ."
                ".#... => #"
                "#.##. => ."
                "..#.# => #"
                "#.#.# => ."
                "###.. => #"
                "###.# => #"
                "..... => ."
                "....# => ."
                ".##.. => #"
                "##### => ."
                "####. => ."
                "..##. => ."
                "##.#. => #"
                ".#..# => #"
                "##..# => ."
                ".##.# => ."
                ".#### => #"
                "..### => ."
                "...## => #"
                "#..## => #"
                "#.... => ."
                "##.## => ."
                "#.#.. => ."
                "##... => ."
                ".#.## => #"
                ".###. => #"
                "...#. => ."
                "#.### => ."
                "#..#. => #"
                ".#.#. => ."))

(defn split-input [input]
  (let [lines (str/split-lines input)
        initial-state-line (first lines)
        rule-lines (drop 2 lines)]
    {:initial-state-input initial-state-line
     :rules-input         rule-lines}))

(defn parse-initial-state-input [{:keys [initial-state-input] :as data}]
  (let [[_ initial-state] (str/split initial-state-input #": " 2)]
    (assoc data :state initial-state)))

(defn parse-rule [rule-input]
  (let [[rule _ outcome] (str/split rule-input #" " 3)]
    {:rule    rule
     :outcome outcome}))

(defn parse-rules-input [{:keys [rules-input] :as data}]
  (assoc data :rules (map parse-rule rules-input)))

(defn filter-positive-rules [rules]
  (filter #(= "#" (:outcome %1)) rules))

(defn rules->matcher-pred [{:keys [rules] :as data}]
  ; adds a function to the data map that returns true if a pattern matches any of the positive rules
  (assoc data :does-it-live? (set (map :rule (filter-positive-rules rules)))))

(defn parse-input [input]
  (-> input
      split-input
      parse-initial-state-input
      parse-rules-input
      (select-keys [:state :rules])
      rules->matcher-pred))

(defn next-generation [state does-it-live?]
  (let [expanded-state (str "...." state "....")
        chars-until-last-pot-to-test (- (count expanded-state) 4)]
    (str/join (for [chars-to-drop (range chars-until-last-pot-to-test)]
                (let [pots-under-test (take 5 (drop chars-to-drop expanded-state))
                      pattern-to-match (str/join pots-under-test)
                      lives? (does-it-live? pattern-to-match)]
                  (if lives?
                    "#"
                    "."))))))

(defn run-game [{:keys [state does-it-live?]} num-generations]
  (loop [state state
         current-gen 0
         pots-added 0]
    (if (= current-gen num-generations)
      {:state state :pots-added pots-added}
      (let [next-state (next-generation state does-it-live?)
            pots-added (+ pots-added (- (count next-state) (count state)))]
        (recur next-state (inc current-gen) pots-added)))))

(defn score-game [{:keys [state pots-added]}]
  (let [extra-pots-before (/ pots-added 2)
        starting-pot-number (* -1 extra-pots-before)]
    (loop [[current-pot & rest-pot] state
           pot-number starting-pot-number
           score 0]
      (if (empty? rest-pot)
        score
        (let [this-pot-score (if (= (str current-pot) "#") pot-number 0)]
          (recur rest-pot (inc pot-number) (+ score this-pot-score)))))))