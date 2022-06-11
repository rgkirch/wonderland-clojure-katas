(ns fox-goose-bag-of-corn.puzzle
  (:require [meander.epsilon :as m]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as z]))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])

(def start [#{:fox :goose :corn :you} #{}])

(defn side-safe?
  [xs]
  (m/match xs
    (m/and (m/not #{:you})
           (m/or #{:goose :corn}
                 #{:fox :goose})) false
    _ true))

(side-safe? #{:corn :you :goose})
;; true
(side-safe? #{:corn :goose})
;; false

(defn safe?
  [[left right]]
  (and (side-safe? left) (side-safe? right)))

(defn done?
  [[left right]]
  (empty? left))

(safe? [#{:corn :you :goose} #{}])
;; true

(defn step
  [[left right]]
  (m/rewrites [left right]

    [#{:you ^& (m/pred side-safe? ?left)} ?right]
    [?left #{:you ^& ?right}]

    [#{:you ?x ^& (m/pred side-safe? ?left)} ?right]
    [?left #{?x :you ^& ?right}]

    [?left #{:you ^& (m/pred side-safe? ?right)}]
    [#{:you ^& ?left} ?right]

    [?left #{:you ?x ^& (m/pred side-safe? ?right)}]
    [#{?x :you ^& ?left} ?right]))

(step [#{:you :corn :goose :fox} #{}])
;; ([#{:fox :corn} #{:you :goose}])

(step [#{:fox :corn} #{:you :goose}])
;; ([#{:you :fox :corn} #{:goose}] [#{:you :fox :goose :corn} #{}])

(m/rewrites #{:you :a :b :c}
  #{:you ?a ^& ?rest} ?rest)
;; (#{:b :a} #{:c :a} #{:c :b})

(defn step-zipper
  "Takes a zipper and adds as children [[step]] applied to the value at loc.
  Returns a seq of zippers that have each navigated to a child."
  [zipper]
  (-> zipper
      (as-> x (reduce z/append-child x (map #(hash-map :value %) (step (:value (z/node x))))))
      z/down
      (as-> x (take-while some? (iterate z/right x)))))

(defn init
  "Construct a zipper from the problem's initial state.
  e.g.
  ```
  (z/node (init [#{:fox :goose :corn :you} #{}]))
  => {:value [#{:you :fox :goose :corn} #{}]}
  ```"
  [state]
  (z/zipper #(not (done? (:value %)))
            :children
            (fn [node children] (assoc node :children children))
            {:value state}))

(defn solve
  "The solution is a state that satisfies [[done?]] (and also `(complement
  branch?)`) by the zipper definition. Reduce over repeated application
  of [[step-zipper]] and terminate with a `reduced` of a zipper to the solved
  state. Then insert `#{:boat}` to each state to satisfy the tests."
  [state]
  (let [solution (reduce (fn [_ states] (when (some (complement z/branch?) states)
                                          (reduced (first (drop-while z/branch? states)))))
                         nil
                         (iterate #(mapcat step-zipper %) [(init state)]))
        raw (conj (mapv :value (reverse (z/path solution)))
                  (:value (z/node solution)))]
    (for [[left right] raw]
      [left #{:boat} right])))

(solve start)
;; ([#{:you :fox :goose :corn} #{:boat} #{}]
;;  [#{:fox :corn} #{:boat} #{:you :goose}]
;;  [#{:you :fox :corn} #{:boat} #{:goose}]
;;  [#{:corn} #{:boat} #{:you :fox :goose}]
;;  [#{:you :goose :corn} #{:boat} #{:fox}]
;;  [#{:goose} #{:boat} #{:you :fox :corn}]
;;  [#{:you :goose} #{:boat} #{:fox :corn}]
;;  [#{} #{:boat} #{:you :fox :goose :corn}])

(defn river-crossing-plan []
  (solve start))
