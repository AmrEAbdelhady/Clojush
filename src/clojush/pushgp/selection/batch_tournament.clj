(ns clojush.pushgp.selection.batch-tournament
  (:use [clojush random]))

(def curr-batch (atom 0))

(defn batch-selection
  [pop argmap]
  (let [batch (nth (:batches argmap) (mod @curr-batch 6))]
    (swap! curr-batch + 1)

    (let [tourn (take (:tourn-size argmap) (shuffle pop))
          err-vals  (map #(apply + %) (map (fn [y] (keep-indexed #(if (.contains batch %1) %2) y)) (map :errors tourn)))]
      (def the-chosen (.indexOf err-vals (apply min err-vals)))
      (nth tourn the-chosen))))
