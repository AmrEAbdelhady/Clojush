(ns clojush.pushgp.selection.batch-tournament
  (:use [clojush random]))

(def curr-batch (atom 0))

(defn batch-selection
  [pop argmap]
  (let [batch (nth (:batches argmap) @curr-batch)]
    (if (> @curr-batch (- (count (:batches argmap)) 2))
    (reset! curr-batch 0)
    (swap! curr-batch + 1))
    (let [tourn (take (:tourn-size argmap) (shuffle pop))]

      (def err-vals  (map #(apply + %) (map (fn [y] (keep-indexed #(if (.contains batch %1) %2) y)) (map :errors tourn))))
      (nth tourn (.indexOf err-vals (apply min err-vals))))))
