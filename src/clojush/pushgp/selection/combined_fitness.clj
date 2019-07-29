(ns clojush.pushgp.selection.combined-fitness
  (:use [clojush random]))

  (defn binary-errors
    [evaluated-population argmap ind]
    (count (filter #(= 0 %) (:error ((:error-function argmap) ind :train)))))

(defn combined-fitness-selection
  "Returns an individual that does the best out of a tournament."
  [pop {:keys [tournament-size total-error-method] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))
        zero-err-seq    (map (partial binary-errors pop argmap) tournament-set)
        max-err (err-fn (apply max-key err-fn tournament-set))
        weight-factor (/ max-err (count (:training-cases argmap)))
        weighted-zero-seq (map #(* weight-factor %) zero-err-seq)]

    (nth tournament-set (.indexOf (map #(- %1 %2) (map err-fn tournament-set) weighted-zero-seq) (apply min (map #(- %1 %2) (map err-fn tournament-set) weighted-zero-seq))))))
