(ns clojush.pushgp.selection.combined-fitness
  (:use [clojush random]))


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
        zero-err-seq    (map (:meta-error argmap) tournament-set) ;;anon function??
        max-err (err-fn (apply max-key err-fn tournament-set))
        ; max-zero-errs (apply max zero-err-seq)
        weight-factor (/ max-err (count (:training-cases argmap)))  ;; training-cases: did i just implement this for certain methods or was it there?
        weighted-zero-seq (map #(* weight-factor %) zero-err-seq)]

    (.indexOf tournament-set (apply min (map #(- %1 %2) (map err-fn tournament-set) weighted-zero-seq)))))
    ; (apply min-key err-fn tournament-set)))
    ; (apply min-key #(err-fn % argmap tournament-set) tournament-set)))

; (defn combined-err-fn
;   [ind argmap]
;
;   )
