(ns clojush.pushgp.selection.knobelty
  (:use [clojush random util globals]
        [clojush.pushgp.selection tournament novelty lexicase])
  (:require [clojure.math.numeric-tower :as math]))


    (defn knobelty-selection
      [pop argmap]
      (if (< (rand) (:knobelty-const argmap)) (lexicase-selection pop argmap) (novelty-tournament-selection pop argmap)))
