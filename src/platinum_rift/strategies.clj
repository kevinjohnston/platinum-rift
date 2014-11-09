(ns platinum-rift.strategies
  (:require [platinum-rift.world :as world]))

;;Strategies used to evaluate game board

;;strategy abstraction
;;needs to specify goal (increase platinum reserves, gain territory reduce enemey units)

(defn strat-hoard
  "Takes in player stats, returns desired appearance"
  [player]
  (assoc player :platinum (* 1.2 (player :platinum))))


(defn start-harm)
