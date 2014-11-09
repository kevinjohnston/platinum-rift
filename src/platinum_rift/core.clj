(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:gen-class))

(def debug true)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;;create a world
  (world/reset-world)
  ;;pick a strategy
  (let [strat (strat/pick-strat)]
  (if debug (name strat)))
  ;;pick tactics
  (println "Hello, World!"))
