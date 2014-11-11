(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.player :as player]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:gen-class))

(def debug true)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;;create a world
  (world/reset-world)
  (let [players (create-players 2)]
    (loop [turn 1]
      (if (<= turn 200)
        true
        (recur (inc turn))))
    (println "Hello, World!")))


(defn create-players
  "Returns a list of players"
  [num]
  (player/reset)
  (loop [more num
         acc []]
    (if (= more 0)
      acc
      (recur (dec more)
             (conj acc (player/new-player))))))
