(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.player :as player]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:gen-class))

(def debug true)
(declare create-players)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;;create a world
  (world/reset-world)
  (let [players (create-players 2)]
    (loop [turn 1
           world @world/world]
      ;;basic turn structure

      ;;determine where to move units
      (let [movement (player/det-move 3 (first players) world)
            placement (player/det-place (first players) world)]
      ;;determine where to place units
      ;;send commands
      (when debug
        (println "TURN: " turn)
        (println "Movement: " movement)
        (println "Placement: " placement)
        (println "Player1 eval: " (player/evaluate (first players) turn))
        (println "Player2 eval: " (player/evaluate (second players) turn))))
      (if (< turn 200)
        (recur (inc turn)
               @world/world)))))


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
