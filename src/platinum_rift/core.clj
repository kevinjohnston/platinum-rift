(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.player :as player]
            [platinum-rift.constants :refer :all]
            [platinum-rift.mimic :refer :all]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:gen-class))

(declare create-players)
(def official-world (world/reset-world))
;;our understanding of the world, for tactical purposes
(def s-world (agent []))
;;our understanding of the world, for movement purposes
(def g-world (agent (graph/make-graph #{} {})))
(def sight-radius 3)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [num-players 2
        players (create-players num-players)
        game-world @official-world] ;;create a world
    ;;add in initialization content to be read by program
    ;;TODO move all constants into this file
    (mwrite (count players))
    (mwrite (:id (nth players (rand-int num-players))))
    (mwrite world/num-nodes)
    (mwrite (count world/edge-vector)) ;;TODO fix, this returns 303, according to webpage it should be 306
    ;;add in zone id and plat information
    (loop [i world/num-nodes
           nodes game-world]
      (mwrite (:id (first nodes))) ;;write node id
      (mwrite (:income (first nodes))) ;;write node income
      (if (> i 1)
        (recur (dec i) (next nodes))))

    ;;add in link id information
    (loop [i (count world/edge-vector)
           vec world/edge-vector]
      (mwrite (first (first vec))) ;;write node id
      (mwrite (second (first vec))) ;;write node income
      (if (> i 1)
        (recur (dec i) (next vec))))
    ;;add in starting platinum information
    (mwrite 200)

    (await g-world s-world)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (let [playerCount (mread) myId (mread) zoneCount (mread) linkCount (mread)]
      ;; playerCount: the amount of players (2 to 4)
      ;; myId: my player ID (0, 1, 2 or 3)
      ;; zoneCount: the amount of zones on the map
      ;; linkCount: the amount of links between all zones
      (loop [i zoneCount]
        (when (> i 0)
          (let [zoneId (mread) platinumSource (mread)]
            ;; zoneId: this zone's ID (between 0 and zoneCount-1)
            ;; platinumSource: the amount of Platinum this zone can provide per game turn
            (send g-world #(graph/add-nodes % zoneId))
            (send s-world #(conj % (world/new-node zoneId platinumSource)))
            (recur (dec i)))))

      (loop [i linkCount]
        (when (> i 0)
          (let [zone1 (mread) zone2 (mread)]
            ;;record edge between nodes
            (send g-world #(graph/add-edge % zone1 zone2))
            ;;update liberties of both zones
            (send s-world #(assoc-in % [zone1 :total-liberties] (inc (:total-liberties (nth % zone1)))))
            (send s-world #(assoc-in % [zone2 :total-liberties] (inc (:total-liberties (nth % zone2)))))
            (recur (dec i)))))

      (await g-world s-world)
      (when debug
        (println "playerCount: " playerCount)
        (println "myId: " myId)
        (println "zoneCount: " zoneCount)
        (println "linkCount: " linkCount))

      ;;(while true -- game loop
      (loop [turn 1
             world game-world]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;turn init
        (let [platinum (mread)]
          ;; (println "My platinum: " platinum)
          ;; platinum: my available Platinum
          (loop [i zoneCount]
            (when (> i 0)
              (let [zId (mread) ownerId (mread) podsP0 (mread) podsP1 (mread) podsP2 (mread) podsP3 (mread)]
                ;; zId: this zone's ID
                ;; ownerId: the player who owns this zone (-1 otherwise)
                ;; podsP0: player 0's PODs on this zone
                ;; podsP1: player 1's PODs on this zone
                ;; podsP2: player 2's PODs on this zone (always 0 for a two player game)
                ;; podsP3: player 3's PODs on this zone (always 0 for a two or three player game)
                (recur (dec i)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;verify AI view of world matches actual world
          (check-world (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) @s-world)
                       (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) game-world))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



          ;;basic turn structure

          ;;determine where to move units
          (let [movement (player/gen-move-message (player/det-move sight-radius (nth players myId) world))
                placement (player/gen-place-message (player/det-place (nth players myId) world)) ]
            ;; (println "global min: " (world/get-global-min @s-world))
            (mprintln (nth players myId) movement @official-world)
            (mprintln (nth players myId) placement @official-world)
            ;;determine where to place units
            ;;send commands
            (when debug
              (println "TURN: " turn)
              (println "Movement: " movement)
              (println "Placement: " placement)
              (println "Player1 eval: " (player/evaluate (first players) turn))
              (println "Player2 eval: " (player/evaluate (second players) turn))
              ))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAME PHASES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;MOVEMENT
          ;;TODO process movement from AIs
;;;;;;;;;;;;;;;;;;;;BUYING
          ;;TODO process placement from AIs
;;;;;;;;;;;;;;;;;;;;FIGHTING
          ;;TODO follow fight logic
;;;;;;;;;;;;;;;;;;;;OWNING
          ;;TODO update ownership in world
;;;;;;;;;;;;;;;;;;;;DISTRIBUTING INCOME
          ;;TODO adjust player income




          (if (< turn 200)
            (recur (inc turn)
                   @world/world)))
        ;;)-- end while loop
        ))
    (shutdown-agents)
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME OVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))


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
