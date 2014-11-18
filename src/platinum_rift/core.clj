(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.player :as player]
            [platinum-rift.constants :refer :all]
            [platinum-rift.mimic :refer :all]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)
           (java.lang Thread))
  (:gen-class))

(declare create-players)
(def official-world (world/reset-world))
(def official-player1 (atom (player/new-player)))
(def official-player2 (atom (player/new-player)))
(def official-player3 (atom (player/new-player)))
(def official-player4 (atom (player/new-player)))
;;our understanding of the world, for tactical purposes
(def s-world (agent []))
;;our understanding of the world, for movement purposes
(def g-world (agent (graph/make-graph #{} {})))
(def sight-radius 3)

(def timekeeper (agent {})) ;;[{playerId# timetaken}]
(def syncher (atom [])) ;;what timekeeper synchs on

(def game-over (atom false))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let [num-players 2
        ai-list (Thread. (take num-players (cycle [(resolve (symbol "ai"))])))
        ;;setup read queues
        info-queues (take num-players (cycle [(atom [])]))
        turn-time 100
        turn-limit 200
        game-over (atom false)] ;;flag for ai's to continue running

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (loop [turn 0
           ai-threads ai-list
           [official-world official-players] [(world/new-world) (create-players num-players 0)] ;;create world and players
           setup-funcs (map #(partial setup-first-turn num-players %1 %2 %3) info-queues (range num-players) [official-world official-players])]

      ;;let form to ease and clarify recur call
      (let [
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AI LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ai-threads (map (partial run-player turn-time) info-queues ai-threads (map #(:id %) official-players) setup-funcs)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME PHASES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            new-state (-> [official-world official-players] ;;each phase returns a vector of [official-world official-players]
                          (movement-phase)
                          (placement-phase)
                          (battle-phase)
                          (ownership-phase)
                          (distribution-phase))]

        ;;recur to the next turn
        (when (< turn turn-limit)
          (recur (inc turn)
                 ai-threads
                 new-state
                 (map #(partial setup-normal %1 %2 %3) info-queues (map #(:id %) official-players) new-state)))))) ;;next turns setup functions

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (swap! game-over #(not %))
  ;;kill any non-daemon threads
  (shutdown-agents))

;;;;;;;;;;;;;;;;;;;;MOVEMENT
      (let [moved-world (run-commands :movement official-world)]
;;;;;;;;;;;;;;;;;;;;BUYING
        (let [world-players (run-commands :placement official-world)
              placed-world (first world-players)
              placed-players (second world-players)]
          ;;clear all commands
          (reset-commands)

          ))

          (run-commands :placement official-world)
;;;;;;;;;;;;;;;;;;;;FIGHTING
          ;;TODO follow fight logic
          (battle official-world)
;;;;;;;;;;;;;;;;;;;;OWNING
          ;;TODO update ownership in world
          (own official-world)
;;;;;;;;;;;;;;;;;;;;DISTRIBUTING INCOME
          ;;TODO adjust player income
          (distrib [official-player1
                    official-player2
                    official-player3
                    official-player4]
                   @official-world)
          (if (< turn turn-limit))
          (recur (inc turn)
                 official-world
                 official-players)


      )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setup)
    (await g-world s-world)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (let [playerCount (mread) myId (mread) zoneCount (mread) linkCount (mread)
          ;;create local players
          players (create-players num-players 0)
          ;; me (condp = myId
          ;;      0 official-player1
          ;;      1 official-player2
          ;;      2 official-player3
          ;;      3 official-player4)
          ]
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
             world game-world
             players players]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;turn init
        (let [platinum (mread)
              players (assoc-in players [myId :platinum] platinum)]
          ;; (println "My platinum: " platinum)
          ;; plawjktinum: my available Platinum
          ;;update my available platinum

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
          (let [movement (.trim (player/gen-move-message (player/det-move sight-radius (nth players myId) world)))
                placement (.trim (player/gen-place-message (player/det-place (nth players myId) world)))  ]
            ;; (println "global min: " (world/get-global-min @s-world))
            (mprintln me movement @official-world)
            (mprintln me placement @official-world)
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
          (run-commands :movement official-world)
;;;;;;;;;;;;;;;;;;;;BUYING
          ;;TODO process placement from AIs
          (run-commands :placement official-world)

          ;;clear all commands
          (reset-commands)
;;;;;;;;;;;;;;;;;;;;FIGHTING
          ;;TODO follow fight logic
          (battle official-world)
;;;;;;;;;;;;;;;;;;;;OWNING
          ;;TODO update ownership in world
          (own official-world)
;;;;;;;;;;;;;;;;;;;;DISTRIBUTING INCOME
          ;;TODO adjust player income
          (distrib [official-player1
                    official-player2
                    official-player3
                    official-player4]
                   @official-world)



          (if (< turn 200)
            (recur (inc turn)
                   @world/world
                   players)))
        ;;)-- end while loop
        ))
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME OVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))


(defn create-players
  "Returns a list of players"
  [num starting-id]
  (player/reset)
  (loop [more num
         id starting-id
         acc []]
    (if (= more 0)
      acc
      (recur (dec more)
             (inc starting-id)
             (conj acc (player/new-player id))))))



;; (let [player (player/new-player)
;;       world-atom (world/reset-world)]
;;   (platinum-rift.mimic/mprintln player "10 0" @world-atom)
;;   ;; (platinum-rift.mimic/run-commands :placement world-atom)

;;   (first @world-atom)
;;   )


;; (let [player (player/new-player)
;;       ;; request "10 0"
;;       request "WAIT"
;;       world-agent (world/reset-world)]
;;   (:id (player/new-player))
;;   (platinum-rift.mimic/enqueue-command :placement (:id player) (partial place player request))
;;   (second (first @platinum-rift.mimic/placement-commands))
;;   (platinum-rift.mimic/run-commands :placement world-agent)

;;   (let [res (first @platinum-rift.mimic/placement-commands)]
;; (player/reset)
;;     res)
;;   )


;; @platinum-rift.mimic/placement-commands




;; ;;parts to recording functions
;; ;;send request to mimic


;; ;;create function to be called later
;; ((first [(partial + 1) (partial + 2)])  2)

;; (def move-commands (atom {0 []
;;                             1 []
;;                             2 []
;;                             3 []}))
;; (def placement-commands (atom {0 []
;;                             1 []
;;                             2 []
;;                             3 []})) ;;requests made by the players to the game

;; ;;call functions
;; ;;[(fn [a] (println a))]
;; (let [player (player/new-player)
;;       placement-commands (atom {0 [(partial platinum-rift.mimic/place-test player [10 0])]
;;                             1 []
;;                             2 []
;;                             3 []})
;;       move-commands (atom {0 []
;;                             1 []
;;                             2 []
;;                            3 []})
;;       world-atom (world/reset-world)]

;;   (platinum-rift.mimic/run-commands-test :placement world-atom placement-commands move-commands)
;;  (player/reset)

;;   )


;; (condp = :things2
;;     :placement "hi"
;;     :movement "bye"
;;     :things "stuff"
;;      nil
;;     )



;; (let [def-fun (fn [& vars] (println vars) (println (reduce + vars)) (reduce + vars))
;;       function-list [(partial def-fun 1) (partial def-fun 2)]]
;;   (loop [func function-list]
;;       ((first func ) 2)
;;     (if (> (count func) 1)

;;       (recur (next func))
;;       ((first func) 2)
;;       )))






;; ;;some atom
;; (def test-atom (atom []))

;; ;;function to swap things
;; (defn i-swap
;;   [my-atom]
;;   (swap! my-atom #(conj % 1))
;;   15)



(defn setup-first-turn
  "Writes information to read queue before a players first turn."
  [num-players read-q player-id [world players]]
    ;;add in initialization content to be read by program
    ;;TODO move all constants into this file
    (mwrite read-q num-players)
    (mwrite read-q player-id)
    (mwrite read-q num-nodes)
    (mwrite read-q (count world/edge-vector)) ;;TODO fix, this returns 303, according to webpage it should be 306
    ;;add in zone id and plat information
    (loop [i num-nodes
           nodes game-world]
      (mwrite read-q (:id (first nodes))) ;;write node id
      (mwrite read-q (:income (first nodes))) ;;write node income
      (if (> i 1)
        (recur (dec i) (next nodes))))

    ;;add in link id information
    (loop [i (count world/edge-vector)
           vec world/edge-vector]
      (mwrite read-q (first (first vec))) ;;write node id
      (mwrite read-q (second (first vec))) ;;write node income
      (if (> i 1)
        (recur (dec i) (next vec))))

    ;;call normal turn setup
    (setup-normal read-q player-id [world players])
    ;; ;;add in starting platinum information
    ;; (mwrite read-q starting-plat)
    )

(defn setup-normal
  "Performs normal setup during game loop."
  [read-q player-id [world players]]
  )

(defn run-player
  "Encapsulates a players AI, handles running and pausing the AI.
  AI -- a partial function, with all parameters supplied to it, so that it can be started or stopped by this function."
  [ai read-q turn-time turn-keeper player-number setup]

  ;;block unti agent receives that all ais have finished or time has run out

  (setup read-q) ;;call the partial function setup, with appropriate read-q

      (println "Finished setup")
  ;;start timer
      (let [ai-thread (Thread. ai)
            started false]
    (while (not @game-over)
      (println "Not over yet")
      (if (empty? (filter false? @turn-keeper)) ;;check if all ai's are ready for the next turn
        (when true
          (if (= 0 (rand-int 10))
            (swap! game-over (fn [_] true)))
          ;;start AI
          (if (.isAlive ai-thread)
            ;; (= Thread.State/NEW (.getState ai))
            (.resume ai-thread)
            (.start ai-thread))
      (println "Started/resumed")
          ;;wait for turn time
          (Thread/sleep turn-time)
      (println "Woke up")
          ;;pause AI
      (if (.isAlive ai-thread)
        (println "alive")
        (println "dead")
            )
      (.suspend ai-thread)
          (println "suspended")
      (if (.isAlive ai-thread)
        (println "alive")
        (println "dead")
            )
;;wait for the next turn to be ready
      (Thread/wait)
          ;; (send turn-keeper #(assoc % player-number true)))
        )
        )
      )
        (.stop ai-thread)
        (println "killed")

      )
java.lang.Thread.State/NEW
java.util.concurrent.TimeUnit/SECONDS
(let [things (Thread. ai1)])

(def turn-keeper (agent [true true true true]))
(send turn-keeper #(assoc % 2 true))

(let [ai test-ai
      read-q (atom [])
      turn-time 100
      turn-keeper (agent [true true true true])
      player-number 0

      num-players 4
      game-world (world/new-world)
      setup (partial setup num-players player-number game-world)]
(swap! game-over (fn [_] false))
  (run-player ai read-q turn-time turn-keeper player-number setup)

  ;; (loop [times 5]
  ;;   (when (> times 0)
  ;;     (println "sleeping")
  ;;     (Thread/sleep 1000)
  ;;     (println "woke up")
  ;;     (recur (dec times))))
  )

  ;; [num-players player-id game-world read-q]


(defn test-ai
  []
    (println "waking up after 10 milliseconds")
  (while true ;;(> (rand-int 100) 20)
  (swap! some-atom #(inc %))
    (Thread/sleep 10)
    (println "waking up after 10 milliseconds")
    )
  )
(def some-atom (atom 1))
(.start (Thread. #(test-ai) ))
 @some-atom
(defn start-thread
  [fn]
  (.start
   (Thread. fn)))

(defn loop-print
  [n]
  (let [line (str n ":**********")]
    (println line)
    (Thread/sleep (rand 5))
    (recur n)))

(defn test-main []
  (dotimes [n 50]
    (start-thread #(loop-print n))))



(def ^:private num-threads 1)
(def ^:private pool (atom nil))
(defn- thread-pool []
  (swap! pool (fn [p] (or p (ScheduledThreadPoolExecutor. num-threads)))))
(defn periodically
  "Schedules function f to run every 'delay' milliseconds after a
  delay of 'initial-delay'."
  [f initial-delay delay]
  (.scheduleWithFixedDelay (thread-pool)
                           f
                           initial-delay delay TimeUnit/MILLISECONDS))
(defn shutdown
  "Terminates all periodic tasks."
  []
  (swap! pool (fn [p] (when p (.shutdown p)))))






(defn ai1
  "Takes in a partial functions, read and println, which are used to communicate with the official game world."
  [mread mprintln]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (let [playerCount (mread) myId (mread) zoneCount (mread) linkCount (mread)
          ;;create local players
          players (create-players num-players 0)
          ;; me (condp = myId
          ;;      0 official-player1
          ;;      1 official-player2
          ;;      2 official-player3
          ;;      3 official-player4)
          ]
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

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (while true
      (loop [turn 1
             world game-world
             players players]

        ;;turn init
        (let [platinum (mread)
              players (assoc-in players [myId :platinum] platinum)]
          ;; (println "My platinum: " platinum)
          ;; plawjktinum: my available Platinum
          ;;update my available platinum

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
          (let [movement (.trim (player/gen-move-message (player/det-move sight-radius (nth players myId) world)))
                placement (.trim (player/gen-place-message (player/det-place (nth players myId) world)))  ]
            ;; (println "global min: " (world/get-global-min @s-world))
            (mprintln me movement @official-world)
            (mprintln me placement @official-world)
            ;;determine where to place units
            ;;send commands
            (when debug
              (println "TURN: " turn)
              (println "Movement: " movement)
              (println "Placement: " placement)
              (println "Player1 eval: " (player/evaluate (first players) turn))
              (println "Player2 eval: " (player/evaluate (second players) turn))
              )))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      );;end initial setup
    )
