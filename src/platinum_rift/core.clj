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



;;move each player
;;process each move request by that player


(defn movement-phase
  "Handles moving requests"
  [[official-world official-players]]
  ;;apply movement requests from each move queue
  (loop [move-req @move-requests
         player official-players
         world official-world]
    (if (empty? player)
      [world official-players]
      (recur
       (next move-req)
       (next player)
       (loop [player-move-requests (first move-req)
              next-world world]
         (if (or (= (first player-move-requests) "WAIT") (empty? player-move-requests))
           next-world ;;return processed world
           (let [request (first player-move-requests)
                 pods (first request)
                 start (second request)
                 dest (nth request 2)
                 player-id (:id (first player))
                 ;;at the given node get the current number of pods owned by the player
                 current-pods (((next-world start) :pods) player-id)]
             (when (< current-pods pods)
             ;;record error if movement is not possible
               (println "ERROR NOT ENOUGH PODS FOR MOVEMENT REQUEST"))
             (recur (next player-move-requests)
                        ;;move
                        (assoc-in ;;add to new location...
                         (assoc-in world [start :pods player-id] (- current-pods pods)) ;;..having removed from old location
                         [dest :pods player-id] (+ (((next-world dest) :pods) player-id) pods))))))))))

(defn placement-phase
  "Handles placement requests."
  [[official-world official-players]]
  ;;loop over each player
  (loop [place-req @placement-requests
         player-num 0
         [world players] [official-world official-players]]
    (if (>= player-num (count players))
      [world players]
      (recur
       (next place-req)
       (inc player-num)
       ;;loop over each set of placement requests
       (loop [player-place-req (first place-req)
              [next-world players]  [world players]]
         (if (or (= (first player-place-req) "WAIT") (empty? player-place-req))
           [next-world players]
           (let [request (first player-place-req)
                 pods (first request)
                 node (second request)
                 ;;at the given node get the current number of pods owned by the player
                 current-pods (((next-world node) :pods) player-num)
                 current-plat (:platinum (nth players player-num))
                 pod-plat-cost (* pod-cost pods)
                 player (nth players player-num)
                 [updated-world updated-player] (if (< current-plat pod-plat-cost)
                                                  [next-world player] ;;no change
                                                  [(assoc-in next-world [node :pods player-num] (+ pods current-pods))
                                                    (assoc player :platinum (- current-plat pod-plat-cost))])]
             (if (< current-plat pod-plat-cost)
               (println "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST"))
             (recur (next player-place-req)
                    [updated-world (assoc players player-num updated-player)]))))))))

(defn battle-phase
  "Handles battle logic."
  [[official-world official-players]]
  ;;loop over each node in the world
  (loop [node-id 0
         [world players] [official-world official-players]]
    (if (<= (count world) node-id)
      [world players]
      (recur (inc node-id)
             (let [[updated-node updated-players]
                   (loop [rounds-left max-fight-loop
                          node (nth world node-id)
                          updated-players players]
                     (let [contested (< 1 (count (filter #(> % 0) (:pods node))))]
                       (if (or (not contested)
                               (= 0 rounds-left))
                         [node updated-players]
                         (recur (dec rounds-left)
                                (assoc node :pods (map #(if (< 0 %) (dec %) %) (:pods node)))
                                (loop [player updated-players
                                       acc []]
                                  (if (empty? player)
                                    acc
                                    (recur
                                     (next player)
                                     (conj acc (assoc-in (first player) [:pods node-id] (if (< 0 ((:pods (first player)) node-id))
                                                                                          (dec ((:pods (first player)) node-id))
                                                                                          ((:pods (first player)) node-id)))))))))))]
               [(assoc world node-id updated-node)
                updated-players])))))


(battle-phase
 [[{:id 0 :pods [2 1 0 0]} {:id 1 :pods [1 2 0 0]}] [{:id 0 :pods [2 1]} {:id 1 :pods [1 2]}]])


(nth [{:id 0 :pods [2 1 0 0]} {:id 1 :pods [2 1 0 0]}] 0)

(create-players 2 0)

(defn ownership-phase
  "Handles updating ownership in the world"
  [[official-world official-players]]
  ;;loop over each node in the world
  (loop [node-id 0
         [updated-world updated-players] [official-world official-players]]
    (if (>= node-id (count official-world))
      [updated-world updated-players]
      (recur (inc node-id)
             ;;determine if node is
             ;;1) NOT contested, and
             ;;2) has at least one pod on it, and
             ;;3) that pod is not by the player listed as owning it
             (let [node (official-world node-id)
                   not-contested (> 2 (count (filter #(> % 0) (:pods node))))
                   occupying-player-id (loop [pos 0] (if ((:pods node) pos)
                                                       pos
                                                       (if (< pos (count official-players))
                                                         (recur (inc pos))
                                                         nil)))
                   defeated-player-id (:owner node)]
               (if (and not-contested
                        occupying-player-id
                        (not (= occupying-player-id defeated-player-id)))
                 ;;all of the above is true, change ownership
                 (change-ownership occupying-player-id defeated-player-id node-id [updated-world updated-players])
                 ;;...otherwise node can keep current ownership
                 [updated-world updated-players]))))))


(defn change-ownership
  "Helper function for ownership phase. Handles updating specific players and the world.
  Takes in the ids of the player that gained the node, the player that lost the node (or nil if neutral) and the node gained/lost,
  and a vector of players world.
  Returns the updated players and world."
  [player-gain-id player-loss-id node-id
   [world players]]
  (println "gain id: " player-gain-id)
  (let [node (world node-id)
        player-gain (players player-gain-id)
        player-loss (if (nil? player-loss-id) nil (players player-loss-id))
        player-adj-fn #(as-> %1 p
                             (assoc p :income (%2 (:income p) (:income node)))
                             (assoc p :liberties (%2 (:liberties p) (:liberties node)))
                             (assoc p :territories (%2 (:territories p) 1)))
        new-gain-players (assoc-in players [player-gain-id] (player-adj-fn player-gain +))
        updated-world (assoc-in world [node-id]
                                (as-> node n
                                      (assoc n :owner player-gain-id)))]
    (if (nil? player-loss)
      ;;handle case that player gained from neutral (nil) player
      [updated-world
       new-gain-players]
      ;;handle case that player conquered territory from another player
      [updated-world
       (assoc-in new-gain-players [player-loss-id] (player-adj-fn player-loss -))])))


(defn distribution-phase
  "Handles updating platinum for the players"
  [[official-world official-players]]
;;loop over each node in the world and update its owners platinum by its income amount
  (loop [nodes official-world
         updated-players official-players]
    (if (empty? nodes)
      [official-world updated-players]
      ;;clarify code with let assignment
      (let [node (first nodes)
            owning-player (updated-players (:owner node))
            player-id (:owner node)
            current-plat (:platinum owning-player)]
      (recur (next nodes)
             (if (:owner node) ;;only adjust player income if node is actually owned by someone
               (assoc-in updated-players [player-id :platinum] (+ current-plat (:income node)))
               updated-players))))))


;; ;;;;;;;;;;;;;;;;;;;;MOVEMENT
;;       (let [moved-world (run-commands :movement official-world)]
;; ;;;;;;;;;;;;;;;;;;;;BUYING
;;         (let [world-players (run-commands :placement official-world)
;;               placed-world (first world-players)
;;               placed-players (second world-players)]
;;           ;;clear all commands
;;           (reset-commands)

;;           ))

;;           (run-commands :placement official-world)
;; ;;;;;;;;;;;;;;;;;;;;FIGHTING
;;           ;;TODO follow fight logic
;;           (battle official-world)
;; ;;;;;;;;;;;;;;;;;;;;OWNING
;;           ;;TODO update ownership in world
;;           (own official-world)
;; ;;;;;;;;;;;;;;;;;;;;DISTRIBUTING INCOME
;;           ;;TODO adjust player income
;;           (distrib [official-player1
;;                     official-player2
;;                     official-player3
;;                     official-player4]
;;                    @official-world)
;;           (if (< turn turn-limit))
;;           (recur (inc turn)
;;                  official-world
;;                  official-players)


;;       )


;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;     (setup)
;;     (await g-world s-world)

;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;     (let [playerCount (mread) myId (mread) zoneCount (mread) linkCount (mread)
;;           ;;create local players
;;           players (create-players num-players 0)
;;           ;; me (condp = myId
;;           ;;      0 official-player1
;;           ;;      1 official-player2
;;           ;;      2 official-player3
;;           ;;      3 official-player4)
;;           ]
;;       ;; playerCount: the amount of players (2 to 4)
;;       ;; myId: my player ID (0, 1, 2 or 3)
;;       ;; zoneCount: the amount of zones on the map
;;       ;; linkCount: the amount of links between all zones
;;       (loop [i zoneCount]
;;         (when (> i 0)
;;           (let [zoneId (mread) platinumSource (mread)]
;;             ;; zoneId: this zone's ID (between 0 and zoneCount-1)
;;             ;; platinumSource: the amount of Platinum this zone can provide per game turn
;;             (send g-world #(graph/add-nodes % zoneId))
;;             (send s-world #(conj % (world/new-node zoneId platinumSource)))
;;             (recur (dec i)))))

;;       (loop [i linkCount]
;;         (when (> i 0)
;;           (let [zone1 (mread) zone2 (mread)]
;;             ;;record edge between nodes
;;             (send g-world #(graph/add-edge % zone1 zone2))
;;             ;;update liberties of both zones
;;             (send s-world #(assoc-in % [zone1 :total-liberties] (inc (:total-liberties (nth % zone1)))))
;;             (send s-world #(assoc-in % [zone2 :total-liberties] (inc (:total-liberties (nth % zone2)))))
;;             (recur (dec i)))))

;;       (await g-world s-world)
;;       (when debug
;;         (println "playerCount: " playerCount)
;;         (println "myId: " myId)
;;         (println "zoneCount: " zoneCount)
;;         (println "linkCount: " linkCount))

;;       ;;(while true -- game loop
;;       (loop [turn 1
;;              world game-world
;;              players players]

;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;turn init
;;         (let [platinum (mread)
;;               players (assoc-in players [myId :platinum] platinum)]
;;           ;; (println "My platinum: " platinum)
;;           ;; plawjktinum: my available Platinum
;;           ;;update my available platinum

;;           (loop [i zoneCount]
;;             (when (> i 0)
;;               (let [zId (mread) ownerId (mread) podsP0 (mread) podsP1 (mread) podsP2 (mread) podsP3 (mread)]
;;                 ;; zId: this zone's ID
;;                 ;; ownerId: the player who owns this zone (-1 otherwise)
;;                 ;; podsP0: player 0's PODs on this zone
;;                 ;; podsP1: player 1's PODs on this zone
;;                 ;; podsP2: player 2's PODs on this zone (always 0 for a two player game)
;;                 ;; podsP3: player 3's PODs on this zone (always 0 for a two or three player game)
;;                 (recur (dec i)))))
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           ;;verify AI view of world matches actual world
;;           (check-world (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) @s-world)
;;                        (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) game-world))
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;           ;;basic turn structure

;;           ;;determine where to move units
;;           (let [movement (.trim (player/gen-move-message (player/det-move sight-radius (nth players myId) world)))
;;                 placement (.trim (player/gen-place-message (player/det-place (nth players myId) world)))  ]
;;             ;; (println "global min: " (world/get-global-min @s-world))
;;             (mprintln me movement @official-world)
;;             (mprintln me placement @official-world)
;;             ;;determine where to place units
;;             ;;send commands
;;             (when debug
;;               (println "TURN: " turn)
;;               (println "Movement: " movement)
;;               (println "Placement: " placement)
;;               (println "Player1 eval: " (player/evaluate (first players) turn))
;;               (println "Player2 eval: " (player/evaluate (second players) turn))
;;               ))
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GAME PHASES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;MOVEMENT
;;           ;;TODO process movement from AIs
;;           (run-commands :movement official-world)
;; ;;;;;;;;;;;;;;;;;;;;BUYING
;;           ;;TODO process placement from AIs
;;           (run-commands :placement official-world)

;;           ;;clear all commands
;;           (reset-commands)
;; ;;;;;;;;;;;;;;;;;;;;FIGHTING
;;           ;;TODO follow fight logic
;;           (battle official-world)
;; ;;;;;;;;;;;;;;;;;;;;OWNING
;;           ;;TODO update ownership in world
;;           (own official-world)
;; ;;;;;;;;;;;;;;;;;;;;DISTRIBUTING INCOME
;;           ;;TODO adjust player income
;;           (distrib [official-player1
;;                     official-player2
;;                     official-player3
;;                     official-player4]
;;                    @official-world)



;;           (if (< turn 200)
;;             (recur (inc turn)
;;                    @world/world
;;                    players)))
;;         ;;)-- end while loop
;;         ))
;;     (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
;;     (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME OVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
;;     (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")))


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







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING
;; (distribution-phase
;;  [[{:id 0 :source-value 0 :pods [5 3 0 0] :income 1 :owner 0}  {:id 1 :source-value 0 :pods [5 3 0 0] :income 3 :owner 1} ]
;;   [{:id 0 :platinum 200} {:id 1 :platinum 200}]])



;; (placement-phase
;;  [[{:id 0 :source-value 0 :pods [5 3 2 5]}  {:id 1 :source-value 0 :pods [5 3 2 5]} ]
;;   [{:id 0 :platinum 200} {:id 1 :platinum 200}]])
;; (create-players 4 0)

;; (first (first @move-requests ))

;; (movement-phase
;;  [[{:id 0 :source-value 0 :pods [5 3 2 5]}  {:id 1 :source-value 0 :pods [5 3 2 5]} ]
;;   (create-players 4 0)])
;; (:id (first (create-players 4 0) ))

;; (first (world/new-world))
