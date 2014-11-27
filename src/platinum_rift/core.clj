(ns platinum-rift.core
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.player :as player]
            [platinum-rift.advisors :as advisors]
            [platinum-rift.constants :refer :all]
            [platinum-rift.mimic :refer :all]
            [platinum-rift.strategies :as strat]
            [platinum-rift.world :as world])
  (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)
           (java.lang Thread))
  (:gen-class))

(declare create-players)
(def official-world (world/reset-world))
;; (def official-player1 (atom (player/new-player)))
;; (def official-player2 (atom (player/new-player)))
;; (def official-player3 (atom (player/new-player)))
;; (def official-player4 (atom (player/new-player)))
;;our understanding of the world, for tactical purposes
(def s-world (agent []))
;;our understanding of the world, for movement purposes
(def g-world (agent (graph/make-graph #{} {})))

(def timekeeper (agent {})) ;;[{playerId# timetaken}]
(def syncher (atom [])) ;;what timekeeper synchs on

(def game-over (atom false))







;;move each player
;;process each move request by that player


(defn movement-phase
  "Handles moving requests"
  [[official-world official-players]]
  (println "Entered Movement phase")
;;   (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players)
;;   (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world)
  ;; (println official-world)
  ;; (println official-players)
  ;;apply movement requests from each move queue
  (let [orig-move-req @move-requests]
    (swap! move-requests (fn [_] [[] [] [] []]))
  (loop [move-req orig-move-req
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
                 current-pods (((nth next-world start) :pods) player-id)
                 dest-node (reduce conj {} (nth next-world dest))  ]
             ;; (println "DEST NODE: " dest-node)
             ;; (println "start pods: " (:pods (nth next-world start)))
             ;; (println "dest pods: " (:pods dest-node))
             (when (< current-pods pods)
             ;;record error if movement is not possible
               (println (str "ERROR NOT ENOUGH PODS FOR MOVEMENT REQUEST:
requst: " request "
current pods: " current-pods "
player-id: " player-id)))
             (let [dest-pods (:pods dest-node)
                   dest-pods-player (nth dest-pods player-id)
                   total-dest-pods (+ dest-pods-player pods)
                   start-world (assoc-in next-world [start :pods player-id] (- current-pods pods))
                   dest-world (assoc-in ;;add to new location...
                         start-world ;;..having removed from old location
                         [dest :pods player-id]
                         total-dest-pods
                         )])
             (recur (next player-move-requests)
                        ;;move
                        (assoc-in ;;add to new location...
                         (assoc-in next-world [start :pods player-id] (- current-pods pods)) ;;..having removed from old location
                         [dest :pods player-id]
                         (+ (nth (:pods (nth next-world dest)) ;;(:pods dest-node)
                           player-id)
                          pods)))))))))))

(defn placement-phase
  "Handles placement requests."
  [[official-world official-players]]
  (println "Entered Placement phase")
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players)
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world)
;; (println official-world)
  ;; (println official-players)
  ;;loop over each player
  (println "Processing placement requests: " @placement-requests)
  ;;outloop moves over each players placement request queue
  (let [orig-placement-req @placement-requests]
    (swap! placement-requests (fn [_] [[] [] [] []]))
  (loop [place-req orig-placement-req
         player-num 0
         [world players] [official-world official-players]]
    (if (>= player-num (count players))
      [world players] ;;finished processing requests, return most up to date players and world
      (recur ;;move on to next player/world
       (next place-req)
       (inc player-num)
       ;;inner loop goes over each set of placement requests for a given player
       (loop [player-place-req (first place-req)
              [next-world next-players]  [world players]]
         (if (or (= (first player-place-req) "WAIT") (empty? player-place-req))
           [next-world next-players] ;;return world/player reflecting this players placement requests
           (let [request (first player-place-req)
                 pods (first request)
                 node (second request)
                 ;;at the given node get the current number of pods owned by the player
                 current-pods (nth ((next-world node) :pods) player-num)
                 current-plat (:platinum (nth next-players player-num))
                 pod-plat-cost (* pod-cost pods)
                 player (nth next-players player-num)
                 p (println node)
                 [updated-world updated-player] (if (< current-plat pod-plat-cost)
                                                  [next-world player] ;;no change
                                                  [(assoc-in
                                                    next-world
                                                    [node :pods player-num]
                                                    (+ pods current-pods))
                                                   (assoc-in
                                                    (assoc player :platinum (- current-plat pod-plat-cost))
                                                    [:pods node]
                                                    (+ pods (nth (:pods player) node)))])]
             (if (< current-plat pod-plat-cost)
               (println (str  "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST
placement requests: " place-req "
this req: " player-place-req))
;;                (println (str "Request: " request " processed,
;; new world: " updated-world "
;; new player: " updated-player))
               )
             (recur (next player-place-req)
                    [updated-world (assoc players player-num updated-player)])))))))))

;; (assoc-in (assoc (first (create-players 1 0)) :platinum 5)
;;                  [:pods 0]
;;                  (inc (nth (:pods (first (create-players 1 0))) 0)))





;; (inc (nth (:pods (first (create-players 1 0))) 0))


(defn battle-phase
  "Handles battle logic."
  [[official-world official-players]]
(println "Entered Battle phase")
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players)
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world)
;; (println official-world)
  ;; (println official-players)
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
                                (assoc node :pods (into [] (map #(if (< 0 %) (dec %) %) (:pods node))) )
                                (loop [player updated-players
                                       acc []]
                                  (if (empty? player)
                                    acc
                                    (recur
                                     (next player)
                                     (conj acc (assoc-in (first player) [:pods node-id] (if (< 0 ((:pods (first player)) node-id))
                                                                                          (dec ((:pods (first player)) node-id))
                                                                                          (nth (:pods (first player)) node-id)))))))))))]
               [(assoc world node-id updated-node)
                updated-players])))))

(defn change-ownership
  "Helper function for ownership phase. Handles updating specific players and the world.
  Takes in the ids of the player that gained the node, the player that lost the node (or nil if neutral) and the node gained/lost,
  and a vector of players world.
  Returns the updated players and world."
  [player-gain-id player-loss-id node-id
   [world players]]
  ;; (println "gain id: " player-gain-id)
  ;; (println "loss id: " player-loss-id)
  (let [node (world node-id)
        player-gain (players player-gain-id)
        player-loss (if (= -1 player-loss-id) nil (players player-loss-id))
        player-adj-fn #(as-> %1 p
                             (assoc p :income (%2 (:income p) (:income node)))
                             (assoc p :liberties (%2 (:liberties p) (:total-liberties node)))
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



(defn ownership-phase
  "Handles updating ownership in the world"
  [[official-world official-players]]
  (println "Entered Ownership phase")
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players)
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world)
;; (println official-world)
  ;; (println official-players)
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
                   occupying-player-id (loop [pos 0] (if (< 0 (nth (:pods node) pos))
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





(defn distribution-phase
  "Handles updating platinum for the players"
  [[official-world official-players]]
  (println "Entered distribution phase")
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players)
;; (println "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world)

;; (println official-world)
  ;; (println official-players)
;;loop over each node in the world and update its owners platinum by its income amount
  (loop [nodes official-world
         updated-players official-players]
    ;; (println "owner: " (if (empty? nodes) "N/A" (:owner (first nodes))) )
    (if (empty? nodes)
      [official-world updated-players]
      ;;clarify code with let assignment
      (let [node (first nodes)
            owning-player (if (not (= (:owner node) -1)) (updated-players (:owner node)) nil)
            player-id (:owner node)
            current-plat (if (:owner node) (:platinum owning-player))]
      (recur (next nodes)
             (if (not (= (:owner node) -1)) ;;only adjust player income if node is actually owned by someone
               (assoc-in updated-players [player-id :platinum] (+ current-plat (:income node)))
               updated-players))))))


(defn create-players
  "Returns a list of players"
  ([num starting-id] (create-players num starting-id num-nodes))
  ([num starting-id num-nodes]
     (loop [more num
            id starting-id
            acc []]
       (if (= more 0)
         acc
         (recur (dec more)
                (inc id)
                (conj acc (player/new-player id num-nodes)))))))




;; (defn shutdown
;;   "Terminates all periodic tasks."
;;   []
;;   (swap! pool (fn [p] (when p (.shutdown p)))))
(defn setup-normal
  "Performs normal setup during game loop."
  [read-q player-id [world players]]
  ;;write player platinum information
  (mwrite read-q (:platinum (players player-id)))
;;   (println "wrote platinum: " (:platinum (players player-id)) "
;; for player: " player-id)


;;   (println "SETUP NORMAL:
;; world: " world "
;; players: " players)
  ;;write zone information
  (loop [nodes world]
    (when (not (empty? nodes))
      (let [node (first nodes)]
        (mwrite read-q (:id node))
        (mwrite read-q (:owner node))
        (mwrite read-q (nth (:pods node) 0))
        (mwrite read-q (nth (:pods node) 1))
        (mwrite read-q (nth (:pods node) 2))
        (mwrite read-q (nth (:pods node) 3))
      (recur (next nodes))))))

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
           nodes world]
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
    (setup-normal read-q player-id [world players]))


(defn run-player
  "Encapsulates a players AI, handles running and pausing the AI.
  AI -- a partial function, with all parameters supplied to it, so that it can be started or stopped by this function."
  [turn-time read-q ai-thread player-number setup]

  ;;block unti agent receives that all ais have finished or time has run out
;; (println "Called")
  (setup) ;;call the partial function setup, with appropriate read-q
  ;; (println "Finished setup")
  ;;start timer
  ;;start AI
  (if (.isAlive ai-thread)
    ;; (= Thread.State/NEW (.getState ai))
    (.resume ai-thread)
    (.start ai-thread))
  ;; (println "Started/resumed")
  ;;wait for turn time
  (Thread/sleep turn-time)
  ;; (println "Woke up")
  ;;pause AI
  ;; (if (.isAlive ai-thread)
  ;;   (println "alive")
  ;;   (println "dead"))
  (.suspend ai-thread)
  ;; (println "suspended")
  ai-thread)

;; (defn run-player
;;   "Encapsulates a players AI, handles running and pausing the AI.
;;   AI -- a partial function, with all parameters supplied to it, so that it can be started or stopped by this function."
;;   [turn-time read-q ai player-number turn-keeper setup]

;;   ;;block unti agent receives that all ais have finished or time has run out

;;   (setup read-q) ;;call the partial function setup, with appropriate read-q

;;   (println "Finished setup")
;;   ;;start timer
;;   (let [ai-thread (Thread. ai)
;;         started false]
;;     (while (not @game-over)
;;       (println "Not over yet")
;;       (if (empty? (filter false? @turn-keeper)) ;;check if all ai's are ready for the next turn
;;         (when true
;;           (if (= 0 (rand-int 10))
;;             (swap! game-over (fn [_] true)))
;;           ;;start AI
;;           (if (.isAlive ai-thread)
;;             ;; (= Thread.State/NEW (.getState ai))
;;             (.resume ai-thread)
;;             (.start ai-thread))
;;           (println "Started/resumed")
;;           ;;wait for turn time
;;           (Thread/sleep turn-time)
;;           (println "Woke up")
;;           ;;pause AI
;;           (if (.isAlive ai-thread)
;;             (println "alive")
;;             (println "dead"))
;;           (.suspend ai-thread)
;;           (println "suspended")
;;           (if (.isAlive ai-thread)
;;             (println "alive")
;;             (println "dead"))
;;           ;;wait for the next turn to be ready
;;           (.wait ai-thread))))
;;     (.stop ai-thread)
;;     (println "killed")))

(defn ai1
  "Takes in a partial functions, read and println, which are used to communicate with the official game world."
  [mread mprintln]

  (let [debug println ;;keep a reference to clojure.core.println for debugging purposes
        read mread ;;rebind read to match game interface
        println mprintln] ;;rebind println to match game interface
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (let [playerCount (read) myId (read) zoneCount (read) linkCount (read)
          sight-radius 3
          ;;create local players
          players (create-players playerCount 0)
          [graph-world node-world] (loop [i linkCount
                                          [g-world n-world] (loop [i zoneCount
                                                                   g-world (graph/make-graph #{} {})
                                                                   n-world []]
                                                              (if (> i 0)
                                                                (let [zoneId (read) platinumSource (read)]
                                                                  ;; zoneId: this zone's ID (between 0 and zoneCount-1)
                                                                  ;; platinumSource: the amount of Platinum this zone can provide per game turn
                                                                  (recur (dec i)
                                                                         (graph/add-nodes g-world zoneId)
                                                                         (conj n-world (world/new-node zoneId platinumSource))))
                                                                [g-world n-world]))]
                                     (if (> i 0)
                                       (let [zone1 (read) zone2 (read)]
                                         ;;record edge between nodes
                                         ;;update liberties of both zones
                                         (recur (dec i)
                                                [(graph/add-edge g-world zone1 zone2)
                                                 (assoc-in
                                                  (assoc-in n-world [zone1 :total-liberties] (inc (:total-liberties (nth n-world zone1))))
                                                  [zone2 :total-liberties] (inc (:total-liberties (nth n-world zone2))))]))
                                       [g-world n-world]))

          ;;determine shortest paths between nodes
          short-paths (world/find-short-paths graph-world zoneCount)
          ;;determine how the world is divided into unique areas
          conts (world/get-continents node-world short-paths)
          ;;create a map of zone-id : continent-number
          conts-lookup-map (loop [next-cont (dec (count conts))
                                  acc {}]
                             (if (< next-cont 0)
                               acc
                               (recur
                                (dec next-cont)
                                (loop [nodes (nth conts next-cont)
                                       inner-acc acc]
                                  (if (empty? nodes)
                                    inner-acc
                                    (recur (next nodes)
                                           (assoc inner-acc (:id (first nodes)) next-cont)))))))
          world-stats {:tot-inc 120
                       :tot-terr zoneCount}]
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (while true
        (loop [turn 1
               ;;a vector of vectors of numbers, the inner vector of numbers contains ai weights,
               ;;the outer vector contains that set of ai weights for a each continent
               ;; ai-weights [[[1] [1 1] [1 1] [1 1] [1 1]]
               ;;             [[1] [1 1] [1 1] [1 1] [1 1]]
               ;;             [[1] [1 1] [1 1] [1 1] [1 1]]
               ;;             [[1] [1 1] [1 1] [1 1] [1 1]]
               ;;             [[1] [1 1] [1 1] [1 1] [1 1]]]
               turn-world node-world
               turn-players (let [platinum (read)] (assoc-in players [myId :platinum] platinum))]

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;verify AI view of world matches actual world
          ;; (check-world (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) @s-world)
          ;;              (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) world))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


          ;;basic turn structure
          (recur (inc turn)
                 ;; ai-weights
                 (loop [i zoneCount
                        new-world turn-world
                        new-players turn-players]
                   (if (> i 0)
                       ;;update world with new, official, info
                     (let [zId (read) ;; zId: this zone's ID
                           ownerId (read) ;; ownerId: the player who owns this zone (-1 otherwise)
                           podsP0 (read) ;; podsP0: player 0's PODs on this zone
                           podsP1 (read) ;; podsP1: player 1's PODs on this zone
                           podsP2 (read) ;; podsP2: player 2's PODs on this zone (always 0 for a two player game)
                           podsP3 (read)] ;; podsP3: player 3's PODs on this zone (always 0 for a two or three player game)
                       (recur (dec i)
                              (assoc new-world zId
                                     (assoc (assoc (new-world zId) :owner ownerId) :pods [podsP0 podsP1 podsP2 podsP3]))
                              (loop [players new-players
                                     pods-vec [podsP0 podsP1 podsP2 podsP3]
                                     acc []]
                                (if (empty? players)
                                         acc
                                         (recur (next players)
                                                (next pods-vec)
                                                (conj acc
                                                      (assoc-in (first players) [:pods zId] (first pods-vec))))))))
                     ;;on last iteration, determine where to move and place units
                     (let [quant-cont (player/quant-world new-world conts myId)
                           ai-weights (player/update-ai-weights quant-cont world-stats) ;;(or   ai-weights)
                           advised-world (advisors/advise new-world (nth turn-players myId) (advisors/get-advisors) sight-radius ai-weights conts-lookup-map)
                           movement (.trim (player/gen-move-message (player/det-move sight-radius (nth new-players myId) advised-world ai-weights conts-lookup-map)))
                           placement (.trim (player/gen-place-message (player/det-place (nth new-players myId) advised-world ai-weights conts-lookup-map)))]
                       (debug "Movement: " movement)
                       (debug "Placement: " placement)
                       (debug "Quant: " quant-cont)
                       ;;send commands
                       new-world)))
                 (assoc-in turn-players [myId :platinum] (read)))))))) ;;next turns platinum
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;END AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn -main
  "I don't do a whole lot ... yet."
  [& args]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let [num-players 2
        ;;setup read queues
        info-queues (take num-players (cycle [(atom [])]))
        ai-list (doall (do (map #(Thread. %) (loop [i num-players
                                         acc []]
                                    (if (= 0 i)
                                      acc
                                      (recur (dec i)
                                             (conj acc (partial ai1
                                                                (partial mread (nth info-queues (- num-players i)))
                                                                (partial mprintln (- num-players i))))))))))
        turn-time 800
        turn-limit 10
        game-over (atom false)
        orig-world (world/new-world)
        orig-players (create-players num-players 0)] ;;flag for ai's to continue running
;; (println "orig world: " orig-world)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (loop [turn 0
           ai-threads ai-list
           [official-world official-players] [orig-world (create-players num-players 0)] ;;create world and players
           setup-funcs (map #(partial setup-first-turn num-players %1 %2 %3) info-queues (range num-players) (cycle [[official-world official-players]]))]
      (println (str
       "
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TURN " turn " ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
;;        "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL PLAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-players "
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OFFICIAL WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; " official-world
))

      (loop [conts (world/get-continents official-world @world/shortest-paths)
             next-cont (dec (count conts))]

        (when (<= 0 next-cont )
          (println (str
;;                     "next continent:
;; " (nth conts next-cont)
"
size: " (count (nth conts next-cont))
))
          (recur conts (dec next-cont))))



;; (doall (do (println (str "res: " (reduce conj [] (sort (map #(last (last (last %))) @world/shortest-paths))))) ) )



      ;; (doall (do (println (str "res: "
      ;;                          ;; (sort-by second
      ;;                                   (reduce conj [] (sort (map #(last (last (last %))) @world/shortest-paths))
      ;;                                           ;;
      ;;                                           ;; (map #( %
      ;;                                                   ;; [(first %) %]
      ;;                                                   ;;) ;;(last (last (last %)))
      ;;                                   ;; @world/shortest-paths)
      ;;                     ))) ) )
      ;;)
      ;; (doall (do
      ;;          (map #(println (str "[first last]: [" (first %)) " "  (last (last (last %)))  "]") @world/shortest-paths)))
      ;;let form to ease and clarify recur call
      (let [
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AI LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            next-ais (doall (do (map (partial run-player turn-time) info-queues ai-threads (map #(:id %) official-players) setup-funcs)))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME PHASES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            new-state (-> [official-world official-players] ;;each phase returns a vector of [official-world official-players]
                          (movement-phase)
                          (placement-phase)
                          (battle-phase)
                          (ownership-phase)
                          (distribution-phase))]

        ;; (println "NEW STATE:
        ;;          Count: " (count new-state)"
        ;;          World: " (first new-state)"
        ;;          players: " (second new-state))
        ;;recur to the next turn
        (if (< turn turn-limit)
          (recur (inc turn)
                 next-ais
                 new-state
                 (map #(partial setup-normal %1 %2 %3) info-queues (map #(:id %) official-players) (cycle [new-state])))
          (println (str
       "
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAME OVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Player: " (:id (last (sort-by :territories official-players ))) " Wins!
Player0 territory: " (:territories (first official-players)) "
Player1 territory: " (:territories (nth official-players 1)) "
Player2 territory: " (if (< 2 (count official-players)) (:territories (nth official-players 2)) "0")  "
Player3 territory: " (if (< 3 (count official-players)) (:territories (nth official-players 3)) "0")
                        )))))) ;;next turns setup functions

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END GAME LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (swap! game-over #(not %))
  ;;kill any non-daemon threads
  (shutdown-agents))





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
