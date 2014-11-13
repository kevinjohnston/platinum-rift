(ns platinum-rift.player
  (:require [platinum-rift.world :as world]
            [platinum-rift.advisors :as advisors]))


(def starting-plat 200)
(def next-id (atom 0))
(def pod-cost 20)


;; With each game round, the following actions are executed sequentially :

;;     First step: distributing. Each player receives a number of Platinum bars related to the number of Platinum available on their owned zones.
;;     Second step: moving. Each player moves as many troops as they want on the map.
;;     Third step: buying. Each player buys PODs et puts them on the map.
;;     Fourth step: fighting. Once all players have completed steps 1, 2 and 3, fights are triggered on zones.
;;     Fifth step: owning. Ownership of zones changes.



;;function to evaluate players
(defn player-stats [p1 world]
  (let [ag-inc (agent 0) ;;totals income for player
        ag-terr (agent 0) ;;totals territory controlled by the player
        ag-lib (agent 0) ;;totals liberties of player
        ]
    (defn calc [zone] ;;function to send messages to agents
      "calculates player stats for the zone"
      (doall
       (if (:sub-zones zone)
         (do (map #(calc %) (:sub-zones zone) ))
         (when (= (:owner zone) p1)
           (send ag-inc #(+ (:income zone) %))
           (send ag-terr inc)
           ;;                 (send ag-inc #(+ (:liberties zone) %))
           ();;inexplicably necessary list
           ))))
    (calc world)
    (await ag-inc ag-terr ag-lib)
    {:player p1
     :income @ag-inc
     :territories @ag-terr
     :liberties @ag-lib}))

(defn evaluate
  "Quantifies players position."
  [p1 turn]
  ;;todo make this useful
  (+ (:platinum p1) (count (:pods p1)) (:income p1) (- (:liberties p1))  (:territories p1)))

(defn new-player
  "Creates and returns a new player."
  []
  {:player (swap! next-id #(inc %))
   :platinum starting-plat
   :income 0
   :territories 0 ;;determined every round
   :liberties 0 ;;determind every round
   ;;pods is a vector of vectors [node-id num-pods]
   :pods []})


(defn reset
  ""
  []
  (swap! next-id #(when % 0)))

(defn det-move
  "TODO Determine where to move units"
  [sight p1 world]
  ;;create a scalar map of the world and move each unit towards its local minimum
  (let [scalar-world (advisors/advise world p1 (advisors/get-advisors))]
    (loop [pods (:pods p1)
           acc []]

      (if (empty? pods)
        acc ;;return moves
        (recur (next pods)
               ;;conj onto accumulator a movement of one pod along its path to its local min
               (conj acc
                     [;;or first tries to find the next node along the shortest path to the pods local minima
                      (or (second
                           ;;get shortest path
                           (world/get-shortest-path (first (first pods)) ;;pods current position
                                                    ;;get node id of local minima
                                                    (:id (world/get-local-min sight
                                                                              (first (first pods))
                                                                              scalar-world))))
                          ;;this second clause of the or will return the pods current node
                          (first (first pods)))
                      ;;move only one pod
                      1]))))))


(defn det-place
  "TODO Determine where to place units"
  [p1]
  (loop [world @world/world
         pods (int (/ (:platinum p1) pod-cost))
         acc []]
    ;;find global minima
    ;;place a pod
    (if (= 0 pods)
      acc
      ;;recur with point modified map and one less pod
      (recur world (dec pods) acc))
    ))

(defn gen-move-message
  "Returns a string to move units."
  [unit-move-vector]
  "WAIT")

(defn gen-place-message
  "Returns a string to place new units."
  [unit-place-vector]
  "WAIT")
