(ns platinum-rift.player
  (:require [platinum-rift.world :as world]
            [platinum-rift.constants :refer :all]
            [platinum-rift.mimic :refer :all] ;;refer to won't need to specify namespace just like in the real game
            [platinum-rift.advisors :as advisors]))


(def next-id (atom -1))



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
        ag-lib (agent 0)] ;;totals liberties of player
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
    {:id p1
     :income @ag-inc
     :territories @ag-terr
     :liberties @ag-lib}))

(defn evaluate
  "Quantifies players position."
  [p1 turn]
  ;;todo make this useful
  (+ (:platinum p1) (reduce + (:pods p1)) (:income p1) (- (:liberties p1))  (:territories p1)))

(defn new-player
  "Creates and returns a new player."
  ([id] (new-player id num-nodes))
  ([id num-nodes]
     {:id id
      :platinum starting-plat
      :income 0
      :territories 0 ;;determined every round
      :liberties 0 ;;determind every round
      ;;pods is a vector the nth value in pod corresponds to the nth node in the world,
      ;;the value at that index is the number of pods the player has on that node
      :pods (loop [pods 0
                   acc []]
              (if (< pods num-nodes)
                (recur (inc pods)
                       (conj acc 0))
                acc))}))


(defn det-move
  "Returns a vector of vectors that represent how pods should be moved to their local minima. Does not combine information."
  [sight p1 world]
  (println "Determining move")
  ;;create a scalar map of the world and move each unit towards its local minimum
  (let [scalar-world (advisors/advise world p1 (advisors/get-advisors) sight)]
    ;; (println "Advised scalar world: " scalar-world)
    (loop [i 0
           pods (:pods p1)
           outer-acc []]
      ;; (println (str "i: " i " pods: " pods " acc: " outer-acc))
      (if (empty? pods)
        outer-acc ;;return moves
        (recur (inc i)
               (next pods)
               (reduce conj outer-acc (loop [pods-remaining (first pods)
               acc []]
;;                                   (println (str "Inner loop
;; pods-remaining: " pods-remaining "
;; acc: " acc))
          (if (< 0 pods-remaining)
            (recur (dec pods-remaining)
                   (conj acc [1 i (second
                          ;;get shortest path
                          (world/get-shortest-path i ;;pods current position
                                                   ;;get node id of local minima
                                                   (:id (world/get-local-min sight
                                                                              i
                                                                              scalar-world))))]))
            acc))))))))


;; (recur (inc i)
;;                (next pods)
;;                ;;conj onto accumulator a movement of one pod along its path to its local min
;;                (conj acc
;;                     ;;or first tries to find the next node along the shortest path to the pods local minima
;;                      [(or (second
;;                           ;;get shortest path
;;                           (world/get-shortest-path i ;;pods current position
;;                                                    ;;get node id of local minima
;;                                                    (:id (world/get-local-min sight
;;                                                                               i
;;                                                                               scalar-world))))
;;                           ;;this second clause of the or will return the pods current node
;;                           i)
;;                       ;;move only one pod
;;                       ]))


(defn det-place
  "Determines where to place units. Does not combine information"
  [p1 world]
  (println "Determining placement")
  (loop [wor world
         pods (int (/ (:platinum p1) pod-cost))
         acc []]
    (if (= 0 pods)
      acc
      ;;recur with point modified map and one less pod
      (let [global-min (world/get-global-min wor)]
        (recur (advisors/point-mod wor p1 global-min (advisors/get-advisors) standard-radius)
               (dec pods) ;;decrease pods available by 1
               ;;place a pod at global minima
               (conj acc [1 (:id global-min)]))))))

(defn comp-move
  "Compares if two move vectors can be combined, returns the combination if so."
  [a b]
  (if (and (= (second a) (second b))
           (= (second (next a)) (second (next b))))
    [[(+ (first a) (first b)) (second a) (second (next a))]]
    [a b]))

(defn comp-place
  "Compares if two placement vectors can be combined, returns the combination if so."
  [a b]
  (if (= (second a) (second b))
    [[(+ (first a) (first b)) (second a)]]
    [a b]))

(defn combine-vectors
  "Combines movement and placement vectors if possible."
  [vectors]
  (loop [acc [(first (sort-by second vectors))]
         v (next (sort-by second vectors))]
    ;; (println acc)
    (if (empty? v)
      acc
      (recur (reduce conj (reduce conj [] (butlast acc)) ((if (= 3 (count (first vectors))) comp-move comp-place ) (last acc) (first v)) )
             (next v)))))

(defn v-to-msg
  "Converts a vector into a string message."
  [vector]
  (loop [acc ""
         v vector]
    (if (empty? v)
      acc
      (recur (str acc (apply str " " (interpose " " (first v)) ))
             (next v)))))

(defn gen-move-message
  "Returns a string to move units."
  [unit-move-vector]
  (if (or (empty? unit-move-vector) (= nil (first unit-move-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-move-vector))))

(defn gen-place-message
  "Returns a string to place new units."
  [unit-place-vector]
  (if (or (empty? unit-place-vector) (= nil (first unit-place-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-place-vector))))
