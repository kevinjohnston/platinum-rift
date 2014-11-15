(ns platinum-rift.mimic
  (:require [platinum-rift.constants :refer :all]))

(def read-queue (atom [])) ;;to mimic ds structure in actual game
(def move-commands (atom {0 []
                            1 []
                            2 []
                            3 []}))
(def placement-commands (atom {0 []
                            1 []
                            2 []
                            3 []})) ;;requests made by the players to the game

(def moved (atom false))

(defn run-commands
  [type]
  (condp type
      :placement (loop [player @placement-commands]
                   (loop [command (second (first player))]
                     (when (not (empty? command))
                       ((first command))

                       (recur (next command))))
                   (when (not (empty? player))
                     (recur (next player))))
      :movement (loop [player @move-commands]
                  (loop [command (second (first player))]
                    (when (not (empty? command))
                      ((first command))

                      (recur (next command))))
                  (when (not (empty? player))
                    (recur (next player))))))

(defn enqueue-command
  [type p1 request]
  (if (= type :movement)
    (swap! move-commands #(assoc-in % [p1] (conj (% p1) request)))
    (swap! placement-commands #(assoc-in % [p1] (conj (% p1) request)))))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn mread
  "Mimics what will be returned by the read function in game."
  []
  (let [ret (first @read-queue)]
    (swap! read-queue #(next %))
    ret))

(defn mwrite
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [& info]
  (if debug
    (println "writing: " info))
  (swap! read-queue #(reduce conj % info)))

(defn battle
  "Applies battle logic to each node in the world."
  [world-atom]
  ;;loop over each node in the world and apply battle logic to it, updating the world at the end
;;   (loop [node @world-atom]
;;     (when (not (empty? node))
;;       ;;apply battle logic
;;       (loop [fights 3]

;;         (loop [pods (:pods node)]
;;           (if (empty? pods)
;;             nil
;;             (swap! world-atom #(assoc-in node (:id node))))
;;           )

;;         )


;; ;; Rules for fighting:

;; ;;     A fight is triggered on every zone having PODs from 2, 3 or 4 different players.
;;       ;;     For each fight zone, a POD from each player is first destroyed. If PODs from different players are still present on the zone after this destruction, an additional POD from each player still present is destroyed. This phase reproduces itself one more time. For each fight zone, a player loses a maximum of 3 PODs per game round.



;;       (recur (next node))))
  )

(defn move
  "Applies movement logic to each movement request by each player."
  [p1 [pods start dest] world-agent]
  (let [world @world-agent
        ;;at the given node get the current number of pods owned by the player
        current-pods (((world start) pods) (:id p1))]
    ;;verify movement is possible
    (if (>= current-pods pods)
      ;;move
      (swap! world-agent
             (assoc-in ;;add to new location...
              (assoc-in world [start :pods (:id p1)] (- current-pods pods)) ;;..having removed from old location
              [dest :pods (:id p1)] (- (((world dest) pods) (:id p1)) pods)))
      (println "ERROR NOT ENOUGH PODS FOR MOVEMENT REQUEST"))))

(defn place
  "Applies placement logic to each buy request by each player"
  [p1 [pods node] world-agent]
  (let [world @world-agent
        ;;at the given node get the current number of pods owned by the player
        current-pods (((world node) pods) (:id p1))
        current-plat (:platinum p1)]
    ;;verify movement is possible
    (if (>= current-plat pod-cost)
      ;;move
      (swap! world-agent
             (assoc-in ;;add to new location...
              world ;;..having removed from old location
              [node :pods (:id p1)] (inc current-pods)))
      (println "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST"))))

(defn own
  "Applies ownership logic to each node in the world."
  [world-atom])

(defn distrib
  "Enqueues player platinum amount"
  [[p1 p2 p3 p4] world]
  ;;iterate over world, find matching player-id and owner if any update income accordingly
  (loop [node world]
    (if (nil? node)
      nil
      (let [p*
            (cond
             (= (:id p1) (:id (first node))) p1
             (= (:id p2) (:id (first node))) p2
             (= (:id p3) (:id (first node))) p3
             (= (:id p4) (:id (first node))) p4
             :default nil)]
        (if (not (nil? p*))
          (swap! p* #(assoc-in % :income (+ (:income %) (:income node)))))
        (recur [(next node)])))))

(defn check-world
  ""
  [w1 w2]
  (if (= (count w1) (count w2))
    (loop [i 0]
      (when (> (count w1) i)
        (when (not (= (nth w1 i)
                      (nth w2 i)))
          (println "Node " i " equal? " (= (nth w1 i)
                                           (nth w2 i)))
          (println "id: " ((nth w1 i) :id) " id: " ((nth w2 i) :id)
                   "source-value: " ((nth w1 i) :source-value) " source-value: " ((nth w2 i) :source-value)
                   "scalar-value: " ((nth w1 i) :scalar-value) " scalar-value: " ((nth w2 i) :scalar-value)
                   "owner: " ((nth w1 i) :owner) " owner: " ((nth w2 i) :owner)
                   "open-liberties: " ((nth w1 i) :open-liberties) " :open-liberties " ((nth w2 i) :open-liberties)
                   "total-liberties: " ((nth w1 i) :total-liberties) " :total-liberties " ((nth w2 i) :total-liberties)
                   "income: " ((nth w1 i) :income) " income: " ((nth w2 i) :income)
                   "pods: " ((nth w1 i) :pods) " pods: " ((nth w2 i) :pods)))
        (recur (inc i))))
    (println "Wrong number of nodes w1: " (count w1) " w2: " (count w2))))

(defn mprintln
  "Mimics the functionality of the println function."
  [player request world-agent]
  (println request)
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement (:id player) #(place player request))
      (enqueue-command :placement (:id player)
                       #(map place
                             (cycle [player])
                             (partition 2 (map parse-int (reduce conj [] (.split request " "))))
                             (cycle [world-agent])))
      ;;(place player request)
      ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
      )

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement(:id player) #(move player request world-agent))
      (enqueue-command :movement (:id player) #(map move
                                          (cycle [player])
                                          (partition 3 (map parse-int (reduce conj [] (.split request " "))))
                                          (cycle [world-agent])))
      ;;(move player request)
      ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
      )
    )
  (swap! moved #(not %)))