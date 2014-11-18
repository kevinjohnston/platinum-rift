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

(defn reset-commands
  ""
  []
(swap! move-commands (fn [_] {0 []
                            1 []
                            2 []
                            3 []}))
(swap! placement-commands (fn [_] {0 []
                            1 []
                            2 []
                            3 []}) ) ;;requests made by the players to the game

  )

(defn run-commands-orig
  [type world-atom]
  (condp = type
      :placement (loop [player @placement-commands] ;;for each player
                   ;;loop over the list of commands for the player
                   (loop [command (second (first player))]
                     ;; (println "commands: " command)
                     (when (not (empty? command))
                       ((first command) world-atom)
                       (recur (next command))))
                   (when (not (empty? player))
                     (recur (next player))))
      :movement (loop [player @move-commands]
                  (loop [command (second (first player))]
                     ;; (println "command: " (first command))
                    (when (not (empty? command))
                       ((first command) world-atom)
                      (recur (next command))))
                  (when (not (empty? player))
                    (recur (next player)))))
  )

(defn run-commands
  [type world-atom]
  ((fn [commands] (loop [player @commands]
                   (loop [command (second (first player))]
                     (when (not (empty? command))
                       ((first command) world-atom)
                       (recur (next command))))
                   (when (not (empty? player))
                     (recur (next player)))))
   (condp = type
     :placement placement-commands
     :movement move-commands)))

(defn enqueue-command
  [type p1 request]
  (if (= type :movement)
    (swap! move-commands #(assoc-in % [p1] (conj (% p1) request)))
    (swap! placement-commands #(assoc-in % [p1] (conj (% p1) request)))))

;; (macroexpand '(enqueue-command a b c) )



(defn parse-int [s]
  (println "parse-int: " s)
   (Integer. (re-find  #"\d+" s )))

(defn mread
  "Mimics what will be returned by the read function in game."
  []
  (let [ret (first @read-queue)]
    (swap! read-queue #(next %))
    ret))

(defn mwrite-orig
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [& info]
  (if debug
    (println "writing: " info))
  (swap! read-queue #(reduce conj % info)))

(defn mwrite
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [read-queue & info]
  (if debug
    (println "writing: " info))
  (swap! read-queue #(reduce conj % info)))

(defn battle
  "Applies battle logic to each node in the world."
  [world-atom]
  (println "battling")
  ;;iterate over each node in the world
  (loop [node @world-atom
         acc []]
    ;;check if node has pods from more than one player
    ;; (let [contested
    ;;       node-pods (:pods (first node))]

    ;; (when contested

    (if (empty? node)
      acc
      (recur (next node)
             (conj acc (assoc-in (first node) [:pods]
                                 (loop [continue
                                        (>
                                                  (count
                                                     (filter #(> % 0) (:pods (first node)))) 1);;whether or not to loop again
               times 0 ;;times looped so far
               pod-result (:pods (first node))] ;;return value
          ;;decrease pods for all players by one (min 0)
          (if (< times max-fight-loop)
            (recur (> (count (filter #(> % 1) pod-result)) 1)
                   (inc times)
                   (map #(if (> % 0) (dec %) 0) pod-result))
            pod-result)
          )
                                 ))
       )
      )

        ;; )


        ;; )
    )
  )

  ;;when a node is found to have pods from multiple players on it
  ;;loop over the pods decrementing the amount up to three times or when only one player has pods left on the square
  ;;assoc-in the node an update to its pods
  ;;assoc-in the accumulating-new world the updated node and swap the atom with it




  ;;loop over each node in the world and apply battle logic to it, updating the world at the end
  ;; (loop [node @world-atom
  ;;        acc []]
  ;;   (when (not (empty? node))
  ;;     ;;apply battle logic
  ;;     (loop [fights 3]
  ;;       (loop [pods (:pods node)
  ;;              acc []] ;;accumulate functions to possibly run
  ;;         (if (empty? pods)
  ;;           nil
  ;;           (recur
  ;;            (next pods)
  ;;            (conj acc #(swap! world-atom #(assoc-in % (:id node) (dec (:pods (% (:id node))) )))))
  ;;           )
  ;;         )

  ;;       )


;; Rules for fighting:

;; ;;     A fight is triggered on every zone having PODs from 2, 3 or 4 different players.
;;       ;;     For each fight zone, a POD from each player is first destroyed. If PODs from different players are still present on the zone after this destruction, an additional POD from each player still present is destroyed. This phase reproduces itself one more time. For each fight zone, a player loses a maximum of 3 PODs per game round.



;;       (recur (next node))))

(defn move
  "Applies movement logic to each movement request by each player."
  [p1 request world-agent]
  (if (= request "WAIT")
    (println "WAITING M")
    (let [pods (first request)
          start (second request)
          dest (nth request 2)
        world @world-agent
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
  )

(defn place
  "Applies placement logic to each buy request by each player"
  [p1 request world-agent]
  ;; (println "Player " (:id p1) " placing units at " (:id node))
  ;; (println "old node: " (@world-agent (:id node)))
  ;; (println "num pods: " (first request))
  ;; (println "nodeId: " (second request))
  ;; (println "node: " (@world-agent (second request)))
  ;; (println "node pods: " ((@world-agent (second request)) :pods) )
  ;; (println "player id: " (:id p1))
  ;; (println "player pods: " (((@world-agent (second request)) :pods) (:id p1)))
  ;; (println "player plat: " (:platinum p1))
  ;; (println "BEFORE: " (nth @world-agent 0))
  (println "request: " request)
  (println "pods " (first request))
  (println "node " (second request))
  (println "(world node) " (@world-agent (second request)))
  (println "((world node) :pods) " ((@world-agent (second request)) :pods))
  (println "(:id p1) " (:id p1))
  (println "current-pods " (((@world-agent (second request)) :pods) (:id @p1)))
  (println "current-plat " (:platinum @p1))
  (println "pod-plat-cost " (* pod-cost (first request)))
  (if (= request "WAIT")
    (println "WAITING P")
    (let [pods (first request)
          node (second request)
          world @world-agent
          ;;at the given node get the current number of pods owned by the player
          current-pods (((world node) :pods) (:id @p1))
          current-plat (:platinum @p1)
          pod-plat-cost (* pod-cost pods)]
      (println "my-plat: " current-plat)
      (println "plat-needed: " pod-plat-cost)
      ;;verify movement is possible
      (if (>= current-plat pod-plat-cost)
        ;;move
        (when true
         (println "going to swap things")
         ;;add pods
         (swap! world-agent
                #(assoc-in ;;add to new location...
                  % ;;..having removed from old location
                  [node :pods (:id @p1)] (+ pods current-pods)))
         ;;decrease player platinum
         (swap! p1 #(assoc-in % [:platinum] (- current-plat pod-plat-cost))))
        (println "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST"))
      ;;(println "After: " (nth @world-agent 0))
      )))


(defn own
  "Applies ownership logic to each node in the world."
  [world-atom]
  (println "owning")
  )

(defn distrib
  "Enqueues player platinum amount"
  [[p1 p2 p3 p4] world]
  ;;iterate over world, find matching player-id and owner if any update income accordingly
  (println "distributing")
  (loop [node world]
    (if (nil? (first node))
      nil
      (let [p*
            (cond
             (= (:id @p1) (:owner (first node))) p1
             (= (:id @p2) (:owner (first node))) p2
             (= (:id @p3) (:owner (first node))) p3
             (= (:id @p4) (:owner (first node))) p4
             :default nil)]
        (if (not (nil? p*))
          (swap! p* #(assoc-in % [:income] (+ (:income %) (:income (first node))))))
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

(defn mprintln-orig
  "Mimics the functionality of the println function."
  [player request world-agent]
  (println "REQUEST: " request)
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement (:id player) #(place player request))
      (enqueue-command :placement (:id player)
                       #(map place
                             (cycle [player])
                             (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                             (cycle [world-agent])))
      ;;(place player request)
      ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
      )

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement(:id player) #(move player request world-agent))
      (enqueue-command :movement (:id player) #(map move
                                          (cycle [player])
                                          (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                                          (cycle [world-agent])))
      ;;(move player request)
      ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
      )
    )
  (swap! moved #(not %))
  )


(defn mprintln-test
  "Mimics the functionality of the println function."
  [player request world-agent]
  (println "REQUEST: " request)
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement (:id player) #(place player request world-agent))
      (map enqueue-command
           (cycle [:placement])
           (cycle [(:id player)])
           #(map place
                 (cycle [player])
                 (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                 (cycle [world-agent])))
      ;;(place player request)
      ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
      )

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement(:id player) #(move player request world-agent))
      (map enqueue-command
           (cycle [:movement])
           (cycle [(:id player)])
           #(map move
                 (cycle [player])
                 (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                 (cycle [world-agent])))
      ;;(move player request)
      ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
      )
    )
  (swap! moved #(not %)))
;; (apply conj [] (map parse-int ["5" "3" "2"]))




(defn mprintln
  "Mimics the functionality of the println function."
  [player request world-agent]
  (println "REQUEST: " request "request type placement? " @moved)
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement (:id @player) (partial place player request))
      (loop [vec-req (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))]
        (when (not (empty? vec-req))
          (enqueue-command :placement (:id @player) (partial place player (first vec-req)))
          (recur (next vec-req))))
      ;; (map enqueue-command
      ;;      (cycle [:placement])
      ;;      (cycle [(:id @player)])
      ;;      #(map partial (cycle 'place)
      ;;            (cycle [player])
      ;;            (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
      ;;            (cycle [world-agent])))
      )

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement (:id @player) (partial move player request))
      (loop [vec-req (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))]
        (when (not (empty? vec-req))
          (enqueue-command :movement (:id @player) (partial move player (first vec-req)))
          (recur (next vec-req))))
      ;;(move player request)
      ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
      )
    )
  (swap! moved #(not %)))

(defn mprintln2
  "Mimics the functionality of the println function."
  [player request world-agent]
  (println "REQUEST: " request "request type placement? " @moved)
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement (:id @player) (partial place player request))
      (map enqueue-command
           (cycle [:placement])
           (cycle [(:id @player)])
           #(map partial (cycle 'place)
                 (cycle [player])
                 (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                 (cycle [world-agent])))
      ;;(place player request)
      ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
      )

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement (:id @player) (partial move player request))
      (map enqueue-command
           (cycle [:movement])
           (cycle [(:id @player)])
           #(map partial (cycle 'move)
                 (cycle [player])
                 (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
                 (cycle [world-agent])))
      ;;(move player request)
      ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
      )
    )
  (swap! moved #(not %)))




(defn run-commands-test
  [type world-atom placement-commands move-commands]
  (condp = type
      :placement (loop [player @placement-commands] ;;for each player
                   ;;loop over the list of commands for the player
                   (loop [command (second (first player))]
                     (println "commands: " command)
                     (println "command: " (first command))
                       ;; (doall (do ((first command) "things")))

                     (when (not (empty? command))
                       ;; (println "first command: " ((first command)))
                       (println "calling command...")
                       ((first command) world-atom)
                       (println "command called")
                       (recur (next command))))
                    ;; (swap! world-atom #((first command) %)) ;;update the world with command results

                   (when (not (empty? player))
                     (recur (next player))))
      :movement (loop [player @move-commands]
                  (loop [command (second (first player))]
                     (println "command: " (first command))
                    (when (not (empty? command))
                       ((first command) world-atom)
                      ;; (swap! world-atom #((first command) %))
                      (recur (next command))))
                  (when (not (empty? player))
                    (recur (next player))))))
(defn place-test
  "Applies placement logic to each buy request by each player"
  [p1 request world-agent]
  ;; (println "Player " (:id p1) " placing units at " (:id node))
  ;; (println "old node: " (@world-agent (:id node)))
  (println "num pods: " (first request))
  (println "nodeId: " (second request))
  (println "node: " (@world-agent (second request)))
  (println "node pods: " ((@world-agent (second request)) :pods) )
  (println "player id: " (:id p1))
  (println "player pods: " (((@world-agent (second request)) :pods) (:id p1)))
  (println "player plat: " (:platinum p1))
  (println "BEFORE: " (nth @world-agent 0))
  (if (= request "WAIT")
    (println "WAITING")

  (let [pods (first request)
        node (second request)
        world @world-agent
        ;;at the given node get the current number of pods owned by the player
        current-pods (((world node) :pods) (:id p1))
        current-plat (:platinum p1)
        pod-plat-cost (* pod-cost pods)]
    ;;verify movement is possible
    (if (>= current-plat pod-plat-cost)
      ;;move
      (doall
       ;;add pods
       (swap! world-agent
             #(assoc-in ;;add to new location...
              % ;;..having removed from old location
              [node :pods (:id p1)] (+ pods current-pods)))
       ;;decrease player platinum
       (swap! p1 #(assoc-in % [:platinum] (- current-plat pod-plat-cost))))
      (println "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST"))
    (println "After: " (nth @world-agent 0)))
  ;;(println "new node: " (@world-agent (:id node)))

  ))

(defn place-orig
  "Applies placement logic to each buy request by each player"
  [p1 request world-agent]
  ;; (println "Player " (:id p1) " placing units at " (:id node))
  ;; (println "old node: " (@world-agent (:id node)))
  (if (= request "WAIT")
    (println "WAITING")
  (let [pods (first request)
        node (second request)
        world @world-agent
        ;;at the given node get the current number of pods owned by the player
        current-pods (((world node) :pods) (:id p1))
        current-plat (:platinum p1)]
    ;;verify movement is possible
    (if (>= current-plat pod-cost)
      ;;move
      (swap! world-agent
             (assoc-in ;;add to new location...
              world ;;..having removed from old location
              [node :pods (:id p1)] (inc current-pods)))
      (println "ERROR NOT ENOUGH PLATINUM FOR PLACEMENT REQUEST")))
  ;;(println "new node: " (@world-agent (:id node)))
  ))
