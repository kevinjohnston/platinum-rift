(ns platinum-rift.mimic
  (:require [platinum-rift.constants :refer :all]))

;; (def move-requests (atom [[[6 0 1]]  ["WAIT"] ["WAIT"] ["WAIT"]]))
;; (def placement-requests (atom [[[6 0]]  [[3 0] [2 1]] ["WAIT"] ["WAIT"]]))
(def move-requests (atom [[] [] [] []]))
(def placement-requests (atom [[] [] [] []]))
(def read-queue (atom [])) ;;to mimic ds structure in actual game
(def move-commands (atom {0 []
                            1 []
                            2 []
                            3 []}))
(def placement-commands (atom {0 []
                            1 []
                            2 []
                            3 []})) ;;requests made by the players to the game

(def moved (atom true))

;; (defn dequeue [q]
;;   (if-let [[f & r] (seq @q)]
;;     (do (reset! q r) f)
;;     []))

;; (defn dequeue [q]
;;   (let [[f & r]
;;         (seq @q)]
;;     (do (reset! q r) f)
;;     ))

;; (defn make-queue []
;;   (atom []))

;; (defn enqueue [q x]
;;   (swap! q conj x))

(defn parse-int [s]
  ;; (println "parse-int: " s)
   (Integer. (re-find  #"\d+" s )))

(defn enqueue-command
  [type p1 request]
  ;; (println "Enqueing request: " request " type: " type " for player num: " p1)
  (if (= type :movement)
    (swap! move-requests #(assoc-in % [p1] (conj (% p1) request)))
    (swap! placement-requests #(assoc-in % [p1] (conj (% p1) request)))))

(defn mprintln
  "Mimics the functionality of the println function."
  [player-id request]
  ;; (println "REQUEST: " request)
  (swap! moved #(not %))
  (if @moved ;;determine if player has already moved this turn
    ;;process placement command
    (if (= request "WAIT")
      (enqueue-command :placement player-id request)
      (map enqueue-command (cycle [:placement]) (cycle [player-id]) (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))))

    ;;process movement command
    (if (= request "WAIT")
      (enqueue-command :movement player-id request)
      (map enqueue-command (cycle [:movement]) (cycle [player-id]) (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " "))))))))
  )

(let [request "10 0"
      player-id 0]
  (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
  ;; (apply conj [] (map parse-int (reduce conj [] (.split request " "))))
  (conj ([[] []] 0) '(10 0))
  (map enqueue-command (cycle [:placement]) (cycle [player-id]) (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " "))))))
)

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






;; (macroexpand '(enqueue-command a b c) )





(defn mread
  "Mimics what will be returned by the read function in game."
  [read-queue]
  (if (empty? @read-queue)
    (while (empty? @read-queue)
      (Thread/sleep 1))
    ;;(println "Read: " (first @read-queue))
    )
  (let [ret (first @read-queue)]
    (swap! read-queue #(next %))
    ret))

(defn mwrite-orig
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [& info]
  (when debug
    (println "writing: " info))
  (swap! read-queue #(reduce conj % info)))

(defn mwrite
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [read-queue & info]
  ;; (when debug
  ;;   (println "writing: " info)
  ;;   (if (= 1 (rand-int 100))
  ;;     (println "total read-queue: " @read-queue)))
  (swap! read-queue #(concat % info)))

(defn mwrite-backup
  "Adds information at the end of the queue to be accessed by turn loop. Should only be called in core.clj"
  [read-queue & info]
  (when debug
    (println "writing: " info)
    (println "total read-queue: " @read-queue))
  (swap! read-queue #(reduce conj % info)))


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





;; (apply conj [] (map parse-int ["5" "3" "2"]))
;; (defn mprintln-test
;;   "Mimics the functionality of the println function."
;;   [player request world-agent]
;;   (println "REQUEST: " request)
;;   (if @moved ;;determine if player has already moved this turn
;;     ;;process placement command
;;     (if (= request "WAIT")
;;       (enqueue-command :placement (:id player) #(place player request world-agent))
;;       (map enqueue-command
;;            (cycle [:placement])
;;            (cycle [(:id player)])
;;            #(map place
;;                  (cycle [player])
;;                  (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
;;                  (cycle [world-agent])))
;;       ;;(place player request)
;;       ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
;;       )

;;     ;;process movement command
;;     (if (= request "WAIT")
;;       (enqueue-command :movement(:id player) #(move player request world-agent))
;;       (map enqueue-command
;;            (cycle [:movement])
;;            (cycle [(:id player)])
;;            #(map move
;;                  (cycle [player])
;;                  (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
;;                  (cycle [world-agent])))
;;       ;;(move player request)
;;       ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
;;       )
;;     )
;;   (swap! moved #(not %)))



;; (defn mprintln
;;   "Mimics the functionality of the println function."
;;   [player request]
;;   (println "REQUEST: " request "request type placement? " @moved)
;;   (if @moved ;;determine if player has already moved this turn
;;     ;;process placement command
;;     (if (= request "WAIT")
;;       (enqueue-command :placement (:id @player) (partial place player request))
;;       (loop [vec-req (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))]
;;         (when (not (empty? vec-req))
;;           (enqueue-command :placement (:id @player) (partial place player (first vec-req)))
;;           (recur (next vec-req)))))

;;     ;;process movement command
;;     (if (= request "WAIT")
;;       (enqueue-command :movement (:id @player) (partial move player request))
;;       (loop [vec-req (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))]
;;         (when (not (empty? vec-req))
;;           (enqueue-command :movement (:id @player) (partial move player (first vec-req)))
;;           (recur (next vec-req))))))
;;   (swap! moved #(not %)))

;; (defn mprintln2
;;   "Mimics the functionality of the println function."
;;   [player request world-agent]
;;   (println "REQUEST: " request "request type placement? " @moved)
;;   (if @moved ;;determine if player has already moved this turn
;;     ;;process placement command
;;     (if (= request "WAIT")
;;       (enqueue-command :placement (:id @player) (partial place player request))
;;       (map enqueue-command
;;            (cycle [:placement])
;;            (cycle [(:id @player)])
;;            #(map partial (cycle 'place)
;;                  (cycle [player])
;;                  (partition 2 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
;;                  (cycle [world-agent])))
;;       ;;(place player request)
;;       ;;(map place (partition 2 (map parse-int (reduce conj [] (.split request " ")))))
;;       )

;;     ;;process movement command
;;     (if (= request "WAIT")
;;       (enqueue-command :movement (:id @player) (partial move player request))
;;       (map enqueue-command
;;            (cycle [:movement])
;;            (cycle [(:id @player)])
;;            #(map partial (cycle 'move)
;;                  (cycle [player])
;;                  (partition 3 (apply conj [] (map parse-int (reduce conj [] (.split request " ")))))
;;                  (cycle [world-agent])))
;;       ;;(move player request)
;;       ;;(map move (partition 3 (map parse-int (reduce conj [] (.split request " ")))))
;;       )
;;     )
;;   (swap! moved #(not %)))
