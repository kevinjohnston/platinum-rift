(ns platinum-rift.world
  (:require [platinum-rift.graph :as graph]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODEL THE GAME WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def next-zone-id (atom 1001))
(def p1 (atom {}))
(def p2 (atom {}))
(def p3 (atom {}))
(def p4 (atom {}))
(def starting-plat 200)
(def world (atom {}))
(def graph-world (atom (graph/make-graph #{} {})))
(def shortest-paths (atom {})) ;;map of paths sorted by shortest distance from each starting node
(def num-nodes 154)

(declare reset-world)

(defn blank-world
  "Returns a zone map representing the whole world."
  []
  (swap! next-zone-id #(if false % (num 1001)))
  (swap! world #(when %
                  {:id 1000
                   :sub-zones [(blank-NoA)]
                   :priority 0
                   :last-eval 0
                   :value 0
                   :total-liberties 0
                   :owner 0
                   :open-liberties 0})))

(defn blank-NoA
  []
  {:id (new-zone-id)
   :sub-zones [(new-zone [0 1 2 3])
                (new-zone [7 13 19 14 20])
                (new-zone [4 8 9 15])
                (new-zone [5 10 6 11 12])
                (new-zone [16 22 17 23])
                (new-zone [29 38 21 28])
                (new-zone [27 37 43])
                (new-zone [46 47 48 49])
                ];;todo#{ (set of zone-ids)}
   :priority 0;;(int to indicate how important this territory is to the strategy gets re-evaluated from time to time)
   :last-eval -1;;(turn priority was last set)
   :value 0;;(accumulated total of platinum bars in all sub-zones)
   :total-liberties 0;;(total number of adjacent zones)
   :open-liberties 0;;(number of adjacent zones that aren't controlled by you)
   :owner 0
   })

;; (defn get-shortest-path-graph
;;   "Returns vector of nodes for shortest path between 1 and 2"
;;   [graph p1 p2]
;;   (first (filter #(= (last %) p2 ) )))


(defn blank-SoA
  [])

(defn blank-Ant
  [])

(defn blank-Afr
  [])

(defn blank-Asi
  [])

(defn blank-Eur
  [])

(defn blank-Aus
  [])

(defn blank-Jap
  [])

(defn blank-NA
  [])

(defn blank-NA
  [])


(defn new-zone [ids]
  (if (not (instance? Long ids))
    {:id (new-zone-id)
     :sub-zones (map new-zone ids)
     :priority 0
     :last-eval 0
     :value 0
<<<<<<< Updated upstream
=======
     :income (rand-int 6)
>>>>>>> Stashed changes
     :total-liberties 0
     :owner 0
     :open-liberties 0}

    {:id ids
     :sub-zones nil
     :priority 0
     :last-eval 0
     :value 0
<<<<<<< Updated upstream
=======
     :income (rand-int 6)
>>>>>>> Stashed changes
     :total-liberties 0
     :owner 0
     :open-liberties 0}))

(defn new-zone-id
  []
  (swap! next-zone-id #(inc %)))

(defn new-player
  [id]
  {:player id
   :platinum starting-plat
   :pods 0
   :income 0
   :territories 0})

(defn setup-players
  ""
  [num]
  (swap! p1 #(when % (new-player :p1)))
  (swap! p2 #(when % (new-player :p2)))
  (when (> num 2)
    (swap! p3 #(when % (new-player :p3)))
    (swap! p4 #(when % (new-player :p4)))))




(defn reset-world
  ""
  []
  (blank-world)
  (setup-graph-world)
  ;;find all shortest paths
  (swap! shortest-paths (fn [_] (find-short-paths @graph-world)))
  (setup-players 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-short-paths
  [graph]
  (loop [node num-nodes
         acc {}]
    (if (< node 0)
      acc
      (recur (dec node)
             (assoc acc node (sort-by count (graph/breadth-first-traversal-with-path graph node)))))))

(defn nearby-nodes
  "Returns all paths with distance <= radius from given origin-node."
  [radius origin-node]
  (filter (fn [_] (<= (count _) radius)) (@shortest-paths origin-node)))

(defn get-shortest-path
  "Returns vector of nodes for shortest path between node1 and node2"
  [p1 p2]
  (first (filter #(= (last %) p2) (@shortest-paths p1))))



;; {:id 1002,
;;  :sub-zones #{

;;               {:id 1010,
;;                :sub-zones (
;;                            {:id 0, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 1, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 2, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 3, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1007,
;;                :sub-zones (
;;                            {:id 29, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 38, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 21, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 28, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties
;;                0}
;;               {:id 1004,
;;                :sub-zones (
;;                            {:id 16, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 22, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 17, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 23, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1009,
;;                :sub-zones (
;;                            {:id 4, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 8, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 9, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 15, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1006,
;;                :sub-zones (
;;                            {:id 46, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 47, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 48, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 49, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1003,
;;                :sub-zones (
;;                            {:id 5, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 10, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 6, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 11, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 12, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0,
;;                :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1008,
;;                :sub-zones (
;;                            {:id 7, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 13, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 19, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 14, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 20, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;               {:id 1005,
;;                :sub-zones (
;;                            {:id 27, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 37, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}
;;                            {:id 43, :sub-zones nil, :priority 0, :last-eval 0, :value 0, :total-liberties 0, :open-liberties 0}),
;;                :priority 0, :last-eval 0, :value
;;                0, :total-liberties 0, :open-liberties 0}}, :priority 0, :last-eval -1, :value 0, :total-liberties 0, :open-liberties 0}

(defn s-dist
  "Given a vector of starting x, y coordinates and a vector of destination coordinates returns the shortest number of moves to traverse."
  [[xS yS] [xD yD]]
  (/
   (+
    (Math/abs (- xD xS))
    (Math/abs (- yD yS)))
   2))

(defn setup-graph-world []
  (swap! graph-world #(when % (graph/make-graph #{} {})))
  (swap! graph-world #(when %
                        (loop [graph (reduce graph/add-nodes (graph/make-graph #{} {}) (range num-nodes)) ;;add all nodes
                               edges edge-vector]
                          (if (empty? edges)
                            graph
                            (recur (graph/add-edge graph (first (first edges)) (second (first edges))) ;;add all links
                                   (next edges)))))))



(def edge-vector
  [[0 1]

   [1 2]
   [1 3]

   [2 7]

   [3 4]
   [3 8]

   [4 8]
   [4 9]
   [4 5]

   [5 9]
   [5 10]
   [5 6]

   [6 10]
   [6 11]

   [7 8]
   [7 13]
   [7 14]

   [8 9]
   [8 14]
   [8 15]

   [9 10]
   [9 15]
   [9 16]

   [10 11]
   [10 16]
   [10 17]

   [11 12]
   [11 17]

   [12 18]

   [13 14]
   [13 19]
   [13 20]

   [14 15]
   [14 20]
   [14 21]

   [15 16]
   [15 21]
   [15 22]

   [16 17]
   [16 22]
   [16 23]

   [7 23]

   [18 24]

   [19 20]
   [19 27]

   [20 21]
   [20 27]
   [20 28]

   [21 22]
   [21 28]
   [21 29]

   [22 23]
   [22 29]

   ;;23

   [24 25]
   [24 30]

   [25 26]
   [25 30]
   [25 31]

   [26 31]
   [26 32]

   [27 28]
   [27 37]

   [28 29]
   [28 38]

   [29 38]

   [30 31]
   [30 39]

   [31 32]
   [31 39]
   [31 40]

   ;;... 36

   [37 43]

   ;;38


   ;;...43
   [43 46]
   ;;...46

   [46 47]
   [46 48]
   [46 49]
   ;;47
   ;;48
   ;;49
   ])







(def q (make-graph #{1 2 3 4} {}))
(def q2 (add-edge (add-edge q 3 1) 1 4))
(def q3 (add-edges q [[1 2] [1 3] [3 4]]))
(def q4 (add-edges (add-nodes q3 5 6) [[4 5] [1 5] [4 6]]))
(def q5 (add-edges (make-graph #{1 2 3 4 5 6})
                   [[1 2] [2 3] [3 4] [4 5] [5 6] [6 1]]))
(def q6 (reduce #(add-node %1 %2) (make-graph) (range 1000)))
(def q7 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                q6
                (take 10000 (repeatedly #(vector (rand-int 1000)
                                                 (rand-int 1000))))))
(neighbors q7 1)
(take-while (complement #{2}) (breadth-first-traversal q7 1))
(def q8 (reduce #(add-node %1 (str "node" %2)) (make-graph) (range 1000)))
(def q9 (reduce (fn [g [n1 n2]] (add-edge g
                                         (str "node" n1)
                                         (str "node" n2)))
                q8
                (take 10000 (repeatedly #(vector (rand-int 1000)
                                                 (rand-int 1000))))))
(def q10 (add-edges (make-graph (set (range 1 10)))
                    [[1 2] [1 3] [3 4] [4 5] [2 6] [7 8] [8 9] [7 9]]))
(map clojure.pprint/pprint
     (map #(vector %
                   (graph-distance-matrix %))
          (connected-components q10)))
(defn neighborfn [g]
  (fn [n] (neighbors g n)))
(def q11 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                 (reduce #(add-node %1 %2) (make-graph) (range 50))
                 (take 100 (repeatedly #(vector (rand-int 50)
                                                (rand-int 50))))))
;;; this breaks connected components!!! puts a nil in node-set. should not do that...
(def q12 (add-edges (make-graph (set (range 1 3)))
                    [[1 1] [1 2] ]))
(def q13 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                 (reduce #(add-node %1 %2) (make-graph) (range 50))
                 (take 60 (repeatedly #(vector (rand-int 50)
                                               (rand-int 50))))))
(connected-components q12)
(partition-graph q12 1)

(breadth-first-traversal-with-path q12 1)
