(ns platinum-rift.world
  (:require [platinum-rift.graph :as graph]
            [platinum-rift.constants :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODEL THE GAME WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def world (atom {}))
(def world (atom []))
(def graph-world (atom (graph/make-graph #{} {})))
(def shortest-paths (atom {})) ;;map of paths sorted by shortest distance from each starting node

(declare reset-world)

(declare edge-vector)
(declare get-shortest-path)
(declare find-short-paths)
(declare nearby-nodes)

(defn setup-graph-world
  "Creates the defacto graph world, hopefully this is identical to the true game world (though logic shouldn't depend on it)."
  []
  (swap! graph-world #(when % (graph/make-graph #{} {})))
  (swap! graph-world #(when %
                        (loop [graph (reduce graph/add-nodes (graph/make-graph #{} {}) (range num-nodes)) ;;add all nodes
                               edges edge-vector]
                          (if (empty? edges)
                            graph
                            (recur (graph/add-edge graph (first (first edges)) (second (first edges))) ;;add all links
                                   (next edges)))))))


(defn new-node
  "Returns a new node"
  ([node-id] (new-node node-id 0))
  ([node-id income]
     {:id node-id
      :source-value 0 ;;reset at beginning of every turn
      :scalar-value 0 ;;recalculated from nearby source values every turn
      :owner -1 ;;id of player controlling this, neutral '1000' by default
      :open-liberties 0 ;;number of touching nodes owners by non-neutral enemy player
      :total-liberties 0 ;;number of bordering nodes, never reset
      :income income
      :pods [0 0 0 0]}))

(defn blank-world
  "Returns a zone map representing the whole world."
  ([] (blank-world 154))
  ([num-nodes] (loop [node-id 0
         acc []]
    (if (< node-id num-nodes)
      (recur (inc node-id)
             (conj acc (new-node node-id))) ;;add another node
      acc))))

(defn add-income
  "Takes in the world (a vector of maps, one for each node)."
  [world]
  (loop [res-remain max-res
         next-node (rand-int num-nodes)
         acc world]
    (if (= 0 res-remain)
      acc;;return the updated world
      (if (>= (:income (world next-node)) max-inc)
        (recur res-remain (rand-int num-nodes) acc)
        (recur (dec res-remain) (rand-int num-nodes) (assoc-in acc [next-node :income] (inc (:income (acc next-node)))))))))

(defn node-liberties
  [node world]
  "Returns the number of liberties, bordering nodes not owned by the same player, of a given node."
  (let [player ((nth world node) :owner)]
    ;;get number of nearby nodes not owned by player
    (count
     (filter #(not (= % player))
             ;;get owners of those ids
             (map #((nth world %) :owner)
                  ;;get all bordering node ids
                  (map last (next (nearby-nodes 2 0))))))))

(defn reset-world
  ""
  []
  (swap! world (fn [_] (add-income (blank-world))))
  (setup-graph-world)
  ;;find all shortest paths
  (swap! shortest-paths (fn [_] (find-short-paths @graph-world)))
  world)

(defn new-world
  ""
  ([] (new-world 154))
  ([num-nodes]
     (add-income (blank-world num-nodes))
     (setup-graph-world)
     ;;find all shortest paths
     (swap! shortest-paths (fn [_] (find-short-paths @graph-world)))
     (add-income (blank-world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-short-paths
  ([graph] (find-short-paths graph num-nodes))
  ([graph num-nodes]
  (loop [node num-nodes
         acc {}]
    (if (< node 0)
      acc
      (recur (dec node)
             (assoc acc node (sort-by count (graph/breadth-first-traversal-with-path graph node))))))))

(defn nearby-nodes
  "Returns all paths with distance <= radius from given origin-node."
  [radius origin-node]
  (filter (fn [_] (<= (count _) radius)) (@shortest-paths origin-node)))

(defn get-shortest-path
  "Returns vector of nodes for shortest path between node1 and node2"
  ([p1 p2] (get-shortest-path p1 p2 @shortest-paths))
  ([p1 p2 shortest-paths] (first (filter #(= (last %) p2) (shortest-paths p1)))))


(defn get-global-min
  "Taking in a scalar world returns the global minimum"
  [world]
  (first (sort-by #(:scalar-value %) world)))

(defn get-local-min
  "Returns the lowest point within a given radius of the world from a given node id."
  [radius node-id world]
  ;;sort by scalar-value and return lowest (first)
  (first (sort-by #(:scalar-value %)
                  ;;get node maps for the ids
                  (map #(nth world %)
                       ;;get node ids of nearby nodes
                       (map last (nearby-nodes radius node-id))))))

(defn is-distinct?
  "Returns true or false depending on if the two nodes have a path between them."
  [n1 n2 shortest-paths]
  (nil? (get-shortest-path (:id n1) (:id n2))))

(defn get-continents
  "Returns a vector of vectors which are composed of nodes."
  [world shortest-paths]
  (loop [acc-continents []
         nodes world]
    (if (empty? nodes)
      acc-continents ;;done, return continents
    ;;get the position of the continent that contains this node, if any
      (let [cont-contains (loop [next-continent (dec (count acc-continents))]
                            (if (< next-continent 0)
                              nil
                              (if (is-distinct? (first (nth acc-continents next-continent))
                                                (first nodes)
                                                shortest-paths)
                                (recur (dec next-continent))
                                next-continent)))]
        ;;check if any existing continent is connected to the node
        (if (or (empty? acc-continents)
                (nil? cont-contains))
          ;;if not, create a new continent with this node at the beginning
          (recur (conj acc-continents [(first nodes)])
                 (next nodes))
          ;;if so, add node to that continent
          (recur (assoc acc-continents
                   cont-contains
                   (conj (nth acc-continents cont-contains) (first nodes)))
                 (next nodes)))))))


;;shortest paths representation
;; [0 ([0] [0 1] [0 1 3] [0 1 2] [0 1 3 8] [0 1 3 4] [0 1 2 7] [0 1 3 8 15] [0 1 3 8 14] [0 1 3 8 9] [0 1 3 4 5] [0 1 2 7 23] [0 1 2 7 13] [0 1 3 8 15 22] [0 1 3 8 15 21] [0 1 3 8 15 16] [0 1 3 8 14 20] [0 1 3 8 9 10] [0 1 3 4 5 6] [0 1 2 7 13 19] [0 1 3 8 15 22 29] [0 1 3 8 15 21 28] [0 1 3 8 15 16 17] [0 1 3 8 14 20 27] [0 1 3 8 9 10 11] [0 1 3 8 15 22 29 38] [0 1 3 8 14 20 27 37] [0 1 3 8 9 10 11 12] [0 1 3 8 14 20 27 37 43] [0 1 3 8 9 10 11 12 18] [0 1 3 8 14 20 27 37 43 46] [0 1 3 8 9 10 11 12 18 24] [0 1 3 8 14 20 27 37 43 46 49] [0 1 3 8 14 20 27 37 43 46 48] [0 1 3 8 14 20 27 37 43 46 47] [0 1 3 8 9 10 11 12 18 24 30] [0 1 3 8 9 10 11 12 18 24 25] [0 1 3 8 9 10 11 12 18 24 30 39] [0 1 3 8 9 10 11 12 18 24 30 31] [0 1 3 8 9 10 11 12 18 24 25 26] [0 1 3 8 9 10 11 12 18 24 30 39 44] [0 1 3 8 9 10 11 12 18 24 30 39 40] [0 1 3 8 9 10 11 12 18 24 30 31 32] [0 1 3 8 9 10 11 12 18 24 30 39 44 45] [0 1 3 8 9 10 11 12 18 24 30 39 40 41] [0 1 3 8 9 10 11 12 18 24 30 31 32 33] [0 1 3 8 9 10 11 12 18 24 30 39 40 41 42] [0 1 3 8 9 10 11 12 18 24 30 31 32 33 34] [0 1 3 8 9 10 11 12 18 24 30 31 32 33 34 35] [0 1 3 8 9 10 11 12 18 24 30 31 32 33 34 35 36])]

;; (let [acc-continents [[{:id 0} {:id 1} {:id 2}] [{:id 3}] [{:id 4} {:id 5}] [{:id 6} {:id 7}] [{:id 8} {:id 9}]]
;;       cont-contains 3
;;       nodes [{:id 10}]]
;; (assoc acc-continents cont-contains (conj (nth acc-continents cont-contains) (first nodes)))
;; )
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

   [32 33]
   [32 40]
   [32 41]

   [33 34]
   [33 41]
   [33 42]

   [34 35]
   [34 42]

   [35 36]

   ;;36

   [37 43]

   ;;38

   [39 40]
   [39 44]

   [40 41]
   [40 44]
   [40 45]

   [41 42]
   [41 45]

   ;;42

   [43 46]

   [44 45]

   ;;45

   [46 47]
   [46 48]
   [46 49]
   ;;47

   [48 49]

   ;;49

   [50 51]
   [50 54]
   [50 55]

   [51 55]
   [51 56]

   [52 53]
   [52 59]

   [53 59]

   [54 55]
   [54 60]
   [54 61]

   [55 56]
   [55 61]
   [55 62]

   [56 62]
   [56 63]

   [57 67]

   [58 68]

   [59 69]
   [59 70]

   [60 61]
   [60 71]

   [61 62]
   [61 71]
   [61 72]

   [62 63]
   [62 72]
   [62 73]

   [63 64]
   [63 73]
   [63 74]

   [64 65]
   [64 74]
   [64 75]

   [65 66]
   [65 75]
   [65 76]

   [66 76]
   [66 77]

   [67 78]

   [68 69]
   [68 79]

   [69 70]
   [69 79]
   [69 80]

   [70 80]
   [70 81]

   [71 72]
   [71 82]
   [71 83]

   [72 73]
   [72 83]
   [72 84]

   [73 74]
   [73 84]
   [73 85]

   [74 75]
   [74 85]
   [74 86]

   [75 76]
   [75 86]
   [75 87]

   [76 77]
   [76 87]
   [76 88]

   [77 88]

   [78 89]

   [79 80]
   [79 90]
   [79 91]

   [80 81]
   [80 91]
   [80 92]

   [81 82]
   [81 92]
   [81 93]

   [82 83]
   [82 93]
   [82 94]

   [83 84]
   [83 94]
   [83 95]

   [84 85]
   [84 95]
   [84 96]

   [85 86]
   [85 96]

   [86 87]

   [87 88]

   ;;88

   [89 97]

   [90 91]
   [90 98]
   [90 99]

   [91 92]
   [91 99]
   [91 100]

   [92 93]
   [92 100]
   [92 101]

   [93 94]
   [93 101]
   [93 102]

   [94 95]
   [94 102]
   [94 103]

   [95 96]
   [95 103]

   ;;96

   [97 104]

   [98 99]
   [98 105]
   [98 106]

   [99 100]
   [99 106]
   [99 107]

   [100 101]
   [100 107]
   [100 108]

   [101 102]
   [101 108]
   [101 109]

   [102 103]
   [102 108]
   [102 110]

   [103 110]
   [103 111]

   [104 113]

   [105 106]
   [105 114]

   [106 107]
   [106 114]
   [106 115]

   [107 108]
   [107 115]
   [107 116]

   [108 109]
   [108 116]
   [108 117]

   [109 110]
   [109 117]
   [109 118]

   [110 111]
   [110 118]
   [110 119]

   [111 112]
   [111 119]

   ;;112

   ;;113

   [114 115]
   [114 120]

   [115 116]
   [115 120]
   [115 121]

   [116 117]
   [116 121]
   [116 122]

   [117 118]
   [117 122]
   [117 123]

   [118 119]
   [118 123]
   [118 124]

   [119 124]
   [119 125]

   [120 121]
   [120 128]
   [120 129]

   [121 122]
   [121 129]
   [121 130]

   [122 123]
   [122 130]
   [122 131]

   [123 124]
   [123 131]
   [123 132]

   [124 125]
   [124 132]
   [124 133]

   [125 126]
   [125 133]

   [126 127]

   [127 134]

   [128 129]
   [128 137]

   [129 130]
   [129 137]

   [130 131]

   [131 132]

   [132 133]

   ;;133

   [134 138]

   [135 136]
   [135 139]
   [135 140]

   [136 140]

   [137 141]
   [137 142]

   [138 139]
   [138 144]

   [139 140]
   [139 145]

   [140 145]
   [140 146]

   [141 142]
   [141 148]

   [142 148]

   [143 150]

   [144 151]

   [145 146]

   [146 147]

   [147 152]

   ;;148

   [149 150]

   ;;150

   ;;151

   [152 153]

   ;;153
   ])







;; (def q (make-graph #{1 2 3 4} {}))
;; (def q2 (add-edge (add-edge q 3 1) 1 4))
;; (def q3 (add-edges q [[1 2] [1 3] [3 4]]))
;; (def q4 (add-edges (add-nodes q3 5 6) [[4 5] [1 5] [4 6]]))
;; (def q5 (add-edges (make-graph #{1 2 3 4 5 6})
;;                    [[1 2] [2 3] [3 4] [4 5] [5 6] [6 1]]))
;; (def q6 (reduce #(add-node %1 %2) (make-graph) (range 1000)))
;; (def q7 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
;;                 q6
;;                 (take 10000 (repeatedly #(vector (rand-int 1000)
;;                                                  (rand-int 1000))))))
;; (neighbors q7 1)
;; (take-while (complement #{2}) (breadth-first-traversal q7 1))
;; (def q8 (reduce #(add-node %1 (str "node" %2)) (make-graph) (range 1000)))
;; (def q9 (reduce (fn [g [n1 n2]] (add-edge g
;;                                          (str "node" n1)
;;                                          (str "node" n2)))
;;                 q8
;;                 (take 10000 (repeatedly #(vector (rand-int 1000)
;;                                                  (rand-int 1000))))))
;; (def q10 (add-edges (make-graph (set (range 1 10)))
;;                     [[1 2] [1 3] [3 4] [4 5] [2 6] [7 8] [8 9] [7 9]]))
;; (map clojure.pprint/pprint
;;      (map #(vector %
;;                    (graph-distance-matrix %))
;;           (connected-components q10)))
;; (defn neighborfn [g]
;;   (fn [n] (neighbors g n)))
;; (def q11 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
;;                  (reduce #(add-node %1 %2) (make-graph) (range 50))
;;                  (take 100 (repeatedly #(vector (rand-int 50)
;;                                                 (rand-int 50))))))
;; ;;; this breaks connected components!!! puts a nil in node-set. should not do that...
;; (def q12 (add-edges (make-graph (set (range 1 3)))
;;                     [[1 1] [1 2] ]))
;; (def q13 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
;;                  (reduce #(add-node %1 %2) (make-graph) (range 50))
;;                  (take 60 (repeatedly #(vector (rand-int 50)
;;                                                (rand-int 50))))))
;; (connected-components q12)
;; (partition-graph q12 1)

;; (breadth-first-traversal-with-path q12 1)
