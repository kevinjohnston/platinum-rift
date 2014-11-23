(ns Player
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAPH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol NodeSet
  (nodes [graph])
  (node? [graph node])
  (add-node [graph node])
  (remove-node [graph node])
  (neighbors [graph node]))
(defprotocol EdgeSet
  (edges [graph]
    [graph node])
  (edge? [graph node1 node2])
  (add-edge [graph edge]
    [graph node1 node2]
    [graph node1 node2 m])
  (remove-edge [graph edge]
    [graph node1 node2]))
(extend-protocol NodeSet
  clojure.lang.PersistentVector
  (nodes [v] v)
  (node? [v node] (some #{node} v))
  (neighbors [v node] (remove #{node} v)))
(defprotocol Edge
  (left [edge])
  (right [edge]))
(extend-protocol Edge
  clojure.lang.IPersistentVector
  (left [v] (first v))
  (right [v] (second v)))
;;; Arcs aren't supported yet. keep the protocol and some methods on it
;;; here for the moment though.
(defprotocol Arc
  (start [edge])
  (end [edge]))
(extend-protocol Arc
  clojure.lang.IPersistentVector
  (start [v] (first v))
  (end [v] (second v)))
(extend-protocol NodeSet
  clojure.lang.IPersistentMap
  (nodes [v] (::node-set v))
  (node? [v node] (get (nodes v) node))
  (add-node [g n]
    (assoc g ::node-set (conj (::node-set g) n)))
  (remove-node [v node]
    (let [edges-removed (reduce (fn [v e]
                                  (remove-edge v (left e) (right e)))
                                v
                                (edges v node))]
      (assoc v
        ::node-set (disj (::node-set edges-removed) node)
        ::edge-map (::edge-map edges-removed))))
  (neighbors [v node]
    (map #(first (neighbors % node)) (vals (get (::edge-map v) node)))))
(extend-protocol EdgeSet
  clojure.lang.IPersistentMap
  (edges ([g]
            (distinct (apply concat (map vals (vals (::edge-map g))))))
    ([g node]
       (vals (get (::edge-map g) node))))
  (edge? [g n1 n2]
    (some #(when (node? % n2) %)
          (vals (get (::edge-map g) n1))))
  (add-edge ([g edge]
               (let [n1 (left edge) n2 (right edge)]
                 (letfn [(add-1-edge [e n1 n2]
                           (assoc e n1 (assoc (or (get e n1) {}) n2 edge)))]
                   (if (some #(node? % n2) (edges g n1))
                     g
                     (assoc g ::edge-map
                            (add-1-edge
                             (add-1-edge (::edge-map g) n2 n1)
                             n1 n2))))))
    ([g n1 n2]
       (add-edge g n1 n2 nil))
    ([g n1 n2 meta-data-map]
       (let [obj (if meta-data-map
                   (with-meta [n1 n2] meta-data-map)
                   [n1 n2])]
         (letfn [(add-1-edge [e n1 n2 obj]
                   (assoc e n1 (assoc (or (get e n1) {}) n2 obj)))]
           (if (some #(node? % n2) (edges g n1))
             g
             (assoc g ::edge-map
                    (add-1-edge
                     (add-1-edge (::edge-map g) n2 n1 obj)
                     n1 n2 obj)))))))
  (remove-edge ([g edge]
                  (remove-edge g (left edge) (right edge)))
    ([g n1 n2]
       (letfn [(remove-1-edge [e n1 n2]
                 (let [inner (dissoc (or (get e n1) {}) n2)]
                   (if (seq inner)
                     (assoc e n1 inner)
                     (dissoc e n1))))]
         (assoc g ::edge-map
                (remove-1-edge
                 (remove-1-edge (::edge-map g) n2 n1)
                 n1 n2))))))
(defn add-edges [g edge-vec]
  (reduce (fn [g [n1 n2]] (add-edge g n1 n2)) g edge-vec))
(defn add-nodes [g & node-vec]
  (reduce (fn [g node] (add-node g node)) g node-vec))
(defn get-node [g node]
  (get (nodes g) node))
(defn make-graph
  ([] (assoc {} ::node-set #{} ::edge-map {}))
  ([nodes] (if nodes
             (assoc {} ::node-set nodes ::edge-map {})
             (make-graph)))
  ([nodes edge-vec] (add-edges (make-graph nodes) edge-vec)))
(defn breadth-first-traversal
  ([g start]
     (when (node? g start)
       (breadth-first-traversal g (conj (clojure.lang.PersistentQueue/EMPTY) start) #{})))
  ([g queue visited]
     (lazy-seq
      (when (peek queue)
        (let [node (peek queue)
              next (remove visited (neighbors g node))]
          (cons node
                (breadth-first-traversal g (into (pop queue) next)
                                         (into (conj visited node) next))))))))
;;; often it's nice to not just do a search, but keep a trail of the
;;; path of how one got to a particular node. we're also going to want
;;; to know the distance from the start, so we can just take the
;;; length of the path to get that.
(defn breadth-first-traversal-with-path
  ([g start]
     (when (node? g start)
       (breadth-first-traversal-with-path
        g (conj (clojure.lang.PersistentQueue/EMPTY) [start]) #{})))
  ([g queue visited]
     (lazy-seq
      (when (peek queue)
        (let [path (peek queue)
              node (last path)
              next (remove visited (neighbors g node))]
          (cons path (breadth-first-traversal-with-path
                      g
                      (into (pop queue)
                            (map #(conj path %) next))
                      (into (conj visited node) next))))))))
;;; note: In contrast to breadth-first-traversal above, in this case,
;;; since we're not returning a lazy-seq, it's trivial (?) to use
;;; recur instead of calling find-node recursively.
(defn find-node
  "finds the target node in g, either starting from a given node, or
  from an (arbitrarily chosen) first node. If target is unreachable from
  the starting node, returns nil."
  ([g target]
     (find-node g target (first (nodes g))))
  ([g target start]
     (when (node? g start)
       (find-node
        g target (conj (clojure.lang.PersistentQueue/EMPTY) [start]) #{})))
  ([g target queue visited]
     (when (peek queue)
       (let [path (peek queue)
             node (last path)
             next (remove visited (neighbors g node))]
         (if (= node target)
           path
           (recur g target (into (pop queue)
                                 (map #(conj path %) next))
                  (into (conj visited node) next)))))))
(defn depth-first-traversal
  ([g start]
     (when (node? g start)
       (depth-first-traversal g (list start) #{})))
  ([g queue visited]
     (lazy-seq
      (when (seq queue)
        (let [node (first queue)
              next (remove visited (neighbors g node))]
          (if-not (visited node)
            (cons node
                  (depth-first-traversal g (into (rest queue) next)
                                         (conj visited node)))
            (depth-first-traversal g (into (rest queue) next) visited)))))))
(defn depth-first-traversal-with-path
  ([g start]
     (when (node? g start)
       (depth-first-traversal-with-path g (list [start]) #{})))
  ([g queue visited]
     (lazy-seq
      (when (seq queue)
        (let [node (last (first queue))
              next (remove visited (neighbors g node))]
          (if-not (visited node)
            (cons (first queue)
                  (depth-first-traversal-with-path g
                                                   (into (rest queue)
                                                         (map #(conj (first queue) %) next))
                                                   (conj visited node)))
            (depth-first-traversal-with-path g
                                             (into (rest queue)
                                                   (map #(conj (first queue) %) next))
                                             visited)))))))
(defn remove-connected-component [g start]
  "returns a graph from which the connected component containing start
is removed."
  (reduce remove-connected-component
          (remove-node g start)
          (neighbors g start)))
(defn partition-graph [graph start]
  "partition graph returns a 2-element vector. the first element is a
graph of the connected component of graph contating start and the
second element is the a graph containing the rest of graph, that is
graph after removing the connected component containing start."
  (letfn [(connected-component*
            [[new old] node]
            (if node
              (reduce connected-component*
                      (let [[new2 old2]
                            (reduce (fn [[new old] edge]
                                      [(add-edge new edge)
                                       (remove-edge old edge)])
                                    [new old]
                                    (edges old node))]
                        [(add-node new2 node)
                         (remove-node old2 node)])
                      (neighbors old node))
              [new old]))]
    (connected-component* [(make-graph) graph] start)))
(defn connected-component [graph start]
  "Returns a graph corresponding the connected component of graph that
contains start."
  (first (partition-graph graph start)))
(defn connected-components [graph]
  "Returns a sequence of the connected components of g."
  (letfn [(connected-components*
            [graph acc]
            (if (empty? (nodes graph))
              acc
              (let [[part rest] (partition-graph graph (first (nodes graph)))]
                (when (nodes rest)
                  (recur rest (conj acc part))))))]
    (connected-components* graph nil)))
(defn find-longest-paths [g]
  "Finds a longest path acecssible from each node in the
graph. Returns a sequence of vectors containing the longest shortest paths for each
node. Each vector represents the path to the farthest node from the
first node of the vector to the last node of the vector."
  (map (fn [start]
         (first
          (reverse
           (sort-by count
                    (breadth-first-traversal-with-path g start)))))
       (nodes g)))
(defn graph-distance-hash [g]
  "Return a hashmap with entries for each node containing 2-element
arrays of the nodes in g and the distances (shortest path) from the
first node to the second node"
  (reduce (fn [m node]
            (conj m
                  {node
                   (map (fn [path]
                          [(last path) (dec (count path))])
                        (breadth-first-traversal-with-path g node))}))
          {}
          (nodes g)))
(defn position [coll x]
  (some (fn [[a b]]
          (when (= b x) a))
        (map vector (iterate inc 0) coll)))
(defn graph-distance-matrix [g]
  "Returns a 2-d array of the distance (shortest path) between two
nodes. The distance between a node and itself is 0. If a node is
unreachable from another node, the distance between the nodes is -1."
  (let [hash (graph-distance-hash g)
        size (count (nodes g))]
    (let [a (make-array (. Double TYPE) size size)
          nodes (nodes g)]
      (dotimes [i size]
        (dotimes [j size]
          (aset a i j -1)))
      (doseq [outer nodes]
        (doseq [[inner distance] (get hash outer)]
          (let [outindex (position nodes outer)
                inindex (position nodes inner)]
            (aset a outindex inindex distance))))
      a)))
(defn find-cycle
  ([g]
     (reduce #(or %1 %2)
             (map #(find-cycle % (first (nodes %)))
                  (connected-components g))))
  ([g start]
     (reduce #(or %1 %2)
             (map (fn [neighbor]
                    (find-node (remove-edge g start neighbor) neighbor start))
                  (neighbors g start)))))
(defn graph-empty? [g]
  "returns true if there are no nodes in g."
  (empty? (nodes g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pod-cost 20)
;; (def starting-plat 200)
;; (def debug true)
;; (def starting-plat 200)
;; (def num-nodes 154)
(def standard-radius 3)
;; (def max-res 200)
;; (def max-inc 6)
;; (def max-fight-loop 3)
(def sight-radius 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (def world (atom []))
;; (def graph-world (atom (make-graph #{} {})))
(def shortest-paths (atom {})) ;;map of paths sorted by shortest distance from each starting node

(declare reset-world)

;; (declare edge-vector)
(declare get-shortest-path)
(declare find-short-paths)
(declare nearby-nodes)

;; (defn setup-graph-world
;;   "Creates the defacto graph world, hopefully this is identical to the true game world (though logic shouldn't depend on it)."
;;   []
;;   (swap! graph-world #(when % (make-graph #{} {})))
;;   (swap! graph-world #(when %
;;                         (loop [graph (reduce add-nodes (make-graph #{} {}) (range num-nodes)) ;;add all nodes
;;                                edges edge-vector]
;;                           (if (empty? edges)
;;                             graph
;;                             (recur (add-edge graph (first (first edges)) (second (first edges))) ;;add all links
;;                                    (next edges)))))))


(defn new-node
  "Returns a new node"
  ([node-id] (new-node node-id 0))
  ([node-id income]
     {:id node-id
      :source-value 0 ;;reset at beginning of every turn
      :scalar-value 0 ;;recalculated from nearby source values every turn
      :owner nil ;;id of player controlling this, neutral '1000' by default
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

;; (defn add-income
;;   "Takes in the world (a vector of maps, one for each node)."
;;   [world]
;;   (loop [res-remain max-res
;;          next-node (rand-int num-nodes)
;;          acc world]
;;     (if (= 0 res-remain)
;;       acc;;return the updated world
;;       (if (>= (:income (world next-node)) max-inc)
;;         (recur res-remain (rand-int num-nodes) acc)
;;         (recur (dec res-remain) (rand-int num-nodes) (assoc-in acc [next-node :income] (inc (:income (acc next-node)))))))))

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

;; (defn reset-world
;;   ""
;;   []
;;   (swap! world (fn [_] (add-income (blank-world))))
;;   (setup-graph-world)
;;   ;;find all shortest paths
;;   (swap! shortest-paths (fn [_] (find-short-paths @graph-world)))
;;   world)

;; (defn new-world
;;   ""
;;   ([] (new-world 154))
;;   ([num-nodes]
;;      (add-income (blank-world num-nodes))
;;      (setup-graph-world)
;;      ;;find all shortest paths
;;      (swap! shortest-paths (fn [_] (find-short-paths @graph-world)))
;;      (add-income (blank-world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD NAVIGATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-short-paths
  [graph num-nodes]
  (loop [node num-nodes
         acc {}]
    (if (< node 0)
      acc
      (recur (dec node)
             (assoc acc node (sort-by count (breadth-first-traversal-with-path graph node)))))))

(defn nearby-nodes
  "Returns all paths with distance <= radius from given origin-node."
  [radius origin-node]
  (filter (fn [_] (<= (count _) radius)) (@shortest-paths origin-node)))

(defn get-shortest-path
  "Returns vector of nodes for shortest path between node1 and node2"
  [p1 p2]
  (first (filter #(= (last %) p2) (@shortest-paths p1))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ADVISORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def banker-inf (atom 1))
(def noise-inf (atom 1))
(def unit-inf (atom 1))
(def territory-inf (atom 1))
(def friendly-terr-constant (atom 5))
(def enemy-terr-constant (atom (- 1)))
(def scalar-constant 1)
(def friendly-constant (atom 1))
(def enemy-constant (atom (- 1)))

(defprotocol advisor
  "Protocol for evaluating board."
  (title [adv] "Name of advisor")
  (evaluate [adv node p1] "Returns source value for the node")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level")
  )

(deftype banker []
  advisor
  (title [adv] "Banker")
  (evaluate [adv node p1]
;;     (println "ADVISOR (BANKER)
;; node: " node "
;; p1: " p1)
    (- (:income node)))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))

(deftype noise []
  advisor
  (title [adv] "Noise")
  (evaluate [adv node p1]
;;     (println "ADVISOR (BANKER)
;; node: " node "
;; p1: " p1)
    (rand-int 3))
  (influence [adv] @noise-inf)
  (influence [adv adjust] (swap! noise-inf #(+ % adjust))))

(deftype unit []
  advisor
  (title [adv] "Unit")
  (evaluate [adv node p1]
;;     (println (str  "ADVISOR (UNIT)
;; node: " node "
;; p1: " p1))
;;     (println (str "friendly constant: " @friendly-constant "
;; enemy constant: " @enemy-constant))
    (loop [player-pods (:pods node)
           p-id 0
           acc 0]
      ;; (println (str "player-pods: " player-pods))
      ;; (println "p-id: " p-id)
      ;; (println "acc: " acc)
      ;; (println "enemy-const: " @enemy-constant)
      (if (and (< p-id (count (:pods node))) ;;ensure we don't process players not in the game
               (:pods node)) ;;ensure pods actually exist
        (recur
         (next player-pods)
         (inc p-id)
         (if (= p-id (:id p1)) ;;process friendly pods different than enemy pods
           (+ acc (* ((:pods node) p-id) @friendly-constant))
           (+ acc (* ((:pods node) p-id) @enemy-constant))))
        acc)))
  (influence [adv] @unit-inf)
  (influence [adv adjust] (swap! unit-inf #(+ % adjust))))

(deftype territory []
  advisor
  (title [adv] "Territory")
  (evaluate [adv node p1]
    (if (= (:id p1) (:owner node))
      @friendly-terr-constant
      @enemy-terr-constant))
  (influence [adv] @unit-inf)
  (influence [adv adjust] (swap! unit-inf #(+ % adjust))))

(defn src-to-scal
  "Converts a source value to its scalar value at a given distance"
  [source distance]
  ;;modeling equation f(x) = constant * source-val / distance^2
  (* scalar-constant source (/ (* distance distance))))

(defn advise
  "Returns the world with source and scalar values modified by advisors."
  [world p1 advisors near-radius]
  ;; (println "GETTING ADVICE ON WORLD: " world)
  (let [source-world
        ;;modify all source values
        (loop [wor world
               next-node 0]
          (if (= next-node (count world))
            wor
            (recur (assoc-in wor
                             [next-node :source-value]
                             (loop [adv advisors
                                    acc 0]
                               (if (empty? adv)
                                 acc ;;gathered adjustment for each advisor return total
                                 ;;get more advice for node
                                 (recur (next adv) (+ acc (evaluate (first adv) (wor next-node) p1))))))
                   (inc next-node))))]
    ;;modify all scalar values
    (loop [acc source-world
           next-node (count source-world)]
      (if (< next-node 0)
        acc
        (let [nearby-nodes-paths (nearby-nodes near-radius next-node) ;;only care about nodes within this step distance
              nearby-source-node-ids (map last nearby-nodes-paths) ;;get id's for nearby nodes
              nearby-source-node-distances (map count nearby-nodes-paths) ;;get distances to those nodes
              nearby-source-node-vals (map (fn [src-node] (:source-value src-node)) (map acc nearby-source-node-ids)) ;;get the source values (from above)
              ;;get the total scalar components by summing contribution from nearby sources
              scalar-val (reduce + (map src-to-scal nearby-source-node-vals nearby-source-node-distances))]
          (recur (assoc-in acc [next-node :scalar-value] scalar-val) ;;return world with scalar-value added in
                 (dec next-node)))))))

(defn point-mod
  "Modifies a specific node and nearby nodes in a given radius"
  [world p1 node advisors near-radius]
  ;; (println "Calling point-mod")
  (let [source-mod
        (loop [adv advisors
               acc 0]
          (if (= nil adv)
            acc ;;gathered adjustment for each advisor return total
            ;;get more advice for node
            (recur
             (next adv)
             (+ acc
                (evaluate
                 (first adv)
                 (nth world (:id node))
                 p1)))))
        ;;modify the source value for the node
        source-world (assoc-in world
                               [(:id node) :source-value]
                               source-mod)]
    ;; (println "source-mod: " source-mod)
    ;; (println "source-world: " source-world)
    ;;modify the scalar value for all near nodes
    (loop [world source-world
           nodes (map last (nearby-nodes near-radius (:id node))) ;;list of surrounding node ids
           distances (map count (nearby-nodes near-radius (:id node)))] ;;list of distances to surrounding nodes
      (if (empty? nodes)
        world
        (recur (assoc-in world
                         [(first nodes) :scalar-value]
                         (src-to-scal source-mod (first distances)))
               (next nodes)
               (next distances))))))

(defn get-advisors
  "Returns a list of all advisors"
  []
  ;;(list (banker.) )
  (list (banker.) (unit.) (territory.)));;(noise.)

(defn move-mod
  "TODO Returns how the would would appear if x pods were moved from p1 to p2."
  [x p1 p2 world])




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def next-id (atom -1))

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
  [id num-nodes starting-plat]
     {:id id
      :platinum starting-plat
      :income 0
      :territories 0 ;;determined every round
      :liberties 0 ;;determined every round
      ;;pods is a vector the nth value in pod corresponds to the nth node in the world,
      ;;the value at that index is the number of pods the player has on that node
      :pods (loop [pods 0
                   acc []]
              (if (< pods num-nodes)
                (recur (inc pods)
                       (conj acc 0))
                acc))})

(defn det-move
  "Returns a vector of vectors that represent how pods should be moved to their local minima. Does not combine information."
  [sight p1 world]
  ;; (println "Determining move")
  ;;create a scalar map of the world and move each unit towards its local minimum
  (let [scalar-world (advise world p1 (get-advisors) sight)]
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
                                             inner-world scalar-world
                                             acc []]
                                        ;;                                   (println (str "Inner loop
                                        ;; pods-remaining: " pods-remaining "
                                        ;; acc: " acc))
                                        (if (< 0 pods-remaining)
                                          ;;get shortest path
                                          (let [next-node (get-shortest-path i ;;pods current position
                                                                                   ;;get node id of local minima
                                                                                   (:id (get-local-min sight
                                                                                                             i
                                                                                                             inner-world)))]
                                        ;; (println (str  "next node: " next-node))

                                            (recur (dec pods-remaining)
                                                   (point-mod inner-world p1 (second  next-node) (get-advisors) standard-radius)
                                                   (conj acc [1 i (second next-node)])))
                                          acc))))))))



(defn det-place
  "Determines where to place units. Does not combine information"
  [p1 world]
  (let [filtered-world (filter #(or (= (:owner %) (- 1)) (= (:owner %) (:id p1)))  world) ;;world;;
        ]
  ;; (println "Determining placement")
  (loop [wor world
         pods (int (/ (:platinum p1) pod-cost))
         acc []]
    (if (= 0 pods)
      acc
      ;;recur with point modified map and one less pod
      (let [global-min (get-global-min filtered-world)]
        (recur (point-mod wor p1 global-min (get-advisors) standard-radius)
               (dec pods) ;;decrease pods available by 1
               ;;place a pod at global minima
               (conj acc [1 (:id global-min)])))))))

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
;;   (if (= 0 (rand-int 5)) (println "Printing test:
;; " vector))
  (loop [acc ""
         v vector]
    (if (empty? v)
      acc
      (recur (str acc (apply str " " (interpose " " (first v)) ))
             (next v)))))

(defn gen-move-message
  "Returns a string to move units."
  [unit-move-vector]
  ;; (if (= 0 (rand-int 5)) (println unit-move-vector))
  (if (or (empty? unit-move-vector) (= nil (first unit-move-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-move-vector))))

(defn gen-place-message
  "Returns a string to place new units."
  [unit-place-vector]
  (if (or (empty? unit-place-vector) (= nil (first unit-place-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-place-vector))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-players
  "Returns a list of players"
  [num starting-id num-nodes starting-plat]
     (loop [more num
            id starting-id
            acc []]
       (if (= more 0)
         acc
         (recur (dec more)
                (inc id)
                (conj acc (new-player id num-nodes starting-plat))))))

(defn -main [& args]
    (let [playerCount (read) myId (read) zoneCount (read) linkCount (read)
          sight-radius 3
          starting-plat 200
          ;;create local players
          players (create-players playerCount 0 zoneCount starting-plat)
          [graph-world node-world] (loop [i linkCount
                                          [g-world n-world] (loop [i zoneCount
                                                                   inner-g-world (make-graph #{} {})
                                                                   inner-n-world []]
                                                              (if (> i 0)
                                                                (let [zoneId (read) platinumSource (read)]
                                                                  ;; zoneId: this zone's ID (between 0 and zoneCount-1)
                                                                  ;; platinumSource: the amount of Platinum this zone can provide per game turn
                                                                  (recur (dec i)
                                                                         (add-nodes inner-g-world zoneId)
                                                                         (conj inner-n-world (new-node zoneId platinumSource))))
                                                                [inner-g-world inner-n-world]))]
                                     (if (> i 0)
                                       (let [zone1 (read) zone2 (read)]
                                         ;;record edge between nodes
                                         ;;update liberties of both zones
                                         (recur (dec i)
                                                [(add-edge g-world zone1 zone2)
                                                 (assoc-in
                                                  (assoc-in n-world [zone1 :total-liberties] (inc (:total-liberties (nth n-world zone1))))
                                                  [zone2 :total-liberties] (inc (:total-liberties (nth n-world zone2))))]))
                                       [g-world n-world]))]
      (swap! shortest-paths (fn [_] (find-short-paths graph-world zoneCount)))
      ;; (println "shortest paths: " @shortest-paths)
      ;; (debug "got here1")
      ;; (when debug
      ;;   (println "playerCount: " playerCount)
      ;;   (println "myId: " myId)
      ;;   (println "zoneCount: " zoneCount)
      ;;   (println "linkCount: " linkCount))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (while true
        (loop [turn 1
               turn-world node-world
               turn-players (let [platinum (read)] (assoc-in players [myId :platinum] platinum))]
;;           (debug "TURN: " turn " MYID: " myId "
;; turn-world: " turn-world "
;; turn-players: " turn-players)

          ;; (debug "got here 2")

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;verify AI view of world matches actual world
          ;; (check-world (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) @s-world)
          ;;              (map (fn [node] (dissoc (dissoc node :total-liberties) :income)) world))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END TODO REMOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;; (debug "got here 3")

          ;;basic turn structure
          (recur (inc turn)
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
                       ;; (println "zId " zId)
                       ;; (println "PodsP0: " podsP0)
                       ;; (println (str "PodsP1: " (or podsP1 "its nil")))
                       ;; (println "PodsP2: " podsP2)
                       ;; (println "PodsP3: " podsP3)
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
                     (let [advised-world (advise new-world (nth turn-players myId) (get-advisors) sight-radius)
                           movement (.trim (gen-move-message (det-move sight-radius (nth new-players myId) advised-world)))
                           placement (.trim (gen-place-message (det-place (nth new-players myId) advised-world)))]
                       (println movement)
                       (println placement)
                       ;; (println "WAIT")
                       ;; (println (str 1 " " (- (count graph-world) 1)))
                       ;;send commands
                       ;; (debug (str "movement result: " (println movement)))
                       ;; (debug (str "placement result: " (println placement)))
                       ;;determine where to place units
                       ;; (when debug
                       ;;   (println "TURN: " turn)
                       ;;   (println "Movement: " movement)
                       ;;   (println "Placement: " placement)
                       ;;   (println "Player1 eval: " (evaluate (first players) turn))
                       ;;   (println "Player2 eval: " (evaluate (second players) turn)))
                       ;; (debug "next turns world: " new-world)
                       new-world)))
                 (assoc-in turn-players [myId :platinum] (read))))))) ;;next turns platinum
