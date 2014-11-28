(ns Player
  (:gen-class))
;;Graph code between lines 6 and 329 is not mine, keeping copyright per request.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAPH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file: shortcut/graph.clj
;;;
;;; Copyright (c) 2010 Cyrus Harmon (ch-lisp@bobobeach.com) All rights
;;; reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above
;;; copyright notice, this list of conditions and the following
;;; disclaimer in the documentation and/or other materials
;;; provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;(ns shortcut.graph)
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
(def standard-radius 3)
(def sight-radius 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol fuzz-in
  (fi-key [fuzzi]) ;;keyword for fuzzy variable
  (fuzz-it [fuzzi num]) ;;takes in the input variable and returns a map of [fuzzy-set-type truthiness] values and a [nil fuzzy-set-vector]
  (kv-pair [fuzzi num])) ;;convenience func that returns a vector with the fuzzy keyword and the fuzzy set for an input

(defprotocol fuzz-rules
  (premise [fuzzr]) ;;returns the premise representation of the rule
  (consequent [fuzzr])) ;;returns the consequent representation of the rule

(defprotocol fuzz-out
  (fo-key [fuzzo])
  (ideal-val [fuzzo])
  (crisp-it [fuzzo m])) ;;returns a number represented by a fuzzy set, given a map of {:fuzz-type val

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn filter-map
  [m kl]
  (loop [k kl
         acc []]
    (if (empty? k)
      acc
      (let [v (m (first k))]
        (recur (next k)
               (if v
                 (conj acc v)
                 acc))))))

(defn extract
  [m v]
  (loop [k v
         inner m]
    (if (or (nil? inner )
            (empty? k))
      inner
      (recur (next k)
             (inner (first k))))))

(defn extract-all
  [m v]
  (loop [next-v v
         acc []]
    (if (empty? next-v)
      acc
      (recur (next next-v)
             (conj acc
                   (loop [k (first next-v)
                          inner m]
                     (if (empty? k)
                       inner
                       (recur (next k)
                              (inner (first k))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VECTOR FUNCTIONS FOR FUZZY SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dot
  [v1 v2]
  (reduce #(+ %1 (* (first %2) (second %2)))
          0
          (partition 2 (interleave v1 v2))))

(defn normalize
  [v1]
  (let [sum (reduce + v1)
        sum (if (> sum 0) sum 1)]
    (map #(/ % sum) v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inference
  [fuzz-rules fuzz-map]
  (loop [rules fuzz-rules
         acc {}]
    (if (empty? rules)
      acc
      (recur
       (next rules)
       (let [prem (premise (first rules))
             consq (consequent (first rules))
             ;;multiply truthiness of all premises
             val (reduce *
                         ;;get a list of truthiness for each premise val
                         (extract-all fuzz-map prem))

             types (map first consq) ;;the list of consequent fuzz-types (i.e. height)
             props (map second consq)] ;;the list of qualitative descriptions for the fuzzy type (i.e. tall, if the fuzz-type was height)
         (reduce
          #(if (extract %1 [(nth %2 0) (nth %2 1) ])
             (assoc-in %1
                       [(nth %2 0) (nth %2 1) ] (conj (extract %1 [(nth %2 0) (nth %2 1) ]) (nth %2 2)) )
             (assoc-in %1
                       [(nth %2 0) (nth %2 1) ] [(nth %2 2)]))
          acc
          (partition 3 (interleave types props (cycle [val])))))))))
(defn composition
  [m]
  (loop [pair (seq m)
         acc m]
    (if (empty? pair)
      acc
      (recur (next pair)
             (loop [inner-pair (seq (second (first pair)))
                    inner-acc acc]
               (if (or (empty? inner-pair)
                       (nil? (first inner-pair)))
                 inner-acc
                 (recur (next inner-pair)
                        (assoc-in inner-acc [(first (first pair)) (first (first inner-pair))] (reduce + (second (first inner-pair)))))))))))
(defn defuzzification
  ""
  [fuzz-out m]

  (loop [fuzzo fuzz-out
         acc {}]
    (if (empty? fuzzo)
      acc
      (recur (next fuzzo)
             (assoc acc (fo-key (first fuzzo)) (crisp-it (first fuzzo) (m (fo-key (first fuzzo)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY INPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype ratio-fe-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-fe-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype per-tot-inc []
  fuzz-in
  (fi-key [fuzzi] :per-tot-inc)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.1 num)) 0)
                              :hi (max (+ 0.5 (- num 0.2)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype per-tot-terr []
  fuzz-in
  (fi-key [fuzzi] :per-tot-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.2 num)) 0)
                              :hi (max (+ 0.5 (- num 0.4)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype tot-e-pods []
  fuzz-in
  (fi-key [fuzzi] :tot-e-pods)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.3 num)) 0)
                              :hi (max (+ 0.5 (- num 0.3)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-fe-pods []
  fuzz-in
  (fi-key [fuzzi] :ratio-fe-pods)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-ne-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-ne-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-nf-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-nf-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-ne-pods []
  fuzz-in
  (fi-key [fuzzi] :ratio-ne-pods)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))


(defn get-fuzzy-inputs
  []
  [(ratio-fe-terr.) (per-tot-inc.) (per-tot-terr.) (tot-e-pods.) (ratio-fe-pods.) (ratio-ne-terr.) (ratio-nf-terr.) (ratio-ne-pods.)
   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY OUTPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gen-crisp
  [fuzzo m]
  (let [[vec model] (loop [kv-pairs (seq (ideal-val fuzzo))
                                               [actual ideal] [[] []]]
                                          (if (empty? kv-pairs)
                                            [actual ideal]
                                            (let [act-val (m (first (first kv-pairs)))
                                                  ideal-val (second (first kv-pairs))]
                                              (recur (next kv-pairs)
                                                     [(conj actual (if act-val act-val 0))
                                                      (conj ideal ideal-val)]))
                                            ))]
    (dot (normalize vec) model)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; banker advisor output
(deftype banker-inf []
  fuzz-out
  (fo-key [fuzzo] :banker-inf)
  (ideal-val [fuzzo] {:lo 20
                      :hi 60})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unit advisor output
(deftype unit-f-inf []
  fuzz-out
  (fo-key [fuzzo] :unit-f-inf)
  (ideal-val [fuzzo] {:lo (- 100)
                      :hi 20})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

(deftype unit-e-inf []
  fuzz-out
  (fo-key [fuzzo] :unit-e-inf)
  (ideal-val [fuzzo] {:lo (- 100)
                      :hi 50})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; territory adviser output
(deftype terr-f-inf []
  fuzz-out
  (fo-key [fuzzo] :terr-f-inf)
  (ideal-val [fuzzo] {:lo 100
                      :hi (- 50)})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

(deftype terr-e-inf []
  fuzz-out
  (fo-key [fuzzo] :terr-e-inf)
  (ideal-val [fuzzo] {:lo 50
                      :hi (- 200)})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))


(defn get-fuzzy-outputs
  "Returns seq of fuzzy outputs to be used."
  []
  [(banker-inf.)
   (unit-f-inf.)
   (unit-e-inf.)
   (terr-f-inf.)
   (terr-e-inf.)
   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY EXPERT RULES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype cont-priority []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :lo] [:per-tot-inc :hi] [:ratio-nf-terr :hi]])
  (consequent [fuzzr] [[:banker-inf :hi] [:terr-f-inf :lo]]))

(deftype avoid-small-cont []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :lo] [:per-tot-inc :lo]])
  (consequent [fuzzr] [[:banker-inf :lo] [:terr-f-inf :lo] [:terr-e-inf :lo]]))

(deftype dont-lose-asia []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :hi] [:ratio-fe-terr :lo]])
  (consequent [fuzzr] [[:terr-f-inf :hi] [:unit-f-inf :hi] [:terr-e-inf :hi] [:unit-e-inf :lo]]))

(deftype take-undefended-cont []
    fuzz-rules
  (premise [fuzzr] [[:ratio-fe-pods :hi] [:ratio-ne-terr :hi]])
  (consequent [fuzzr] [[:banker-inf :hi] [:terr-e-inf :hi] [:unit-f-inf :hi] ]))

(deftype start-strong []
    fuzz-rules
  (premise [fuzzr] [[:ratio-nf-terr :hi] [:ratio-ne-terr :hi]])
  (consequent [fuzzr] [[:banker-inf :hi] [:terr-e-inf :hi] [:unit-e-inf :hi] [:unit-f-inf :hi]]))

(deftype hunt-and-kill []
    fuzz-rules
  (premise [fuzzr] [[:ratio-fe-terr :hi] [:ratio-fe-pods :hi]])
  (consequent [fuzzr] [[:banker-inf :lo] [:terr-e-inf :hi] [:unit-e-inf :lo] [:unit-f-inf :hi]]))

(deftype spread-out []
    fuzz-rules
  (premise [fuzzr] [[:ratio-nf-terr :lo] [:ratio-ne-pods :hi]])
  (consequent [fuzzr] [[:banker-inf :lo] [:banker-inf :hi] [:terr-f-inf :lo] [:unit-e-inf :lo] [:unit-f-inf :hi]]))

(defn get-fuzzy-rules
  "Returns seq of fuzzy rules to be used."
  []
  [(cont-priority.) (avoid-small-cont.) (dont-lose-asia.) (start-strong.) (hunt-and-kill.) (spread-out.);;(take-undefended-cont.)
   ])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def shortest-paths (atom {})) ;;map of paths sorted by shortest distance from each starting node
(declare reset-world)
(declare find-short-paths)
(declare nearby-nodes)


(defn new-node
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
  ([] (blank-world 154))
  ([num-nodes] (loop [node-id 0
         acc []]
    (if (< node-id num-nodes)
      (recur (inc node-id)
             (conj acc (new-node node-id))) ;;add another node
      acc))))

(defn node-liberties
  [node world]
  (let [player ((nth world node) :owner)]
    ;;get number of nearby nodes not owned by player
    (count
     (filter #(not (= % player))
             ;;get owners of those ids
             (map #((nth world %) :owner)
                  ;;get all bordering node ids
                  (map last (next (nearby-nodes 2 0))))))))

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
  [radius origin-node]
  (filter (fn [_] (<= (count _) radius)) (@shortest-paths origin-node)))

(defn get-shortest-path
  ([p1 p2] (get-shortest-path p1 p2 @shortest-paths))
  ([p1 p2 shortest-paths] (first (filter #(= (last %) p2) (shortest-paths p1)))))

(defn get-global-min
  [world]
  (first (sort-by #(:scalar-value %) world)))

(defn safe-global-min
  [world pid]
  (first (filter #(or (= pid (:owner %))
               (= (- 1) (:owner %)))  (sort-by #(:scalar-value %) world))))

(defn get-local-min
  [radius node-id world]
  ;;sort by scalar-value and return lowest (first)
  (first (sort-by #(:scalar-value %)
                  ;;get node maps for the ids
                  (map #(nth world %)
                       ;;get node ids of nearby nodes
                       (map last (nearby-nodes radius node-id))))))

(defn is-distinct?
  [n1 n2 shortest-paths]
  (nil? (get-shortest-path (:id n1) (:id n2) shortest-paths)))

(defn get-continents
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ADVISORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def banker-inf (atom 2))
(def noise-inf (atom 1))
(def unit-inf (atom 1))
(def territory-inf (atom 1))
(def friendly-terr-constant (atom 5))
(def enemy-terr-constant (atom (- 2)))
(def scalar-constant 1)
(def friendly-constant (atom (+ 1)))
(def enemy-constant (atom (- 1)))

(defprotocol advisor
  (title [adv] "Name of advisor")
  (evaluate [adv node p1 inf num-players] "Returns source value for the node")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level"))

(deftype banker []
  advisor
  (title [adv] "Banker")
  (evaluate [adv node p1 inf num-players]
    (* (- 1 (if (> num-players 2)
                     (/ num-players 10)
                     0) ) (nth inf 0) (- (:income node))))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))

(deftype unit []
  advisor
  (title [adv] "Unit")
  (evaluate [adv node p1 inf num-players]
    (loop [player-pods (:pods node)
           p-id 0
           acc 0]
      (if (and (< p-id (count (:pods node))) ;;ensure we don't process players not in the game
               (:pods node)) ;;ensure pods actually exist
        (recur
         (next player-pods)
         (inc p-id)
         (if (= p-id (:id p1)) ;;process friendly pods different than enemy pods
           (+ acc (* ((:pods node) p-id) (first inf)))
           (* (+ 1 (if (> num-players 2)
                     (/ num-players 10)
                     0) )  (+ acc (* ((:pods node) p-id) (second inf))))))
        acc)))
  (influence [adv] @unit-inf)
  (influence [adv adjust] (swap! unit-inf #(+ % adjust))))

(deftype territory []
  advisor
  (title [adv] "Territory")
  (evaluate [adv node p1 inf num-players]
    (if (= (:id p1) (:owner node))
      (nth inf 0)
      (nth inf 1)))
  (influence [adv] @unit-inf)
  (influence [adv adjust] (swap! unit-inf #(+ % adjust))))

(defn src-to-scal
  [source distance]
  ;;modeling equation f(x) = constant * source-val / distance^2
  (* scalar-constant source (/ (* distance distance))))

(defn advise
  [world p1 advisors near-radius ai conts num-players]
  (let [num-nodes (count world)
        source-world
        ;;modify all source values
        (loop [wor world
               next-node 0]
          (if (= next-node num-nodes)
            wor
            (recur (assoc-in wor
                             [next-node :source-value]
                             (loop [adv advisors
                                    inf (ai (conts next-node)) ;;the set of influences for the continent next-node is on
                                    acc 0]
                               (if (empty? adv)
                                 acc ;;gathered adjustment for each advisor return total
                                 ;;get more advice for node
                                 (recur (next adv)
                                        (next inf)
                                        (+ acc (evaluate (first adv) (wor next-node) p1 (first inf) num-players))))))
                   (inc next-node))))]
    ;;modify all scalar values
    (loop [acc source-world
           next-node 0]
      (if (= next-node num-nodes)
        acc
        (let [nearby-nodes-paths (nearby-nodes near-radius next-node) ;;only care about nodes within this step distance
              nearby-source-node-ids (map last nearby-nodes-paths) ;;get id's for nearby nodes
              nearby-source-node-distances (map count nearby-nodes-paths) ;;get distances to those nodes
              nearby-source-node-vals (map (fn [src-node] (:source-value src-node)) (map acc nearby-source-node-ids)) ;;get the source values (from above)
              ;;get the total scalar components by summing contribution from nearby sources
              scalar-val (reduce + (map src-to-scal nearby-source-node-vals nearby-source-node-distances))]
          (recur (assoc-in acc [next-node :scalar-value] scalar-val) ;;return world with scalar-value added in
                 (inc next-node)))))))

(defn point-mod
  [world p1 node advisors near-radius ai conts num-players]
(let [source-mod
        (loop [adv advisors
               inf (ai (conts (:id node)))
               acc 0]
          (if (= nil adv)
            acc ;;gathered adjustment for each advisor return total
            ;;get more advice for node
            (recur
             (next adv)
             (next inf)
             (+ acc
                (evaluate
                 (first adv)
                 (nth world (:id node))
                 p1
                 (first inf)
                 num-players)))))
        ;;modify the source value for the node
        source-world (assoc-in world
                               [(:id node) :source-value]
                               source-mod)]
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
  []
  ;;(list (banker.) )
  (list (banker.) (unit.) (territory.)));;(noise.)

(defn move-mod
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
  [p1 turn]
  ;;todo make this useful
  (+ (:platinum p1) (reduce + (:pods p1)) (:income p1) (- (:liberties p1))  (:territories p1)))

(defn quant-node
  [node stats pid]
  (if (= (node :owner) pid)
    ;;node is friendly, update stats
    (as-> stats %
        (assoc % :tot-f-terr (inc (% :tot-f-terr)))
        (assoc % :tot-f-inc (+ (% :tot-f-inc) (node :income)))
        (assoc % :tot-f-pods (+ (% :tot-f-pods) (nth (node :pods) pid)))
        (assoc % :tot-f-tlib (+ (% :tot-f-tlib) (node :total-liberties)))
        (assoc % :tot-f-olib (+ (% :tot-f-olib) (node :open-liberties))))
    (if (= (node :owner) (- 1))
      ;;node is neutral, update stats
      (as-> stats %
            (assoc % :tot-n-terr (inc (% :tot-n-terr)))
            (assoc % :tot-n-inc (inc (% :tot-n-inc)))
            (assoc % :tot-n-pods (+ (% :tot-n-pods) 0)) ;;neutral player can't have pods (nth (node :pods) pid)
            (assoc % :tot-n-tlib (+ (% :tot-n-tlib) (node :total-liberties)))
            (assoc % :tot-n-olib (+ (% :tot-n-olib) (node :open-liberties))))
      ;;node is enenmy, update stats
      (as-> stats %
        (assoc % :tot-e-terr (inc (% :tot-e-terr)))
        (assoc % :tot-e-inc (inc (% :tot-e-inc)))
        (assoc % :tot-e-pods (+ (% :tot-e-pods) (nth (node :pods) pid)))
        (assoc % :tot-e-tlib (+ (% :tot-e-tlib) (node :total-liberties)))
        (assoc % :tot-e-olib (+ (% :tot-e-olib) (node :open-liberties)))))))

(defn quant-continent
  "Returns a map with quantitative information about a continent."
  [world cont pid]
  ;;get the list of nodes in the continent but with the most up-to-date information from world.
  (let [updated-cont (loop [nodes cont
                            acc []]
                       (if (empty? nodes)
                         acc
                         (let [old-node (first nodes)
                               new-node (nth world (:id old-node))]
                           (recur (next nodes)
                                  (conj acc new-node)))))]
    ;;for every node in the continent gather stats to be returned later
    (loop [nodes updated-cont
           acc-stats {:tot-f-terr 0 ;;total friendly territory
                      :tot-e-terr 0 ;;total enemy territory
                      :tot-n-terr 0 ;;total neutral territory

                      :tot-f-inc 0 ;;total income contained on friendly nodes
                      :tot-e-inc 0 ;;total income contained on enemy nodes
                      :tot-n-inc 0 ;;total income contained on neutral nodes

                      :tot-f-pods 0 ;;total friendly pods
                      :tot-e-pods 0 ;;total neutral pods
                      :tot-n-pods 0 ;;total enemy pods

                      :tot-f-tlib 0 ;;total liberties on all friendly nodes
                      :tot-e-tlib 0 ;;total liberties on all enemy nodes
                      :tot-n-tlib 0 ;;total liberties on all neutral nodes

                      :tot-f-olib 0 ;;total open liberties on all friendly nodes
                      :tot-e-olib 0 ;;total open liberties on all enemy nodes
                      :tot-n-olib 0 ;;total open liberties on all neutral nodes
                      }]
      (if (empty? nodes)
        acc-stats
        (recur (next nodes)
               (quant-node (first nodes) acc-stats pid))))))

(defn quant-world
  [world conts pid]
  (loop [next-cont conts
         acc []]
    (if (empty? next-cont)
      acc
      (recur (next next-cont) (conj acc (quant-continent world (first next-cont) pid))))))

(defn new-player
  [id num-nodes starting-plat]
     {:id id
      :platinum starting-plat
      :income 0
      :territories 0 ;;determined every round
      :liberties 0 ;;determined every round
      :pods (loop [pods 0
                   acc []]
              (if (< pods num-nodes)
                (recur (inc pods)
                       (conj acc 0))
                acc))})

(defn det-move
  [sight p1 world ai conts num-players]
  (loop [i 0
           pods (:pods p1)
           outer-acc []]
      (if (empty? pods)
        outer-acc ;;return moves
        (recur (inc i)
               (next pods)
               (reduce conj outer-acc (loop [pods-remaining (first pods)
                                             inner-world world
                                             acc []]
                                        (if (< 0 pods-remaining)
                                          ;;get shortest path
                                          (let [minima (second
                                                      ;;get shortest path
                                                      (get-shortest-path i ;;pods current position
                                                                         ;;get node id of local minima
                                                                         (:id (get-local-min sight
                                                                                             i
                                                                                             inner-world))))
                                              next-node (if (nil? minima) i minima)]
                                            (recur (dec pods-remaining)
                                                   (point-mod inner-world p1 (nth inner-world next-node) (get-advisors) standard-radius ai conts num-players)
                                                   (conj acc [1 i next-node])))
                                          acc)))))))



(defn det-place
  [p1 world ai conts num-players]
  (loop [wor world
         pods (int (/ (:platinum p1) pod-cost))
         acc []]
    (if (= 0 pods)
      acc
      ;;recur with point modified map and one less pod
      ;; (let [global-min (get-global-min wor)]
      (let [global-min (safe-global-min wor (:id p1))]
        (recur (point-mod wor p1 global-min (get-advisors) standard-radius ai conts num-players)
               (dec pods) ;;decrease pods available by 1
               ;;place a pod at global minima
               (conj acc [1 (:id global-min)]))))))


(defn comp-move
  [a b]
  (if (and (= (second a) (second b))
           (= (second (next a)) (second (next b))))
    [[(+ (first a) (first b)) (second a) (second (next a))]]
    [a b]))

(defn comp-place
  [a b]
  (if (= (second a) (second b))
    [[(+ (first a) (first b)) (second a)]]
    [a b]))

(defn combine-vectors
  [vectors]
  (loop [acc [(first (sort-by second vectors))]
         v (next (sort-by second vectors))]
    (if (empty? v)
      acc
      (recur (reduce conj (reduce conj [] (butlast acc)) ((if (= 3 (count (first vectors))) comp-move comp-place ) (last acc) (first v)) )
             (next v)))))

(defn v-to-msg
  [vector]
  (loop [acc ""
         v vector]
    (if (empty? v)
      acc
      (recur (str acc (apply str " " (interpose " " (first v)) ))
             (next v)))))

(defn gen-move-message
  [unit-move-vector]
  (if (or (empty? unit-move-vector) (= nil (first unit-move-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-move-vector))))

(defn gen-place-message
  [unit-place-vector]
  (if (or (empty? unit-place-vector) (= nil (first unit-place-vector)))
    "WAIT"
    (v-to-msg (combine-vectors unit-place-vector))))

(defn crisp-to-ai-weights
  [m]
  [[(:banker-inf m)]
   [(:unit-f-inf m)
    (:unit-e-inf m)]
   [(:terr-f-inf m)
     (:terr-e-inf m)]])

(defn cont-to-fuzz-map
  [cont world-stats]
  ;;return a vector of numbers for the fuzzy inputs, needs to be in the same order as returned from (fuzzy/get-fuzzy-inputs)
  [;;ratio-fe-terr
   (/ (:tot-f-terr cont) (inc (:tot-e-terr cont)))
   ;;per-tot-inc
   (/ (+ (:tot-f-inc cont) (:tot-e-inc cont) (:tot-n-inc cont)) (:tot-inc world-stats))
   ;;per-tot-terr
   (/ (+ (:tot-f-terr cont) (:tot-e-terr cont) (:tot-n-terr cont)) (:tot-terr world-stats))
   (:tot-e-pods cont)
   (/ (:tot-f-pods cont) (inc (:tot-e-pods cont)))
   (/ (:tot-n-terr cont) (inc (:tot-e-terr cont)))
   (/ (:tot-n-terr cont) (inc (:tot-f-terr cont)))
   (/ (:tot-n-pods cont) (inc (:tot-e-pods cont)))
   ;;(:tot-f-terr cont)
   ])


(defn update-ai-weights
  [cont-quants world-stats]
  (loop [conts cont-quants
         acc []]
    (if (empty? conts)
      acc
      (recur (next conts)
             (conj acc (as-> (cont-to-fuzz-map (first conts) world-stats) %
                   ;;fuzzification
                   (map fuzz-it (get-fuzzy-inputs) %)
                   ;;turn into single map for inference step
                   (reduce conj  {} %)
                   ;;inference
                   (inference (get-fuzzy-rules) %)
                   ;;composition
                   (composition %)
                   ;;defuzzification
                   (defuzzification (get-fuzzy-outputs) %)
                   ;;get ai weight vector
                   (crisp-to-ai-weights %)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-players
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
                                       [g-world n-world]))
          ;;determine shortest paths between nodes
          short-paths (find-short-paths graph-world zoneCount)
          ;;determine how the world is divided into unique areas
          conts (get-continents node-world short-paths)
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
      (swap! shortest-paths (fn [_] short-paths))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (while true
        (loop [turn 1
               turn-world node-world
               turn-players (let [platinum (read)] (assoc-in players [myId :platinum] platinum))]
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
                     (let [quant-cont (quant-world new-world conts myId)
                           ai-weights (update-ai-weights quant-cont world-stats)
                           advised-world (advise new-world (nth turn-players myId) (get-advisors) sight-radius ai-weights conts-lookup-map playerCount)
                           movement (.trim (gen-move-message (det-move sight-radius (nth new-players myId) advised-world ai-weights conts-lookup-map playerCount)))
                           placement (.trim (gen-place-message (det-place (nth new-players myId) advised-world ai-weights conts-lookup-map playerCount)))]
                       ;;send commands
                       (println movement)
                       (println placement)
                       new-world)))
                 (assoc-in turn-players [myId :platinum] (read))))))) ;;next turns platinum
