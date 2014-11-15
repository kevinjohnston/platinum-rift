(ns platinum-rift.advisors
  (:require [platinum-rift.world :as world]))

(def banker-inf (atom 1))
(def unit-inf (atom 1))
(def scalar-constant 1)

(defprotocol advisor
  "Protocol for evaluating board."
  (title [adv] "Name of advisor")
  (evaluate [adv node] "Returns source value for the node")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level")
  )

(deftype banker []
  advisor
  (title [adv] "Banker")
  (evaluate [adv node] (- (:income node)))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))

(deftype unit []
  advisor
  (title [adv] "Unit")
  (evaluate [adv node] (+ (* (:pods node) 5)))
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
  (let [source-world
        ;;modify all source values
        (loop [wor world
               next-node 0]
          (if (= next-node world/num-nodes)
            wor
            (recur (assoc-in wor
                             [next-node :source-value]
                             (loop [adv advisors
                                    acc 0]
                               (if (= nil adv)
                                 acc ;;gathered adjustment for each advisor return total
                                 ;;get more advice for node
                                 (recur (next advisors) (+ acc (evaluate (first adv) (wor next-node)))))))
                   (inc next-node))))]
    ;;modify all scalar values
    (loop [acc source-world
           next-node 0]
      (if (= next-node world/num-nodes)
        acc
        (let [nearby-nodes-paths (world/nearby-nodes near-radius next-node) ;;only care about nodes within this step distance
              nearby-source-node-ids (map last nearby-nodes-paths) ;;get id's for nearby nodes
              nearby-source-node-distances (map count nearby-nodes-paths) ;;get distances to those nodes
              nearby-source-node-vals (map (fn [src-node] (:source-value src-node)) (map acc nearby-source-node-ids)) ;;get the source values (from above)
              ;;get the total scalar components by summing contribution from nearby sources
              scalar-val (reduce + (map src-to-scal nearby-source-node-vals nearby-source-node-distances))
              ]
          (recur (assoc-in acc [next-node :scalar-value] scalar-val) ;;return world with scalar-value added in
                 (inc next-node)))))))

(defn point-mod
  "Modifies a specific node and nearby nodes in a given radius"
  [world p1 node advisors near-radius]
  (let [source-mod
        (loop [adv advisors
               acc 0]
          (if (= nil adv)
            acc ;;gathered adjustment for each advisor return total
            ;;get more advice for node
            (recur
             (next advisors)
             (+ acc
                (evaluate
                 (first adv)
                 (world (:id node)))))))
        ;;modify the source value for the node
        source-world (assoc-in world
                               [(:id node) :source-value]
                               source-mod)
        ]
    ;;modify the scalar value for all near nodes
    (loop [world source-world
           nodes (map last (world/nearby-nodes near-radius (:id node))) ;;list of surrounding node ids
           distances (map count (world/nearby-nodes near-radius (:id node)))] ;;list of distances to surrounding nodes
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
  (list (banker.)))

(defn move-mod
  "TODO Returns how the would would appear if x pods were moved from p1 to p2."
  [x p1 p2 world])



;;how to predict opponents next move
;;how to determine confidence of being able to predict opponents next move
