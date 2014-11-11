(ns platinum-rift.advisors
  (:require [platinum-rift.world :as world]))

(def banker-inf (atom 1))
(def scalar-constant 1)

(defprotocol advisor
  "Protocol for evaluating board."
  (name [adv] "Name of advisor")
  (evaluate [adv node] "Returns source value for the node")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level")
  )

(deftype banker []
  advisor
  (name [adv] "Banker")
  (evaluate [adv node] (:income node))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))

(defn src-to-scal
  "Converts a source value to its scalar value at a given distance"
  [source distance]
  ;;modeling equation f(x) = constant * source-val / distance^2
  (* scalar-constant source (/ (* distance distance))))

(defn advise
  "Returns the world with source and scalar values modified by advisors."
  [world p1 & advisors]
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
                                 acc ;;no more advisors
                                 ;;get more advice for node
                                 (recur (next advisors) (+ acc (evaluate (first adv) (wor next-node)))))))
                   (inc next-node))))]
    ;;modify all scalar values
    (loop [acc source-world
           next-node 0]
      (if (= next-node world/num-nodes)
        acc
        (let [near-radius 3 ;;only care about nodes within this step distance
              nearby-nodes-paths (world/nearby-nodes near-radius next-node)
              nearby-source-node-ids (map last nearby-nodes-paths) ;;get id's for nearby nodes
              nearby-source-node-distances (map count nearby-nodes-paths) ;;get distances to those nodes
              nearby-source-node-vals (map (fn [src-node] (:source-value src-node)) (map acc nearby-source-node-ids)) ;;get the source values (from above)
              ;;get the total scalar components by summing contribution from nearby sources
              scalar-val (reduce + (map src-to-scal nearby-source-node-vals nearby-source-node-distances))
              ]
          (recur (assoc-in acc [next-node :scalar-value] scalar-val) ;;return world with scalar-value added in
                 (inc next-node)))))))
