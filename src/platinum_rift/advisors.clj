(ns platinum-rift.advisors
  (:require [platinum-rift.world :as world]
            [platinum-rift.constants :refer :all]))

(def banker-inf (atom 1))
(def noise-inf (atom 1))
(def unit-inf (atom 1))
(def scalar-constant 1)
(def friendly-constant (atom 1))
(def enemy-constant (atom 1))

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
           next-node 0]
      (if (= next-node num-nodes)
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
                 (world (:id node))
                 p1)))))
        ;;modify the source value for the node
        source-world (assoc-in world
                               [(:id node) :source-value]
                               source-mod)]
    ;; (println "source-mod: " source-mod)
    ;; (println "source-world: " source-world)
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
  ;;(list (banker.) )
  (list (banker.) (unit.) (noise.))
  )

(defn move-mod
  "TODO Returns how the would would appear if x pods were moved from p1 to p2."
  [x p1 p2 world])



;;how to predict opponents next move
;;how to determine confidence of being able to predict opponents next move
