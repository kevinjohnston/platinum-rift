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
  (evaluate [adv node p1 inf] "Returns source value for the node")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level")
  )

(deftype banker []
  advisor
  (title [adv] "Banker")
  (evaluate [adv node p1 inf]
;;     (println "ADVISOR (BANKER)
;; node: " node "
;; p1: " p1)
    (* inf (- (:income node))))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))

(deftype noise []
  advisor
  (title [adv] "Noise")
  (evaluate [adv node p1 inf]
;;     (println "ADVISOR (BANKER)
;; node: " node "
;; p1: " p1)
    (* inf (rand-int 3)))
  (influence [adv] @noise-inf)
  (influence [adv adjust] (swap! noise-inf #(+ % adjust))))

(deftype unit []
  advisor
  (title [adv] "Unit")
  (evaluate [adv node p1 inf]
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
           (+ acc (* ((:pods node) p-id) (first inf)))
           (+ acc (* ((:pods node) p-id) (second inf)))))
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
  [world p1 advisors near-radius ai conts]
  ;; (println "GETTING ADVICE ON WORLD: " world)
  (println "GETTING ADVICE ON AI " ai)
  ;; (println "GETTING ADVICE ON CONTS " (sort-by second conts))
  (let [num-nodes (count world)
        source-world
        ;;modify all source values
        (loop [wor world
               next-node 0]
          (if (= next-node (count world))
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
                                        (+ acc (evaluate (first adv) (wor next-node) p1 (first inf)))))))
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
              scalar-val (reduce + (map src-to-scal nearby-source-node-vals nearby-source-node-distances))]
          (recur (assoc-in acc [next-node :scalar-value] scalar-val) ;;return world with scalar-value added in
                 (inc next-node)))))))

(defn point-mod
  "Modifies a specific node and nearby nodes in a given radius"
  [world p1 node advisors near-radius ai conts]
  ;; (println "Calling point-mod")
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
                 (first inf))))))
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
  (list (banker.) (noise.) (unit.)))

(defn move-mod
  "TODO Returns how the would would appear if x pods were moved from p1 to p2."
  [x p1 p2 world])

(defn ret-adv-map
  [pod terr income]
  {:pod-str pod
   :territory terr
   :income income})




;; (normalize [0 0.0])

;;  (dot [0.5 1 1] [1 0 1])

;; (map (fn [%1 %2] [%1 %2]) [1 2 3] [2 2 2])

;; (defn high-low-case

;;   )

;; ;;case macros
;; (defn case-dispatch
;;   [type num]
;;   (case type
;;     :ratio-fe (num)))


;; (defn eval-continent
;;   "Takes in a continent (a vector of zones), and a player id, then returns an evaluation map for the given player."
;;   [cont p-id]
;;   (let [total-pods (reduce #(reduce + (:pods %)) cont)
;;         enemy-pods (reduce #(- (reduce + (:pods %)) ((:pods %) p-id)) cont)
;;         friendly-pods (- total-pods enemy-pods)]

;;   (reduce #(assoc %1 (first %2) (second %2)) {}

;; 1 ;; number of friendly pods
;; 1 ;; number of enemy pods
;; (kv-pair (ratio-fe. )
;;          ;; ratio of friendly to enemy pods	< .2	< 0.5	> .8 && < 1.2	< 1.5	< 2.0	 > 2
;;          (if (> enemy-pods 0) (friend))
;;          )

;; 1 ;; pod density
;; 1 ;; total income	< 3	< 10	< 30	< 60	< 90	> 90
;; 1 ;; income percent of map total	< 3/120	< 10/120	< ¼	< ½	< ¾	> ¾
;; 1 ;; friendly/enemy territory total	< 2/10	< 8/10	< 1.5	< 3	< 1000	> 1000
;; 1 ;; friendly/neutral territory total
;; 1 ;; friendly territories on continent
;; 1 ;; total territories
;; 1 ;; continent territories to world territories ratio	< 1/10	< ¼	< 3/8	< ½	< 1	1
;; 1 ;; Open-liberties
;; 1 ;; turn	< 3	< 10	< 30	< 60	< 100	> 100

;;    )
;;   ))


;; (reduce #(assoc %1 (first %2) (second %2))  {} [[ :some-key :some-val] [ :some-key2 :things]])


;; how to evaluate a map	:very-low	:low	:medium	:high	:very-high	:max
;; number of friendly pods
;; number of enemy pods
;; ratio of friendly to enemy pods	< .2	< 0.5	> .8 && < 1.2	< 1.5	< 2.0	 > 2
;; pod density
;; total income	< 3	< 10	< 30	< 60	< 90	> 90
;; income percent of map total	< 3/120	< 10/120	< ¼	< ½	< ¾	> ¾
;; friendly/enemy territory total	< 2/10	< 8/10	< 1.5	< 3	< 1000	> 1000
;; friendly/neutral territory total
;; friendly territories on continent
;; total territories
;; continent territories to world territories ratio	< 1/10	< ¼	< 3/8	< ½	< 1	1
;; Open-liberties
;; turn	< 3	< 10	< 30	< 60	< 100	> 100






;; (def adv-map
;;   {
;; (ret-adv-map :high :low :high)
;; [1 2 0.3]
;; })

;; (adv-map {:pod-str :high :territory :low :income :high :things :stuff})




;; ;;continent state
;; {:pod-str :high
;;  :territory :low
;;  :income :high}

;; :high :low


;; (if (= {:pod-str :high
;;     :territory :low
;;     :income :high}
;;    {:pod-str :high
;;     :territory :low
;;     :income :high}
;;        )
;;   [1 1 1])








;;how to predict opponents next move
;;how to determine confidence of being able to predict opponents next move



;; (defn confidence-at-point
;;   [p ds turn fading-func spreading-factor]
;;   (reduce #(let [total %1
;;                  ds-t (first %2)
;;                  ds-inf (nth %2 1)
;;                  ds-conf (nth %2 2)] (+ %1
;;                                          (* ds-conf
;;                                            (Math/pow (- 1.01 (Math/abs (- ds-conf p))) spreading-factor)
;;                                            (fading-func turn ds-t)
;;                                            ))) 0 ds))

;; (defn confidence-table
;;   [ds turn fading-func spreading-factor resolution]
;;   (let [conf-map (map #(confidence-at-point %
;;                                             ds turn fading-func spreading-factor) (range 0 1 resolution))
;;         weave (partition 2 (interleave (range 0 1 resolution) conf-map))]
;;     weave))

;; (defn confidence-table-adj
;;   "Takes a center point and resolution and returns a table of values to adjust a confidence table with."
;;   [cp resolution]
;;   (partition 2 (interleave (range 0 1 resolution)
;;                            (map #(if (< % cp)
;;           (* (Math/abs (- cp %))
;;              (Math/abs (- cp %))
;;              (/ 1 cp))
;;           (* (Math/abs (- cp %))
;;              (Math/abs (- cp %))
;;              (/ 1 (- 1 cp))
;;              )) (range 0 1 resolution)))))

;; (defn combine-tables
;;   [t1 t2]
;;   (map (fn [%1 %2] [(first %1) (+ (second %1) (second %2))]) t1 t2))

;; (combine-tables [[1 1]] [[2 2]])
;; (second (first [[1 1]]))
;; (map println [1 2 3] [2 2 2])

;; (confidence-table-adj 0.2 0.1)

;; (defn adj-function
;;   [center-point point]
;;   (Math/abs (- center-point point)))


;; confidence function as multiinput func






;; table adj function (for 5 advisors)

;; x   y
;; 0   1
;; .1  .5
;; .2  0
;; .3
;; .4
;; .5
;; .6  .5
;; .7
;; .8
;; .9
;; 1   1

;; ed / cd
;; .4 / .1 4
;; 0 / .5 0
;; 1 - (ed * cd)

;; cd * mult
;; mult = cp
;; decreases as cp increases
;; (1 - cp) + 1
;; cd * cd *  1 / cp

;; (defn next-confidence-point
;;   [table]
;;   (first (first (sort-by second table))) )







;; (/ (Math/pow 0.1 2) 1)
;; (- 1 (Math/pow (- p ds-conf) 2))

;; (- 1 (- 1 (Math/pow (- 0.2 0.0) 2)))
;; (/ p (Math/abs (- ds-conf p)))

;; (let [ds [[3 0.1 0.1]
;;           [2 0.2 0.2]
;;           [1 1.0 1.0]
;;           [4 0.1 0.1]]
;;       turn 5
;;       fading-func #(/ 1 (- %1 %2))
;;       spreading-factor 2
;;       resolution 0.01
;;       conf-map (map #(confidence-at-point %
;;                                           ds turn fading-func spreading-factor) (range 0 1 resolution))
;;       weave (partition 2 (interleave (range 0 1 resolution) conf-map))
;;       conf-table (confidence-table ds turn fading-func spreading-factor resolution)
;;       adj-table (confidence-table-adj 0.2 resolution)
;;       total-table (combine-tables conf-table adj-table)
;;       ]
;; ;; (sort conf-map)


;; ;; conf-map
;;   ;; (sort-by second weave)
;;   ;; (next-confidence-point total-table)
;;   conf-table
;;   adj-table
;;   (combine-tables conf-table adj-table)
;;   (next-confidence-point total-table)
;; )




;; 0.1 1

;; 0.5  0.1 (/ (- 1 distance squared))




;; (confidence-at-point
;;  0.3
;;  [[1 0.2 0.2]
;;   [2 0.4 0.4]]
;;  3
;;  identity
;;  2)


;; ;; (confidence-at-point
;; ;;  30
;; ;;  [[1 20 20]
;; ;;   [2 40 40]]
;; ;;  3
;; ;;  identity
;; ;;  2)




;; ;;gradient descent
;; create map of some resolution



;; confidence, exploitation


;; data point is a vector
;; [adv-inf, conf, payoff]


;; create a map of some resolution
;; [.2 .2 .2 .2 .2] conf exp-outcome

;; confidence is a function of points
;; exp-outcome is a function of conf and points

;; range of values from -1 to 1



;; income nodes can vary from 0 to 6
;; nodes with pods can vary from 0 to 2000, with 0-5 being most common

;; a


;; (defn generate-vectors
;;   [num-fields resolution]
;;   (let [vect (loop [num 0 end 1 acc []] (if (> num end) acc (recur (+ num resolution) end (conj acc num))))
;;         total-steps (count vect)]
;;     (loop [acc []])
;;     (loop [acc []
;;            fields-remaining num-fields
;;            next-step 0]
;;       (if (= 0 fields-remaining)
;;         acc
;;         (recur (conj acc [next-step])
;;                (dec fields-remaining)
;;                (+ next-step resolution)))
;;       )
;;   ;; (loop [field num-fields
;;   ;;        acc []]
;;   ;;   (if (= field 0)
;;   ;;     acc
;;   ;;     (recur (dec field) (conj acc 0))))
;;     ))

;; (defn helper-gen
;;   "Given a vector, resolution, and a col, returns set of vectors where col has every possible value"
;;   [vector start end resolution col]
;;   (loop [acc []
;;          mod-col 0]
;;     (if (= mod-col (count vector))
;;       acc
;;       (recur (reduce conj acc (loop [acc []
;;            num start]
;;       (if (> num end)
;;       acc
;;       (recur
;;        (conj acc (mod-vec vector num mod-col))
;;        (+ num resolution)

;;        ))))
;;            (inc mod-col)))
;;     ))




;; (helper-gen [0 0 0 0 0] 0 1 1/10 1)

;; (defn mod-vec
;;   [vector new-num col]
;;   (assoc vector col new-num))
;; (assoc [0 0 0 0 0] 2 3)
;; [[0] [0] [0] [0] [0]]


;; for every field in vect create a set of vectors of num vect


;; (generate-vectors 2 1/10)


;; represent each function a a vector
;; c1...cn are corrdinates of source point
;; y is actual payoff
;; t is turn occured on
;; [c1 c2 c3 y t]

;; where is max
;; c1 c2 c3
;; min is farthest point from max

;; [c1 c2 c3 y t]
;; [c1 c2 c3 y t]

;; z=y / dt + dt

;; given a min z from y and t, how to map back to c values


;; [0 0 0 10 1]
;; [1 2 2 5 2]

;; 7 3


;; 1 10   10/2 + 2
;; 2 5     5/1 + 1

;; z = - y - c
;; constraints


;; how to find global min of function
;; how to format modifier function




;; how am i picturing this
;; known points have z value
;; function for how z is modified around point is well-known
;; minimum of z could be sampled


;; order, flux

;; order has (pow n n) possibilities
;; flux can be constrained between 0 (resulting in .2, .2,...) and 1 (resulting in 0 0 1 0 0)
;; modification (f, d) = flux * (number of advisors / distance from center adv) * z



;; [2 5/2 1]

;; y=mx+b

;; points
;; [c1 c2 c3 y] [.2 .2 .2 5]
;; [c1 c2 c3 y] [.1 .1 .4 5]
;; [c1 c2 c3 y] [.2 .1 .3 5]
;; [c1 c2 c3 y] [.2 .3 .1 5]
