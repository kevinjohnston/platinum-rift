(ns platinum-rift.advisors
  (:require [platinum-rift.world :as world]))

(def banker-inf (atom 1))

(defprotocol advisor
  "Protocol for evaluating board."
  (name [adv] "Name of advisor")
  (evaluate [adv zone] "Returns map of zone-id (key) and priority level (value).")
  (influence [adv] [adv adjust] "Returns advisors influence, possibly adjusting that level")
  )

(deftype banker []
  advisor
  (name [adv] "Banker")
  (evaluate [adv zone] (if (:sub-zones zone) (reduce + (map #(evaluate adv %) (:sub-zones zone))) (:income zone)))
  (influence [adv] @banker-inf)
  (influence [adv adjust] (swap! banker-inf #(+ % adjust))))



;;how to model map priorities


;;order world by priority of zones
(sort-by identity '(1 2 3 2 0))
(:priority (first (sort-by :priority (:sub-zones @world/world) )))


(sort-by :priority [{:priority -2} {:priority -4}])






(world/reset-world)
@world/graph-world

@world/world
(evaluate (banker.) @world/world)

(first (:sub-zones (first (:sub-zones (first (:sub-zones @world/world) )))))

(def agent-test (agent 0))

@agent-test
(send agent-test (fn [_] (println @agent-test) _))
(send agent-test (fn [_] 5))

(let [ag (agent 0)]
  (send
   ag
   (fn [_]
             (dosync
     (println
           (loop [count 100000]
     (send ag dec)
     (send ag inc)
     (send ag inc)
     (send ag inc)
     (send ag inc)
     (if (= count 0)
       @ag
       (recur (dec count))))


             )) _)
   )

  (println "finished again"))
