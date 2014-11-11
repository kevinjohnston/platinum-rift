(ns platinum-rift.player
  (:require [platinum-rift.world :as world]))




;;function to evaluate players
(defn player-stats [p1 world]
  (let [ag-inc (agent 0) ;;totals income for player
        ag-terr (agent 0) ;;totals territory controlled by the player
        ag-lib (agent 0) ;;totals liberties of player
        ]
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
    {:player p1
     :income @ag-inc
     :territories @ag-terr
     :liberties @ag-lib}))

(defn evaluate
  "Quantifies players position."
  [p1 turn]
  ;;todo make this useful
  (+ (:income p1) (- (:liberties p1))  (:territories p1))
  )

(map
 #(:id %1)
 (map #(:sub-zones %1) (:sub-zones @world/world)))


(map #(:id %) (first (map #(:sub-zones %) (first (map #(:sub-zones %1) (:sub-zones @world/world))) )))


(count
 (:sub-zones
 (:sub-zones @world/world) ) )
(if (:sub-zones @world/world) true false)

(world/reset-world)
@world/world
(def ag-test (agent 0))
@ag-test
           (send ag-test inc)

(evaluate (eval-world 0 @world/world) 1)



(let [test-map
      [
{ :val 0
 :inner
 [{:val 0
   :inner [
           {:inner nil :val 5 }
           {:inner nil :val 0 }
           {:inner nil :val 0 }]
   }
  {

   :val 0
   :inner [
           {:inner nil :val 3 }
           {:inner nil :val 0 }
           {:inner nil :val 1 }]
   }]}]
      ]

  (map #(:val %)
      (first  (map #(:inner %)
     test-map
     ))
       )

)





(
 [
  {:open-liberties 0,
   :value 0,
   :total-liberties 0,
   :priority 0,
   :last-eval 0,
   :id 1003,
   :player 0,
   :sub-zones
   ({:open-liberties 0,
     :value 0,
     :total-liberties 0,
     :priority 0,
     :last-eval 0,
     :id 0,
     :player 0,
     :sub-zones nil,
     :income 5}
    {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 2, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 3, :player 0, :sub-zones nil, :income 1}), :income 2} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1004, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 7, :player 0, :sub-zones nil, :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 13, :player 0, :sub-zones nil, :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         0, :last-eval 0, :id 19, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 14, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 20, :player 0, :sub-zones nil, :income 1}), :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1005, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 4, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 8, :player 0, :sub-zones nil, :income 1} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 9, :player 0, :sub-zones nil, :income 2} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 15, :player 0, :sub-zones nil, :income 3}), :income 1}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1006, :player
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 5, :player 0, :sub-zones nil, :income 1} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 10, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 6, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 11, :player 0, :sub-zones nil, :income 4} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 12, :player 0, :sub-zones nil, :income 2}), :income 1} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1007, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 16, :player 0, :sub-zones nil, :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 22, :player 0, :sub-zones nil, :income 5} {:open-liberties 0, :value
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                0, :total-liberties 0, :priority 0, :last-eval 0, :id 17, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 23, :player 0, :sub-zones nil, :income 0}), :income 2} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1008, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 29, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 38, :player 0, :sub-zones nil, :income 1} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 21, :player 0, :sub-zones nil, :income 5} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 28, :player 0, :sub-zones nil, :income 5}), :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1009, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, :id 27, :player 0, :sub-zones nil, :income 2} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 37, :player 0, :sub-zones nil, :income 1} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 43, :player 0, :sub-zones nil, :income 0}), :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 1010, :player 0, :sub-zones ({:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 46, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 47, :player 0, :sub-zones nil, :income 3} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 48, :player 0, :sub-zones nil, :income 0} {:open-liberties 0, :value 0, :total-liberties 0, :priority 0, :last-eval 0, :id 49, :player 0, :sub-zones nil, :income 1}), :income 3}])
