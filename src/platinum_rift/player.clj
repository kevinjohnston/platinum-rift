(ns platinum-rift.player
  (:require [platinum-rift.world :as world]))


(def starting-plat 200)
(def next-id (atom 0))

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
  (+ (:platinum p1) (:pods p1) (:income p1) (- (:liberties p1))  (:territories p1)))

(defn new-player
  "Creates and returns a new player."
  []
  {:player (swap! next-id #(inc %))
   :platinum starting-plat
   :income 0
   :territories 0
   :liberties 0
   :pods 0})


(defn reset
  ""
  []
  (swap! next-id #(when % 0)))
