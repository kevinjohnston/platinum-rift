(ns platinum-rift.strategies
  (:require [platinum-rift.world :as world]))

(def curiosity (atom 1))

(def inc-payoff (atom 0.5))
(def inc-trials (atom 1))

;;Strategies used to evaluate game board

;;strategy abstraction
;;needs to specify goal (increase platinum reserves, gain territory reduce enemey units)

(defn strat-hoard
  "Takes in player stats, returns desired appearance"
  [player]
  (assoc player :platinum (* 1.2 (player :platinum))))


(defn pick-strat
  "Returns the best strategy given balancing need to explore and need to exploit best known strat."
  []
  (first (sort (fn [strat]
    (*
    ;;randomness
    (/ (+ 90 (rand-int 10)) 100)
    ;;exploitation factor
    (* (confidence (trials strat))
       (payoff strat))
    @curiosity
    ))
        [(income.)] )))

(defn confidence
  ""
  [trials]
  (- 1 (/ trials)))


(defn strat-expand
  "Takes in player stats, returns desired appearance"
  [player]
  (assoc player :income (* 1.2 (inc (player :income)) )))

(defprotocol strategy
  "Protocol for overall strategy"
  (name [strat] "Name of strat for debugging")
  (evaluate [strat p-old p-new] "Method to return evaulations of two player states")
  (goal [strat player] "Method to return desired player state given current player state")
  (payoff [strat] "Returns current average payoff for this strategy")
  (trials [strat] "Returns trials of strategy run so far")
  (prioritize [strat zone] "Returns a priority given a zone-map"))

(deftype hoard []
  strategy
  (name [strat] "Hoard")
  (evaluate [strat p-old p-new] (/ (:platinum p-new) (:platinum (goal strat p-old))))
  (goal [strat player] (assoc player :platinum (* 1.2 (inc (player :platinum))))))

(defrecord income []
  strategy
  (name [strat] "Income")
  (evaluate [strat p-old p-new] (/ (:platinum p-new) (:platinum (goal strat p-old))))
  (goal [strat player] (assoc player :platinum (* 1.2 (inc (player :platinum)))))
  (payoff [strat] @inc-payoff)
  (trials [strat] @inc-trials)
  (prioritize [strat zone] ))

(extends? strategy income)

;; (let [player1 {:platinum 20}]
;;   (evaluate (hoard.) {:platinum 20} {:platinum 30})

;;   )
;; (payoff (hoard.)  )

;;protocol for strategies
;;

;;methods for all strategies
;;evalutate (takes a two player ds and returns some measurement for how they differ)
;;goal (takes a current player ds and returns a player ds representing desired result)
;;past-success (returns value of atom for how well this strategy has worked in the past)
;;trials (returns number of trials run)
;;prioritize (returns how to prioritize a zone)

;;goals should differ based on the stage of the game



;; (defprotocol Fly
;;   "A simple protocol for flying"
;;   (fly [this] "Method to fly"))

;; (defrecord Bird [name species]
;;   Fly
;;   (fly [this] (str (:name this) " flies...")))

;; (extends? Fly Bird)
;; -> true

;; (def crow (Bird. "Crow" "Corvus corax"))

;; (fly crow)
;; -> "Crow flies..."
