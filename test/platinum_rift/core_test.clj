(ns platinum-rift.core-test
  (:require [clojure.test :refer :all]
            [platinum-rift.core :refer :all]
            [platinum-rift.mimic :refer :all]
            [platinum-rift.constants :refer :all]
            [platinum-rift.world :as world]
            [platinum-rift.player :as player]))

;; (deftest a-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))

(defn test-setup
  "Sets up the ai"
  [player-id info-q]
  [(partial mread info-q)
   (partial mprintln player-id)])

(deftest ai1-test
  []
  (let [player-id 0
        num-players 2
        num-nodes 154
        info-queues (take num-players (cycle [(atom [])]))
        read-q (nth info-queues player-id)
        official-world (world/new-world num-nodes)
        official-players (create-players num-players 0 num-nodes)
        [mread mprintln] (test-setup player-id read-q)]
    ;; (map #(partial setup-first-turn num-players %1 %2 %3) info-queues (range num-players) (cycle [[official-world official-players]]))
    (setup-first-turn num-players (nth info-queues player-id) player-id [official-world official-players])
    (ai1 mread mprintln)
;;    (is (= nil ))
    )

  ;; (testing "Test the ai for bugs"
  ;;   (is (= 0 1)))
  )

(ai1-test)


(let [player-id 0
        num-players 2
        num-nodes 154
        info-queues (take num-players (cycle [(atom [])]))
        read-q (nth info-queues player-id)
        official-world (world/new-world num-nodes)
        official-players (create-players num-players 0 num-nodes)
        [mread mprintln] (test-setup player-id read-q)]
    (setup-first-turn num-players (nth info-queues player-id) player-id [official-world official-players])
    (ai1 mread mprintln)
;;    (is (= nil ))
    )

(deftest det-move
  ""
  []
  (let [sight 3
        p1 (assoc-in (create-players 1 0 154) [0 :pods 0] 2)
        world (assoc-in (world/new-world 154) [0 :pods 0] 2)]
  ;;ensure existing bug doesn't appear again
  (testing "Ensure existing malformed return value doesn't reappear."
  (is (not (= (count (first (player/det-move sight (first p1) world))) 1)))      )
    ;; (player/det-move sight (first p1) world)
    ;; (:pods (first p1))
(println (player/det-move sight (first p1) world))
    ))
