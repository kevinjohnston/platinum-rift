(ns Player
  (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [playerCount (read) myId (read) zoneCount (read) linkCount (read)]
    ; playerCount: the amount of players (2 to 4)
    ; myId: my player ID (0, 1, 2 or 3)
    ; zoneCount: the amount of zones on the map
    ; linkCount: the amount of links between all zones
    (loop [i zoneCount]
      (when (> i 0)
        (let [zoneId (read) platinumSource (read)]
          ; zoneId: this zone's ID (between 0 and zoneCount-1)
          ; platinumSource: the amount of Platinum this zone can provide per game turn
        (recur (dec i)))))
    (loop [i linkCount]
      (when (> i 0)
        (let [zone1 (read) zone2 (read)]
        (recur (dec i)))))
    (while true
      (let [platinum (read)]
        ; platinum: my available Platinum
        (loop [i zoneCount]
          (when (> i 0)
            (let [zId (read) ownerId (read) podsP0 (read) podsP1 (read) podsP2 (read) podsP3 (read)]
              ; zId: this zone's ID
              ; ownerId: the player who owns this zone (-1 otherwise)
              ; podsP0: player 0's PODs on this zone
              ; podsP1: player 1's PODs on this zone
              ; podsP2: player 2's PODs on this zone (always 0 for a two player game)
              ; podsP3: player 3's PODs on this zone (always 0 for a two or three player game)
            (recur (dec i)))))

        ; (binding [*out* *err*]
        ;   (println "Debug messages..."))

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println "WAIT")
        (println "1 73")))))
