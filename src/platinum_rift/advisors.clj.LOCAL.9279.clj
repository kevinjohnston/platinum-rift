(ns platinum-rift.advisors)

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
