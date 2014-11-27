(ns platinum-rift.fuzzy)


(defprotocol fuzz-in
  "Protocol for turning numbers into low/high logic"
  (fi-key [fuzzi]) ;;keyword for fuzzy variable
  (fuzz-it [fuzzi num]) ;;takes in the input variable and returns a map of [fuzzy-set-type truthiness] values and a [nil fuzzy-set-vector]
  (kv-pair [fuzzi num])) ;;convenience func that returns a vector with the fuzzy keyword and the fuzzy set for an input

(defprotocol fuzz-rules
  "Protocol for rules to be applied to fuzzy variables."
  (premise [fuzzr]) ;;returns the premise representation of the rule
  (consequent [fuzzr])) ;;returns the consequent representation of the rule

(defprotocol fuzz-out
  "Protocol for fuzzy output variables."
  (fo-key [fuzzo])
  (ideal-val [fuzzo])
  (crisp-it [fuzzo m])) ;;returns a number represented by a fuzzy set, given a map of {:fuzz-type val

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn filter-map
  "Takes in a map and a seq of keys. Returns the subset map values whose keys correspond to the input key list."
  [m kl]
  (loop [k kl
         acc []]
    (if (empty? k)
      acc
      (let [v (m (first k))]
        (recur (next k)
               (if v
                 (conj acc v)
                 acc))))))

(defn extract
  ""
  [m v]
  (loop [k v
         inner m]
    (if (or (nil? inner )
            (empty? k))
      inner
      (recur (next k)
             (inner (first k))))))

(defn extract-all
  ""
  [m v]
  (loop [next-v v
         acc []]
    (if (empty? next-v)
      acc
      (recur (next next-v)
             (conj acc
                   (loop [k (first next-v)
                          inner m]
                     (if (empty? k)
                       inner
                       (recur (next k)
                              (inner (first k))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VECTOR FUNCTIONS FOR FUZZY SETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dot
  "Returns dot product of two vectors."
  [v1 v2]
  (reduce #(+ %1 (* (first %2) (second %2)))
          0
          (partition 2 (interleave v1 v2))))

(defn normalize
  "Returns the normalized version of the passed in vector."
  [v1]
  (let [sum (reduce + v1)
        sum (if (> sum 0) sum 1)]
    (map #(/ % sum) v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inference
  "Takes in a map of [fuzzy-input-type fuzzy-set], and a seq of fuzz-rules to evaluate. Returns a map of {:fuzzy-output-type {:fuzz-prop [val1 val2 ...]}}
  Applies fuzzy inputs to rules, and tracks output fuzzy sets by type."
  [fuzz-rules fuzz-map]
  (println (str  "Fuzz-map:
" fuzz-map))
  (loop [rules fuzz-rules
         acc {}]
    (if (empty? rules)
      acc
      (recur
       (next rules)
       (let [prem (premise (first rules))
             debug-p (println (str "Premise is: " prem))
             consq (consequent (first rules))
             debug-c (println (str "Consequent is: " consq))
             ;;multiply truthiness of all premises
             val (reduce *
                         ;;get a list of truthiness for each premise val
                         (extract-all fuzz-map prem))

             types (map first consq) ;;the list of consequent fuzz-types (i.e. height)
             props (map second consq)] ;;the list of qualitative descriptions for the fuzzy type (i.e. tall, if the fuzz-type was height)
         (reduce
          #(if (extract %1 [(nth %2 0) (nth %2 1) ])
             (assoc-in %1
                       [(nth %2 0) (nth %2 1) ] (conj (extract %1 [(nth %2 0) (nth %2 1) ]) (nth %2 2)) )
             (assoc-in %1
                       [(nth %2 0) (nth %2 1) ] [(nth %2 2)]))
          acc
          (partition 3 (interleave types props (cycle [val])))))))))



(defn composition
  "Composes inference results"
  [m]
  (println "COMPOSITION:
" m)
  (loop [pair (seq m)
         acc m]
    (if (empty? pair)
      acc
      (recur (next pair)
             (loop [inner-pair (seq (second (first pair)))
                    inner-acc acc]
               (if (or (empty? inner-pair)
                       (nil? (first inner-pair)))
                 inner-acc
                 (recur (next inner-pair)
                        (assoc-in inner-acc [(first (first pair)) (first (first inner-pair))] (reduce + (second (first inner-pair)))))))))))


(defn defuzzification
  ""
  [fuzz-out m]
  (println "COMPOSITION:
" m)
  (println "COMPOSITION:
" fuzz-out)
  (loop [fuzzo fuzz-out
         acc {}]
    (if (empty? fuzzo)
      acc
      (recur (next fuzzo)
             (assoc acc (fo-key (first fuzzo)) (crisp-it (first fuzzo) (m (fo-key (first fuzzo)))))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY INPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype ratio-fe-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-fe-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype per-tot-inc []
  fuzz-in
  (fi-key [fuzzi] :per-tot-inc)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.3 num)) 0)
                              :hi (max (+ 0.5 (- num 0.3)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype per-tot-terr []
  fuzz-in
  (fi-key [fuzzi] :per-tot-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.3 num)) 0)
                              :hi (max (+ 0.5 (- num 0.3)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype tot-e-pods []
  fuzz-in
  (fi-key [fuzzi] :tot-e-pods)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.3 num)) 0)
                              :hi (max (+ 0.5 (- num 0.3)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-fe-pods []
  fuzz-in
  (fi-key [fuzzi] :ratio-fe-pods)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-ne-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-ne-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))

(deftype ratio-nf-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-nf-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)}))
  (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))


;; (deftype tot-f-terr []
;;   fuzz-in
;;   (fi-key [fuzzi] :tot-f-terr)
;;   (fuzz-it [fuzzi num]
;;     (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 0.3 num)) 0)
;;                               :hi (max (+ 0.5 (- num 0.3)) 0)}))
;;   (kv-pair [fuzzi num] [(fi-key fuzzi) (fuzz-it fuzzi num)]))


(defn get-fuzzy-inputs
  "Returns seq of fuzzy inputs to be used."
  []
  [(ratio-fe-terr.) (per-tot-inc.) (per-tot-terr.) (tot-e-pods.) (ratio-fe-pods.) (ratio-ne-terr.) (ratio-nf-terr.)
   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY OUTPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gen-crisp
  [fuzzo m]
  (let [[vec model] (loop [kv-pairs (seq (ideal-val fuzzo))
                                               [actual ideal] [[] []]]
                                          (if (empty? kv-pairs)
                                            [actual ideal]
                                            (let [act-val (m (first (first kv-pairs)))
                                                  ideal-val (second (first kv-pairs))]
                                              (recur (next kv-pairs)
                                                     [(conj actual (if act-val act-val 0))
                                                      (conj ideal ideal-val)]))
                                            ))]
    (dot (normalize vec) model)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; banker advisor output

(deftype banker-inf []
  fuzz-out
  (fo-key [fuzzo] :banker-inf)
  (ideal-val [fuzzo] {:lo 10
                      :hi 40})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unit advisor output
(deftype unit-f-inf []
  fuzz-out
  (fo-key [fuzzo] :unit-f-inf)
  (ideal-val [fuzzo] {:lo (- 100)
                      :hi 20})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

(deftype unit-e-inf []
  fuzz-out
  (fo-key [fuzzo] :unit-e-inf)
  (ideal-val [fuzzo] {:lo (- 50)
                      :hi 50})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; territory adviser output
(deftype terr-f-inf []
  fuzz-out
  (fo-key [fuzzo] :terr-f-inf)
  (ideal-val [fuzzo] {:lo 50
                      :hi (- 100)})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

(deftype terr-e-inf []
  fuzz-out
  (fo-key [fuzzo] :terr-e-inf)
  (ideal-val [fuzzo] {:lo 50
                      :hi (- 200)})
  (crisp-it [fuzzo m] (gen-crisp fuzzo m)))

(defn get-fuzzy-outputs
  "Returns seq of fuzzy outputs to be used."
  []
  [(banker-inf.)
   (unit-f-inf.)
   (unit-e-inf.)
   (terr-f-inf.)
   (terr-e-inf.)
   ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY EXPERT RULES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype cont-priority []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :lo] [:per-tot-inc :hi] [:ratio-fe-terr :hi]])
  (consequent [fuzzr] [[:banker-inf :hi]]))

(deftype avoid-small-cont []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :lo] [:per-tot-inc :lo]])
  (consequent [fuzzr] [[:terr-f-inf :lo] [:terr-e-inf :lo]]))

(deftype dont-lose-asia []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :hi] [:ratio-fe-terr :lo]])
  (consequent [fuzzr] [[:terr-f-inf :hi] [:unit-f-inf :hi] [:terr-e-inf :hi] [:unit-e-inf :hi]]))

(deftype take-undefended-cont []
    fuzz-rules
  (premise [fuzzr] [[:ratio-fe-pods :hi] [:ratio-ne-terr :hi]])
  (consequent [fuzzr] [[:terr-e-inf :hi] [:unit-e-inf :lo] [:unit-f-inf :hi] [:unit-e-inf :lo]]))

(deftype start-strong []
    fuzz-rules
  (premise [fuzzr] [[:ratio-nf-terr :hi] [:ratio-ne-terr :hi]])
  (consequent [fuzzr] [[:banker-inf :hi] [:terr-e-inf :hi] [:unit-e-inf :hi] [:unit-f-inf :hi]]))

(deftype hunt-and-kill []
    fuzz-rules
  (premise [fuzzr] [[:ratio-fe-terr :hi] [:ratio-fe-pods :hi]])
  (consequent [fuzzr] [[:banker-inf :lo] [:terr-e-inf :hi] [:unit-e-inf :hi] [:unit-f-inf :hi]]))


(defn get-fuzzy-rules
  "Returns seq of fuzzy rules to be used."
  []
  [(cont-priority.) (avoid-small-cont.) (dont-lose-asia.) (start-strong.) (hunt-and-kill.);;(take-undefended-cont.)
   ])
