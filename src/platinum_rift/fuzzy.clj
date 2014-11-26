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
  [fuzz-map fuzz-rules]
  (loop [rules fuzz-rules
         acc {}]
    (if (empty? rules)
      acc
      (recur
       (next rules)
       (let [prem (premise (first rules))
             consq (consequent (first rules))
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
  [m fuzz-out]

  (loop [fuzzo fuzz-out
         acc {}]
    (if (empty? fuzzo)
      acc
      (recur (next fuzzo)
             (assoc acc (fo-key (first fuzzo)) (crisp-it (first fuzzo) (m (fo-key (first fuzzo)))))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY INPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defprotocol fuzz-in
;;   "Protocol for turning numbers into low/high logic"
;;   (fi-key [fuzzi]) ;;keyword for fuzzy variable
;;   (fuzz-it [fuzzi num]) ;;takes in the input variable and returns a map of [fuzzy-set-type truthiness] values and a [nil fuzzy-set-vector]
;;   (kv-pair [fuzzi num])) ;;convenience func that returns a vector with the fuzzy keyword and the fuzzy set for an input

;; (deftype ratio-fe []
;;   fuzz-in
;;   (name [fuzi] "Ratio of Friendly to Enemy pods")
;;   (my-key [fuzi] :ratio-fep)
;;   (fuzz-it [fuzi num]
;;     (cond
;;      (< num 1) {:vlo 1 nil [1]}
;;      (< num 1) :low
;;      (< num 1) :med
;;      (< num 1) :high
;;      (< num 1) :vhi
;;      (< num 1) :max))
;;   (kv-pair [fuzi num]
;;     [(my-key fuzi) (fuzz-it fuzi num)]))

(deftype ratio-fe-terr []
  fuzz-in
  (fi-key [fuzzi] :ratio-fe-terr)
  (fuzz-it [fuzzi num]
    (hash-map (fi-key fuzzi) {:lo (max (+ 0.5 (- 1 num)) 0)
                              :hi (max (+ 0.5 (- num 1)) 0)})
    ;; (cond
    ;;  (< num 0.5) (hash-map (fi-key fuzzi) {:lo (- 1 num)
    ;;                                      :low num
    ;;                                      :med 0
    ;;                                      :high 0
    ;;                                      :vhi 0
    ;;                                      :max 0})
    ;;  (< num 0.7) (hash-map (fi-key fuzzi) {:vlo num
    ;;                                      :low (- 1 num)
    ;;                                      :med (- 1 num)
    ;;                                      :high 0
    ;;                                      :vhi 0
    ;;                                      :max 0})

    ;;  )
    )
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

(defn get-fuzzy-inputs
  "Returns seq of fuzzy inputs to be used."
  []
  [(ratio-fe-terr.) (per-tot-inc.) (per-tot-terr.)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY OUTPUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defprotocol fuzz-out
;;   "Protocol for fuzzy output variables."
;;   (fo-key [fuzzo])
;;   (ideal-val [fuzzo])
;;   (crisp-it [fuzzo m])) ;;returns a number represented by a fuzzy set, given a map of {:fuzz-type val


(deftype importance []
  fuzz-out
  (fo-key [fuzzo] :importance)
  (ideal-val [fuzzo] {:lo 0
                      :hi 5})
  (crisp-it [fuzzo m] (let [[vec model] (loop [kv-pairs (seq (ideal-val fuzzo))
                                               [actual ideal] [[] []]]
                                          (if (empty? kv-pairs)
                                            [actual ideal]
                                            (let [act-val (m (first (first kv-pairs)))
                                                  ideal-val (second (first kv-pairs))]
                                              (recur (next kv-pairs)
                                                     [(conj actual (if act-val act-val 0))
                                                      (conj ideal ideal-val)]))
                                            ))]
                        (dot (normalize vec) model))))

;; (deftype agg []
;;   fuzz-out
;;   (fo-key [fuzo] :agg)
;;   (ideal-val [fuzo] {:vlo 1
;;                      :low 3
;;                      :med 7
;;                      :high 10
;;                      :vhi 15
;;                      :max 20})
;;   (crisp-it [fuzo m]
;;     (let [[vec model] (loop [kv-pairs (seq (ideal-val fuzo))
;;                              [actual ideal] [[] []]]
;;                         (if (empty? kv-pairs)
;;                           [actual ideal]
;;                           (let [act-val (m (first (first kv-pairs)))
;;                                 ideal-val (second (first kv-pairs))]
;;                           (recur (next kv-pairs)
;;                                  [(conj actual (if act-val act-val 0))
;;                                   (conj ideal ideal-val)]))
;;                           ))]
;;       (dot (normalize vec) model))))

;; (deftype panic []
;;   fuzz-out
;;   (fo-key [fuzo] :panic)
;;   (ideal-val [fuzo] {:vlo 0
;;                      :low 5
;;                      :med 90
;;                      :high 150
;;                      :vhi 170
;;                      :max 200})
;;   (crisp-it [fuzo m]
;;     (let [[vec model] (loop [kv-pairs (seq (ideal-val fuzo))
;;                              [actual ideal] [[] []]]
;;                         (if (empty? kv-pairs)
;;                           [actual ideal]
;;                           (let [act-val (m (first (first kv-pairs)))
;;                                 ideal-val (second (first kv-pairs))]
;;                           (recur (next kv-pairs)
;;                                  [(conj actual (if act-val act-val 0))
;;                                   (conj ideal ideal-val)]))))]
;;       (dot (normalize vec) model))))

(defn get-fuzzy-outputs
  "Returns seq of fuzzy outputs to be used."
  []
  [(importance.)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUZZY EXPERT RULES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defprotocol fuzz-rules
;;   "Protocol for rules to be applied to fuzzy variables."
;;   (premise [fuzzr]) ;;returns the premise representation of the rule
;;   (consequent [fuzzr])) ;;returns the consequent representation of the rule


(deftype cont-priority []
    fuzz-rules
  (premise [fuzzr] [[:per-tot-terr :low] [:per-tot-inc :high] [:ratio-fe-terr :high]])
  (consequent [fuzzr] [[:importance :high]]))

;; (deftype my-other-rule []
;;     fuzz-rules
;;   (premise [fuzzr] [[:terr :high] [:income :med]])
;;   (consequent [fuzzr] [[:agg :med] [:panic :high]])
;;   )


(defn get-fuzzy-rules
  "Returns seq of fuzzy rules to be used."
  []
  [(cont-priority.)])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (assoc

;; (assoc {} :hi :there)
;;     :stuff :things)




;; (let [comp {:panic {:med [0 2 3]
;;                     :high [0 2 3]}
;;             :agg {:low [0 1 2 3 4]}
;;             }
;;       rdy (composition comp)]


;;   (defuzzification (composition comp)
;;     [(agg.) (panic.)])


;;   ;; (fo-key (agg.))
;;   ;; (crisp-it (agg.))

;;   ;; (rdy :agg )
;;   ;; (seq comp)
;;   ;; (second )
;;   )







;; (loop [pair (seq m)
;;          acc m]
;;     (if (empty? pair)
;;       acc
;;       (recur (next pair)
;;              (assoc acc (first (first pair))
;;                     (crisp-it ())
;;                     )
;; (assoc-in acc)
;;              (loop [inner-pair (seq (second (first pair)))
;;                     inner-acc acc]
;;                (if (or (empty? inner-pair)
;;                        (nil? (first inner-pair)))
;;                  inner-acc
;;                  (recur (next inner-pair)
;;                         (assoc-in inner-acc [(first (first pair)) (first (first inner-pair))] (reduce + (second (first inner-pair))))))))))
;;   )

;; (seq (second (first {:things {:stuff [1 2 3]
;;                               :other [2 2 2]}
;;                      :things2 {:stuff [1 2 3]
;;                               :other [2 2 2]}})))
;; (let [comp {:panic {:med [0 1 2 3 4]
;;                     :high [0 2 3]}
;;             :agg {:low [0 1 2 3 4]}}]
;;   (composition comp)
;;   ;; (seq comp)
;;   ;; (second )
;;   )


;; (let [rules [(my-rule.) (my-other-rule.)]
;;       fuzz-map {:terr {:low 0.2
;;                        :med 0.8
;;                        :high 0.1}
;;                 :income {:med 0.8
;;                          :high 0.2}}

;;       prem (premise (first rules))
;;       consq (consequent (first rules))
;;       val (reduce *
;;                                     ;;get a list of truthiness for each premise val
;;                   (extract-all fuzz-map prem))
;;       types (map first consq)
;;       props (map second consq)
;;       ]
;;   ;; (extract-all fuzz-map prem)
;;   (composition (inference fuzz-map rules))

;;   ;; val
;;   ;; props
;;   ;; (reduce merge {} (map #(hash-map %1 (hash-map %2 %3) ) types props (cycle [val])))
;;   )












;; (reduce + (second (first (second (first {:hi {:there [5]
;;                      :stuff [7]}})))))


;; {:panic {:low 15}}


;; (extract {:hi {:thre 5}} [:asdf :assfa])


;; (let [m {}
;;       %1 a
;;       %2 b
;;       %3 b]

;;   (if (m %1)
;; (assoc-in m [%1 %2] %3)
;;     (hash-map %1 (hash-map %2 [%3]) )
;;     )
;;   )

;; (assoc-in {} [:hi :things] :there)

;; (merge {:stuff [0 1]} {:stuff [1 1 1 1]})






;; (premise (first [(my-rule.)]))

;; (let [rules [(my-rule.) (my-other-rule.)]
;;       fuzz-map {:terr {:low 0.2
;;                        :med 0.8
;;                        :high 0.1}
;;                 :income {:med 0.8
;;                          :high 0.2}}

;;       prem (premise (first rules))
;;       consq (consequent (first rules))
;;       val (reduce *
;;                                     ;;get a list of truthiness for each premise val
;;                   (extract-all fuzz-map prem))
;;       types (map first consq)
;;       props (map second consq)
;;       ]
;;   ;; (extract-all fuzz-map prem)
;; (inference fuzz-map rules)
;;   ;; val
;;   ;; props
;;   ;; (reduce merge {} (map #(hash-map %1 (hash-map %2 %3) ) types props (cycle [val])))
;;   )


;; (map #(hash-map %1 (hash-map %2 %3) ) '("hi") ["there"] [:things])


;; (hash-map 2 3)

;; (if (empty? consq)
;;            inner-acc
;;            (assoc inner-acc rule-fuzz-type (conj (inner-acc rule-fuzz-type) rule-fuzz-val))
;;            )

;; (reduce merge {:start :map} [{:things :stuff} {:yay :done}])
;; (let [fuzz-map {:terr {:low 0.2
;;                        :med 0.8}
;;                 :income {:med 0.8
;;                          :high 0.2}}
;;       prem [[:terr :med] [:income :high]]
;;       consq [[:agg :med] [:panic :high]]]

;;   (extract fuzz-map (first prem))
;;   ;; (map extract (repeatedly fuzz-map) prem)
;;   (extract-all fuzz-map prem)
;;   ;; (map extract (take 2 (cycle fuzz-map)) prem)
;;   ;; (map extract fuzz-map (first prem))
;;   ;; (map first consq)
;;   (reduce * (extract-all fuzz-map prem)))




;; (pull {:terr {:low 0.2
;;                        :med 0.8}
;;                 :income {:med 0.8
;;                          :high 0.2}} [:income :med])

;; ({:terr {:low 0.2}} [:terr :low])
;; premise
;; {:terr :high}

;; consq
;; {:aggressiveness :low
;;  :panic :high}

;; {:things {}}


;; each type has low,med,high map with values


;; (reduce * [0.1 0.1 0.1])
;; fuzz-in
;; (:terr {:low 0.8
;;         :high 0.2
;;         nil [0.8 0.2]})

;; {:agg {:low 0.8
;;         :high 0.2
;;         nil [0.8 0.2]}}


;; i want to get all values from fuzz in where the key from fuzz-in matches val from premise
;; [0.2]


;; :terr
;; {:aggressiveness {:low .2}
;;  :panic {:high .2}}



;; {:aggressiveness {:low .2}
;;  :panic {:high .2}}

;; {:aggressiveness {:low [.5 .3]}
;;  :panic {:high [.3] }}




;; (search :low )


;; {:agg {:low .6
;;        :high .3}}


;; (keys {:things :stuff})


;; fuzz-out

;; (empty-map {:high 0
;;             :low 0})



;; (filter-map {:test :things :stuff :junk :more :less} [:stuff :test  :nope]  )

;; (filter #(contains? % :a )  [:a :b] )
;; (filter [:test :more] {:test :things :stuff :junk :more :less} )
;; (loop [next-kv {:hi :there :bye :things nil :stuff}]
;;   (when (not (empty? next-kv))
;;     (println (first next-kv))
;;     (recur (next next-kv))
;;     ))
;; (merge {:hi :there :bye [[1 2 3]]} {:bye [4 5]})
;; (merge [[1 2 3]] [4 5])
;; (let [some-key :bye
;;       ag-map {:hi :there :bye [[1 2 3]]}
;;       rule-res {:bye [4 5]}]
;;   (assoc ag-map some-key (conj (ag-map some-key) (rule-res some-key))   )
;;   )
