(ns semantic-namespace.compound.identity
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce registry (atom {}))

(defn query [find*]
  (let [find* (set find*)]
    (->> (keys @registry)
         (filter (fn [x] (set/superset? x find*)))
         (mapv (fn [x] (vec (sort (vec x))))))))

(defn remove* [composed-identity]
  (swap! registry dissoc composed-identity))

(defn valid? [composed-identity]
  (and (set? composed-identity) (>= (count composed-identity) 2) (every? qualified-keyword? composed-identity)))

(defn exists? [compound-identity value]
  (let [internal-value (get @registry compound-identity)]
    [(boolean internal-value) (= internal-value value)]
    ))

(defn fetch [identity]
  (get @registry identity))


;; =============================================================================
;; Discovery & Navigation
;; =============================================================================

(defn all-identities
  "Returns all compound identities in the registry."
  []
  (vec (keys @registry)))

(defn find-with
    "Find all identities containing the given aspect(s).
   aspect can be a keyword or set of keywords."
  [aspect]
  (let [aspect-set (if (set? aspect) aspect #{aspect})]
    (->> @registry
         (filter (fn [[k _]] (set/superset? k aspect-set)))
         (into {}))))

(defn find-exact
  "Find identities that exactly match the given set."
  [identity-set]
  (get @registry identity-set))

(defn related-to
    "Find all identities that share at least one aspect with the given aspect.
   Returns a frequency map of co-occurring aspects."
  [aspect]
  (->> (all-identities)
       (filter #(contains? % aspect))
       (mapcat identity)
       (remove #{aspect})
       (frequencies)
       (sort-by val >)
       (into {})))

(defn semantic-neighbors
  "Find identities that are semantically related (share aspects)."
  [identity-set]
  (let [aspects (vec identity-set)]
    (->> (all-identities)
         (map (fn [other]
                {:identity other
                 :shared (set/intersection identity-set other)
                 :similarity (/ (count (set/intersection identity-set other))
                                (count (set/union identity-set other)))}))
         (filter #(and (pos? (:similarity %))
                       (not= identity-set (:identity %))))
         (sort-by :similarity >))))


;; =============================================================================
;; Identity Analytics
;; =============================================================================

(defn identity-stats
  "Returns statistics about identity usage in the system."
  []
  (->> (all-identities)
       (map (fn [id]
              {:identity id
               :size (count id)
               :value (get @registry id)}))
       (sort-by :size >)))

(defn aspect-frequency
  "Returns frequency of individual aspects across all identities."
  []
  (->> (all-identities)
       (mapcat identity)
       (frequencies)
       (sort-by val >)
       (into {})))

(defn singletons
  "Find identities that are used only once (potential orphans)."
  []
  (let [freq (aspect-frequency)]
    (->> (all-identities)
         (filter (fn [id-set]
                   (some #(= 1 (get freq %)) id-set))))))

(defn most-connected
  "Find aspects that appear in the most compound identities."
  [n]
  (take n (aspect-frequency)))

(defn clusters
  "Group identities by their primary namespace."
  []
  (reduce (fn [acc id-set]
            (reduce (fn [inner-acc aspect]
                      (let [ns-part (namespace aspect)]
                        (update inner-acc (keyword ns-part)
                                (fnil conj #{}) id-set)))
                    acc
                    id-set))
          {}
          (all-identities)))


;; =============================================================================
;; Advanced Querying
;; =============================================================================

(defn query-algebra
    "Query identities using set algebra operations.
   ops: {:intersection [...], :union [...], :difference [...]}"
  [ops]
  (let [all-ids (set (all-identities))]
    (cond
      (:intersection ops)
      (filter (fn [id]
                (every? #(set/superset? id %) (:intersection ops)))
              all-ids)

      (:union ops)
      (filter (fn [id]
                (some #(set/superset? id %) (:union ops)))
              all-ids)

      (:difference ops)
      (let [[include exclude] (:difference ops)]
        (filter (fn [id]
                  (and (set/superset? id include)
                       (empty? (set/intersection id exclude))))
                all-ids))

      :else
      (throw (ex-info "Unknown operation" {:ops ops})))))

(defn where
  "Query identities with a predicate function."
  [pred]
  (->> @registry
       (filter (fn [[k v]] (pred k v)))
       (into {})))

(defn missing-aspects
    "Find identities that might be missing common aspects.
   Returns suggestions based on correlation."
  [identity-set]
  (let [similar (semantic-neighbors identity-set)
        all-aspects (->> similar
                         (mapcat :identity)
                         (frequencies)
                         (sort-by val >))]
    (->> all-aspects
         (remove (fn [[aspect _]] (contains? identity-set aspect)))
         (take 5)
         (map (fn [[aspect freq]]
                {:aspect aspect
                 :correlation (/ freq (count similar))})))))


;; =============================================================================
;; Dependency Analysis
;; =============================================================================

(defn dependency-graph
  "Build a dependency graph based on shared aspects."
  []
  (let [all-ids (all-identities)]
    (reduce (fn [graph id]
              (assoc graph id
                     {:depends-on (semantic-neighbors id)
                      :depended-by (filter #(and (not= id %)
                                                 (not-empty (set/intersection id %)))
                                           all-ids)}))
            {}
            all-ids)))

(defn transitive-deps
  "Find all transitive dependencies for an identity."
  [identity-set]
  (loop [to-visit [identity-set]
         visited #{}
         deps #{}]
    (if (empty? to-visit)
      deps
      (let [current (first to-visit)
            neighbors (->> (semantic-neighbors current)
                           (map :identity)
                           (remove visited))]
        (recur (concat (rest to-visit) neighbors)
               (conj visited current)
               (into deps neighbors))))))



;; =============================================================================
;; Refactoring Support
;; =============================================================================

(defn suggest-identity
  "Suggest additional aspects for an identity based on patterns."
  [identity-set]
  (let [missing (missing-aspects identity-set)]
    {:current identity-set
     :suggestions (map :aspect missing)
     :rationale missing}))

(defn find-anomalies
  "Find unusual or potentially problematic identity combinations."
  []
  (let [all-ids (all-identities)]
    (->> all-ids
         (filter (fn [id]
                   (or
                    ;; Contradictory aspects
                    (and (contains? id :sync/operation)
                         (contains? id :async/operation))
                    ;; Missing common pairs
                    (and (contains? id :api/endpoint)
                         (not (contains? id :auth/required)))))))))

(defn rename-aspect
  "Helper to show impact of renaming an aspect."
  [old-aspect new-aspect]
  (let [affected (find-with old-aspect)]
    {:affected-count (count affected)
     :affected-identities (keys affected)
     :proposed-changes (map (fn [id]
                              {:old id
                               :new (-> id
                                        (disj old-aspect)
                                        (conj new-aspect))})
                            (keys affected))}))

;; =============================================================================
;; Visualization Support
;; =============================================================================

(defn to-graphviz
  "Generate Graphviz DOT format for visualization."
  []
  (let [all-ids (all-identities)
        edges (for [id all-ids
                    aspect id]
                [aspect id])]
    (str "digraph CompoundIdentity {\n"
         "  rankdir=LR;\n"
         "  node [shape=box];\n"
         (str/join "\n"
                   (for [[aspect id] edges]
                     (format "  \"%s\" -> \"%s\";"
                             (pr-str aspect)
                             (pr-str id))))
         "\n}")))

(defn to-mermaid
  "Generate Mermaid diagram format."
  []
  (let [clusters (clusters)]
    (str "graph TD\n"
         (str/join "\n"
                   (for [[ns-key ids] clusters]
                     (str "  subgraph " (name ns-key) "\n"
                          (str/join "\n"
                                    (for [id ids]
                                      (format "    %s[\"%s\"]"
                                              (hash id)
                                              (str/join ", " (map name id)))))
                          "\n  end"))))))

(defn correlation-matrix
  "Build a correlation matrix of aspect co-occurrence."
  []
  (let [aspects (keys (aspect-frequency))
        all-ids (all-identities)]
    (into {}
          (for [a1 aspects]
            [a1 (into {}
                      (for [a2 aspects]
                        [a2 (count (filter #(and (contains? % a1)
                                                 (contains? % a2))
                                           all-ids))]))]))))


;; =============================================================================
;; REPL Development Support
;; =============================================================================

(defn def-temp
  "Define a temporary identity for experimentation."
  [identity-set value]
  (swap! registry assoc
         (conj identity-set :temp/experimental)
         value)
  identity-set)

(defn clear-temp
  "Remove all temporary identities."
  []
  (let [temps (find-with :temp/experimental)]
    (doseq [id (keys temps)]
      (remove* id))
    (count temps)))

(defn promote-temp
  "Promote a temporary identity to permanent."
  [temp-identity]
  (when-let [value (get @registry temp-identity)]
    (let [permanent-id (disj temp-identity :temp/experimental)]
      (swap! registry assoc permanent-id value)
      (remove* temp-identity)
      permanent-id)))


;; =============================================================================
;; Documentation Generation
;; =============================================================================

(defn describe
  "Generate a description of an identity."
  [identity-set]
  (let [value (find-exact identity-set)
        neighbors (semantic-neighbors identity-set)
        missing (missing-aspects identity-set)]
    {:identity identity-set
     :aspects (sort (vec identity-set))
     :value value
     :semantic-neighbors (take 3 neighbors)
     :suggested-aspects (take 3 (map :aspect missing))
     :usage-count (count (find-with identity-set))}))

(defn generate-docs
  "Generate documentation for all identities."
  []
  (let [all-ids (all-identities)
        by-namespace (clusters)]
    {:total-identities (count all-ids)
     :namespaces (keys by-namespace)
     :aspect-frequency (take 10 (aspect-frequency))
     :clusters by-namespace
     :anomalies (find-anomalies)}))

;; =============================================================================
;; Summary Report
;; =============================================================================

(defn summary
  "Generate a comprehensive summary of the identity system."
  []
  (let [all-ids (all-identities)
        aspects (aspect-frequency)
        clustered (clusters)]
    (println "=== Compound Identity System Summary ===")
    (println (format "Total Identities: %d" (count all-ids)))
    (println (format "Unique Aspects: %d" (count aspects)))
    (println (format "Namespaces: %s" (str/join ", " (keys clustered))))
    (println "\nTop Aspects:")
    (doseq [[aspect count] (take 5 aspects)]
      (println (format "  %s: %d" aspect count)))
    (println "\nLargest Identities:")
    (doseq [id (take 3 (sort-by count > all-ids))]
      (println (format "  %s (%d aspects)"
                       (str/join ", " (map name id))
                       (count id))))
    (println "\nPotential Issues:")
    (doseq [anomaly (find-anomalies)]
      (println (format "  - %s" (pr-str anomaly))))))
