(ns semantic-namespace.compound.identity
  "Semantic kernel providing compound-identity algebra.
   Each identity is a set of qualified keywords representing composed meaning.
   All operations are deterministic, data-oriented, and side-effect free,
   except for explicit registry mutation helpers."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; =============================================================================
;; Registry
;; =============================================================================

(def ^:dynamic env-id #{})

(def registry
  (atom {}))

;; =============================================================================
;; Validation & Registry
;; =============================================================================

(defn valid?
  "Returns true if `id` is a valid compound identity.
   A valid identity is a set of at least 2 qualified keywords."
  ([id]
   (valid? id false))
  ([id verbose?]
   (cond
     (not (set? id)) (if verbose? [:error "not-a-set"] false)
     (< (count id) 2) (if verbose? [:error "too-few-elements"] false)
     (not-every? qualified-keyword? id) (if verbose? [:error "unqualified-keywords"] false)
     :else true)))

(defn fetch
  "Fetch the value associated with a compound identity."
  [identity]
  (get @registry identity))

(defn exists?
  "Check if a compound identity exists in the registry and whether its value matches `value`.
   Returns {:exists bool :matches bool}."
  [compound-identity value]
  (let [found (get @registry compound-identity)]
    {:exists (contains? @registry compound-identity)
     :matches (= found value)}))

(defn remove*
  "Remove a compound identity from the registry."
  [compound-identity]
  (swap! registry dissoc compound-identity))

(defn register!
  "Register a new compound identity and its associated value.
   Asserts validity before inserting.

   Two arities:
   - [id value]: Register without dev-id
   - [dev-id id value]: Augment value with :dev/id"
  ([id value]
   (assert (valid? id))
   (swap! registry assoc id value)
   id)
  ([dev-id id value]
   (assert (valid? id))
   (swap! registry assoc id (assoc value :dev/id dev-id))
   id))

;; =============================================================================
;; Querying & Discovery
;; =============================================================================

(defn all-identities
  "Return a vector of all compound identities in the registry."
  []
  (vec (keys @registry)))

(defn query
  "Return all compound identities that are supersets of `find*`."
  [find*]
  (let [find* (set find*)]
    (->> (keys @registry)
         (filter #(set/superset? % find*))
         (map #(vec (sort %)))
         (sort)
         (vec))))

(defn find-with
  "Find all identities containing the given aspect(s).
   Returns a plain map preserving deterministic order by identity size."
  [aspect]
  (let [aspect-set (if (set? aspect) aspect #{aspect})]
    (->> @registry
         (filter (fn [[k _]] (set/superset? k aspect-set)))
         (sort-by (comp count key))
         (into (array-map)))))

(defn find-exact
  "Find the exact identity and its value if registered."
  [identity-set]
  (get @registry identity-set))

(defn related-to
  "Return a frequency map of co-occurring aspects related to a given aspect."
  [aspect]
  (->> (all-identities)
       (filter #(contains? % aspect))
       (mapcat identity)
       (remove #{aspect})
       (frequencies)
       (sort-by val >)
       (into {})))

(defn semantic-neighbors
  "Return a list of related identities with similarity metrics.
   Each result has {:identity :shared :similarity}."
  [identity-set]
  (->> (all-identities)
       (keep (fn [other]
               (let [shared (set/intersection identity-set other)
                     union (set/union identity-set other)
                     sim (if (seq union)
                           (/ (count shared) (count union))
                           0)]
                 (when (and (pos? sim)
                            (not= identity-set other))
                   {:identity other
                    :shared shared
                    :similarity sim}))))
       (sort-by :similarity >)))

;; =============================================================================
;; Algebraic Operations
;; =============================================================================

(defn query-algebra
  "Perform set-algebraic queries over compound identities.
   Supported ops:
     {:intersection [sets]}
     {:union [sets]}
     {:difference [include exclude]}"
  [ops]
  (let [all-ids (set (all-identities))
        results
        (cond
          (:intersection ops)
          (filter #(every? (fn [x] (set/superset? % x)) (:intersection ops)) all-ids)

          (:union ops)
          (filter #(some (fn [x] (set/superset? % x)) (:union ops)) all-ids)

          (:difference ops)
          (let [[include exclude] (:difference ops)]
            (filter #(and (set/superset? % include)
                          (empty? (set/intersection % exclude)))
                    all-ids))

          :else
          (throw (ex-info "Unknown operation" {:ops ops})))]
    (->> results (map #(vec (sort %))) sort vec)))

(defn where
  "Return a map of all registry entries satisfying predicate (pred id value)."
  [pred]
  (->> @registry
       (filter (fn [[k v]] (pred k v)))
       (into {})))

;; =============================================================================
;; Analytical Utilities
;; =============================================================================

(defn aspect-frequency
  "Return frequency map of aspects across all identities, sorted descending."
  []
  (->> (all-identities)
       (mapcat identity)
       (frequencies)
       (sort-by val >)
       (into {})))

(defn identity-stats
  "Return list of {:identity :size :value} sorted by descending size."
  []
  (->> (all-identities)
       (map (fn [id]
              {:identity id
               :size (count id)
               :value (get @registry id)}))
       (sort-by :size >)))

(defn clusters
  "Group identities by primary namespace, returning a map {:ns [identities]}."
  []
  (reduce (fn [acc id-set]
            (reduce (fn [inner aspect]
                      (let [ns-part (namespace aspect)]
                        (update inner (keyword ns-part)
                                (fnil conj #{}) id-set)))
                    acc id-set))
          {}
          (all-identities)))

(defn correlation-matrix
  "Return normalized correlation matrix of aspect co-occurrence (as 0–1 floats)."
  []
  (let [aspects (keys (aspect-frequency))
        all-ids (all-identities)
        total (max 1 (count all-ids))]
    (into {}
          (for [a1 aspects]
            [a1 (into {}
                      (for [a2 aspects]
                        [a2 (/ (count (filter #(and (contains? % a1)
                                                    (contains? % a2))
                                              all-ids))
                               total)]))]))))

;; =============================================================================
;; Heuristics & Diagnostics
;; =============================================================================

(defn missing-aspects
  "Suggest aspects potentially missing from an identity based on correlation."
  [identity-set]
  (let [similar (semantic-neighbors identity-set)
        all-aspects (->> similar
                         (mapcat :identity)
                         (frequencies)
                         (sort-by val >))]
    (->> all-aspects
         (remove (fn [[aspect _]] (contains? identity-set aspect)))
         (take 5)
         (mapv (fn [[aspect freq]]
                 {:aspect aspect
                  :correlation (/ freq (count similar))})))))

(defn find-anomalies
  "Return list of identities that violate expected semantic constraints."
  []
  (let [all-ids (all-identities)]
    (->> all-ids
         (filter (fn [id]
                   (or (and (contains? id :sync/operation)
                            (contains? id :async/operation))
                       (and (contains? id :api/endpoint)
                            (not (contains? id :auth/required)))))))))

;; =============================================================================
;; Visualization
;; =============================================================================

(defn to-graphviz
  "Return a Graphviz DOT string representing compound-identity relationships."
  []
  (let [edges (for [id (all-identities)
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
  "Return a Mermaid diagram (graph TD) for namespace-based clusters."
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

;; =============================================================================
;; Summary
;; =============================================================================

(defn summary
  "Return data summary of the current identity registry:
   {:total :unique-aspects :namespaces :top-aspects :largest :anomalies}"
  []
  (let [ids (all-identities)
        aspects (aspect-frequency)
        clusters (clusters)]
    {:total (count ids)
     :unique-aspects (count aspects)
     :namespaces (keys clusters)
     :top-aspects (take 5 aspects)
     :largest (take 3 (sort-by count > ids))
     :anomalies (find-anomalies)}))
