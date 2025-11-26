(ns semantic-namespace.tooling.semantic-queries
  "Semantic query patterns for compound identity registries.
   
   Query categories:
   - Basic: find-by, find-intersection
   - Data Flow: produces, consumes, trace-data-flow
   - Dependencies: dependency-graph, impact-of-change
   - Architecture: by-tier, domain-coupling
   - Compliance: pii-surface, error-handler-coverage
   - Discovery: semantic-similarity, aspect-coverage
   - Decisions: decisions-by-category"
  (:require [clojure.set :as set]
            [semantic-namespace.compound.identity :as cid]))

(defn find-by
  "Find all entities containing aspect(s).
   aspect can be keyword or set of keywords."
  [registry aspect]
  (let [aspect-set (if (set? aspect) aspect #{aspect})]
    (->> registry
         (filter (fn [[id _]] (set/superset? id aspect-set)))
         (into {}))))

(defn find-intersection
  "Find entities containing ALL given aspects."
  [registry & aspects]
  (find-by registry (set aspects)))

(defn produces
  "Find entities that produce a data key in their response."
  [registry response-key id-key]
  (->> registry
       (filter (fn [[_ v]]
                 (some #{response-key} (get v :context/response))))
       (map (fn [[id v]] {:id (get v id-key) :identity id}))))

(defn consumes
  "Find entities that consume a data key in their context."
  [registry context-key id-key]
  (->> registry
       (filter (fn [[_ v]]
                 (some #{context-key} (get v :context/input))))
       (map (fn [[id v]] {:id (get v id-key) :identity id}))))

(defn trace-data-flow
  "Trace how a data key flows through the system."
  [registry data-key id-key]
  (let [producers (produces registry data-key id-key)
        consumers (consumes registry data-key id-key)]
    {:data-key data-key
     :produced-by (mapv :id producers)
     :consumed-by (mapv :id consumers)
     :connected? (and (seq producers) (seq consumers))}))

(defn dependency-graph
  "Build dependency graph from entities with :deps key."
  [registry id-key deps-key]
  (->> registry
       (filter (fn [[_ v]] (get v deps-key)))
       (map (fn [[id v]]
              {:id (get v id-key)
               :deps (get v deps-key)
               :identity id}))
       (remove #(empty? (:deps %)))))

(defn by-tier
  "Group entities by architectural tier."
  [registry id-key]
  (let [tiers [:tier/foundation :tier/service :tier/api]]
    (into {}
          (for [tier tiers]
            [tier (->> (find-by registry tier)
                       vals
                       (map #(get % id-key))
                       (remove nil?)
                       vec)]))))

(defn pii-surface
  "All entities handling PII with audit status."
  [registry id-key context-key response-key]
  (->> (find-by registry :compliance/pii)
       (map (fn [[id v]]
              {:id (get v id-key)
               :audited? (contains? id :compliance/audited)
               :context (get v context-key)
               :response (get v response-key)}))))

(defn aspect-coverage
  "Show which entities have specific cross-cutting concerns."
  [registry entity-type id-key aspects]
  (for [[id v] (find-by registry entity-type)]
    {:id (get v id-key)
     :has (into {} (for [a aspects]
                     [a (contains? id a)]))}))

(defn domain-coupling
  "Analyze inter-domain dependencies."
  [registry id-key deps-key]
  (let [domains (->> registry
                     keys
                     (mapcat identity)
                     (filter #(= "domain" (namespace %)))
                     set)]
    (for [domain domains
          :let [domain-entities (find-by registry domain)
                deps (->> domain-entities vals (mapcat #(get % deps-key)) set)
                dep-domains (->> registry
                                 (filter (fn [[_ v]] (some deps #{(get v id-key)})))
                                 (mapcat (fn [[id _]]
                                           (filter #(= "domain" (namespace %)) id)))
                                 set)]]
      {:domain domain
       :depends-on (disj dep-domains domain)
       :entity-count (count domain-entities)})))

(defn decisions-by-category
  "Group architectural decisions by category."
  [registry id-key]
  (->> (find-by registry :semantic-namespace/architectural-decision)
       (map (fn [[id v]]
              {:id (get v id-key)
               :category (first (filter #(= "decision-category" (namespace %)) id))
               :question (:decision/question v)
               :chosen (:decision/chosen v)
               :priority (:decision/priority v)}))
       (group-by :category)))

(defn impact-of-change
  "What would be affected if entity changes?"
  [registry entity-id id-key deps-key response-key]
  (let [entity-entry (->> registry
                          (filter (fn [[_ v]] (= entity-id (get v id-key))))
                          first)
        entity-response (get (second entity-entry) response-key)
        direct-deps (->> registry
                         (filter (fn [[_ v]]
                                   (some #{entity-id} (get v deps-key))))
                         (map (fn [[_ v]] (get v id-key))))]
    {:entity entity-id
     :produces entity-response
     :direct-dependents (vec direct-deps)}))

(defn semantic-similarity
  "Find entities with similar semantic profiles."
  [registry entity-id id-key]
  (let [target-entry (->> registry
                          (filter (fn [[_ v]] (= entity-id (get v id-key))))
                          first)
        target-id (first target-entry)]
    (when target-id
      (->> registry
           (remove (fn [[id _]] (= id target-id)))
           (map (fn [[id v]]
                  (let [shared (set/intersection target-id id)
                        union-size (count (set/union target-id id))]
                    {:id (get v id-key)
                     :similarity (if (pos? union-size)
                                   (double (/ (count shared) union-size))
                                   0.0)
                     :shared-aspects shared})))
           (sort-by :similarity >)
           (take 5)))))

(defn error-handler-coverage
  "Check if error handlers exist for marked concerns."
  [registry id-key]
  (let [handlers (find-by registry :semantic-namespace/error-handler)
        needs-handling #{:protocol/http :temporal/timeout
                         :authorization/required :capacity/rate-limited}]
    {:handlers (map (fn [[id v]]
                      {:id (get v id-key)
                       :handles (set/intersection id needs-handling)})
                    handlers)
     :coverage (for [concern needs-handling
                     [id v] (find-by registry concern)]
                 {:entity (get v id-key)
                  :concern concern
                  :has-handler? (some #(contains? % concern) (keys handlers))})}))
