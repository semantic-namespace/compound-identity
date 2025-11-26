(ns semantic-namespace.tooling.ontology-tools
  "Comprehensive tooling for effortless semantic ontology development"
  (:require [semantic-namespace.compound.identity :as cid]
            [clojure.string :as str]))

;; =============================================================================
;; DISCOVERY TOOLS - Understanding what exists
;; =============================================================================

(defn aspect-catalog
  "Show all aspects with usage stats and examples"
  []
  (let [freq (cid/aspect-frequency)]
    (->> freq
         (map (fn [[aspect count]]
                (let [examples (take 2 (filter #(contains? % aspect)
                                               (cid/all-identities)))]
                  {:aspect aspect
                   :usage-count count
                   :examples (map #(:dev/id (cid/fetch %)) examples)})))
         (group-by #(namespace (:aspect %)))
         (sort-by key))))

(defn suggest-aspects
  "Given partial identity, suggest aspects based on similar entries"
  [partial-identity]
  (let [similar (cid/semantic-neighbors partial-identity)
        all-aspects (->> similar
                         (take 5)
                         (mapcat :identity)
                         frequencies
                         (remove (fn [[asp _]] (contains? partial-identity asp)))
                         (sort-by val >)
                         (take 10))]
    {:similar-entries (take 3 (map :identity similar))
     :suggested-aspects (map first all-aspects)
     :rationale "Based on 5 most similar entries"}))

(defn impact-analysis
  "Analyze what would be affected by changes to an aspect"
  [aspect]
  (let [direct-users (cid/find-with aspect)
        by-tier (group-by (fn [[id _]]
                            (cond
                              (contains? id :tier/foundation) :foundation
                              (contains? id :tier/service) :service
                              (contains? id :tier/feature) :feature
                              (contains? id :tier/api) :api
                              :else :other))
                          direct-users)
        by-type (group-by (fn [[id _]]
                            (cond
                              (contains? id :semantic-namespace/component) :component
                              (contains? id :semantic-namespace/function) :function
                              (contains? id :semantic-namespace/endpoint) :endpoint
                              (contains? id :semantic-namespace/error-handler) :error-handler
                              (contains? id :semantic-namespace/architectural-decision) :decision
                              :else :other))
                          direct-users)]
    {:aspect aspect
     :total-impact (count direct-users)
     :by-tier (into {} (map (fn [[k v]] [k (count v)]) by-tier))
     :by-type (into {} (map (fn [[k v]] [k (count v)]) by-type))
     :examples (take 3 (map (fn [[_ v]] (:dev/id v)) direct-users))}))

;; =============================================================================
;; VALIDATION TOOLS - Preventing errors before registration
;; =============================================================================

(defn validate-before-register
  "Check if identity is valid before registering"
  [new-identity]
  (let [issues []
        symmetric-diff (fn [a b]
                         (clojure.set/union
                          (clojure.set/difference a b)
                          (clojure.set/difference b a)))]
    (-> issues
        (into (when (contains? @cid/registry new-identity)
                [{:level :error
                  :type :collision
                  :message "Identity already exists"
                  :existing-dev-id (:dev/id (cid/fetch new-identity))}]))
        (into (keep (fn [existing-id]
                      (let [diff (symmetric-diff new-identity existing-id)]
                        (when (= 2 (count diff))
                          {:level :warning
                           :type :near-collision
                           :message "Differs by only one aspect from existing"
                           :existing-dev-id (:dev/id (cid/fetch existing-id))
                           :difference diff})))
                    (take 100 (cid/all-identities))))
        (into (when (and (contains? new-identity :integration/external)
                         (not (contains? new-identity :temporal/async)))
                [{:level :error
                  :type :axiom-violation
                  :message "External integrations must be async"}]))
        (into (when (and (contains? new-identity :compliance/pii)
                         (not (contains? new-identity :compliance/audited)))
                [{:level :warning
                  :type :compliance-gap
                  :message "PII handling should be audited"}])))))

;; =============================================================================
;; QUERY TOOLS - Fluent querying
;; =============================================================================

(defn where->
  "Fluent query builder"
  [initial-aspects]
  {:aspects (if (set? initial-aspects) initial-aspects #{initial-aspects})
   :ops []})

(defn with-aspect [query aspect]
  (update query :aspects conj aspect))

(defn without-aspect [query aspect]
  (update query :ops conj [:exclude aspect]))

(defn in-tier [query tier]
  (with-aspect query tier))

(defn execute-query [query]
  (let [{:keys [aspects ops]} query
        base-results (cid/find-with aspects)
        filtered (reduce (fn [results [op arg]]
                           (case op
                             :exclude (into {} (remove (fn [[id _]] (contains? id arg)) results))
                             results))
                         base-results
                         ops)]
    (keys filtered)))

;; =============================================================================
;; TEMPLATE TOOLS - Reducing boilerplate
;; =============================================================================

(defn template:foundation-component
  "Template for foundation-tier components"
  [domain & {:keys [external? async? traced?]
             :or {external? false async? false traced? false}}]
  (cond-> #{:semantic-namespace/component
            :semantic/docs
            :tier/foundation
            :effect/read}
    domain (conj domain)
    external? (conj :integration/external)
    (not external?) (conj :integration/internal)
    async? (conj :temporal/async :temporal/timeout-configured)
    traced? (conj :observability/traced)
    (not traced?) (conj :observability/metered)))

(defn template:service-function
  "Template for service-tier business logic"
  [domain operation & {:keys [external? pure? pii?]
                       :or {external? false pure? false pii? false}}]
  (cond-> #{:semantic-namespace/function
            :semantic/docs
            :tier/service
            :effect/read}
    domain (conj domain)
    operation (conj operation)
    external? (conj :integration/external :temporal/async :temporal/timeout-configured :observability/traced)
    (not external?) (conj :observability/metered)
    pure? (conj :effect/pure)
    pii? (conj :compliance/pii :compliance/audited)))

(defn template:api-endpoint
  "Template for API endpoints"
  [domain operation & {:keys [auth-required? rate-limited? pii?]
                       :or {auth-required? true rate-limited? false pii? false}}]
  (cond-> #{:semantic-namespace/endpoint
            :semantic-namespace/function
            :semantic/docs
            :tier/api
            :effect/read
            :protocol/http
            :observability/logged
            :observability/metered}
    domain (conj domain)
    operation (conj operation)
    auth-required? (conj :authorization/required)
    rate-limited? (conj :capacity/rate-limited)
    pii? (conj :compliance/pii :compliance/audited)))

;; =============================================================================
;; REFACTORING TOOLS - Safe evolution
;; =============================================================================

(defn refactor-aspect
  "Safely rename/replace an aspect across the registry"
  [old-aspect new-aspect & {:keys [dry-run?] :or {dry-run? true}}]
  (let [affected (filter #(contains? % old-aspect) (cid/all-identities))
        migrations (map (fn [old-id]
                          (let [new-id (-> old-id (disj old-aspect) (conj new-aspect))
                                value (cid/fetch old-id)]
                            {:old-id old-id
                             :new-id new-id
                             :dev-id (:dev/id value)
                             :will-collide? (contains? @cid/registry new-id)}))
                        affected)]
    (if dry-run?
      {:dry-run true
       :affected-count (count migrations)
       :migrations migrations
       :conflicts (filter :will-collide? migrations)}
      (do
        (doseq [{:keys [old-id new-id]} migrations
                :when (not (:will-collide? (first migrations)))]
          (let [value (cid/fetch old-id)]
            (cid/remove* old-id)
            (swap! cid/registry assoc new-id value)))
        {:migrated (count migrations)}))))

;; =============================================================================
;; DOCUMENTATION TOOLS - Auto-generation
;; =============================================================================

(defn generate-ontology-docs
  "Generate markdown documentation of the ontology"
  []
  (let [catalog (aspect-catalog)
        by-category (group-by (fn [[ns _]]
                                (cond
                                  (= ns "tier") "Architecture Layers"
                                  (= ns "domain") "Business Domains"
                                  (= ns "operation") "Operations"
                                  (= ns "compliance") "Compliance"
                                  (= ns "temporal") "Temporal Concerns"
                                  (= ns "observability") "Observability"
                                  (= ns "integration") "Integration"
                                  (= ns "protocol") "Protocols"
                                  :else "Other"))
                              catalog)]
    (apply str
           "# Semantic Ontology Documentation\n\n"
           "Generated from live registry.\n\n"
           (for [[category aspects] (sort-by key by-category)]
             (str "## " category "\n\n"
                  (apply str
                         (for [[_ aspect-list] aspects
                               {:keys [aspect usage-count examples]} aspect-list]
                           (str "### " aspect "\n"
                                "- **Usage**: " usage-count " entries\n"
                                "- **Examples**: " (str/join ", " examples) "\n\n"))))))))

;; =============================================================================
;; REPL TOOLS - Interactive exploration
;; =============================================================================

(defn help-ontology
  "Interactive help system"
  []
  (println "=== Semantic Ontology Tooling ===\n")
  (println "Discovery:")
  (println "  (aspect-catalog)              - Browse all aspects with usage")
  (println "  (suggest-aspects #{...})      - Get aspect suggestions")
  (println "  (impact-analysis :aspect/x)   - See impact of changes")
  (println)
  (println "Validation:")
  (println "  (validate-before-register #{...}) - Check before registering")
  (println)
  (println "Querying:")
  (println "  (where-> :domain/x)           - Fluent query builder")
  (println "  (-> (where-> :domain/x) (with-aspect :tier/service) execute-query)")
  (println)
  (println "Templates:")
  (println "  (template:foundation-component :domain/x :external? true)")
  (println "  (template:service-function :domain/x :operation/y :pii? true)")
  (println "  (template:api-endpoint :domain/x :operation/y :rate-limited? true)")
  (println)
  (println "Refactoring:")
  (println "  (refactor-aspect :old :new :dry-run? true)")
  (println)
  (println "Documentation:")
  (println "  (generate-ontology-docs)      - Generate markdown docs")
  nil)

(defn inspect
  "Quick inspection of a dev-id"
  [dev-id]
  (let [matches (filter #(= dev-id (:dev/id (cid/fetch %))) (cid/all-identities))]
    (if (seq matches)
      (let [id (first matches)
            val (cid/fetch id)]
        {:dev-id dev-id
         :semantic-identity id
         :aspect-count (count id)
         :value val
         :docs (:docs/content val)})
      {:error "Not found" :dev-id dev-id})))
