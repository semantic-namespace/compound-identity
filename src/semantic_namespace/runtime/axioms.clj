(ns semantic-namespace.runtime.axioms
  "Axioms that validate semantic consistency and catch real mistakes."
  (:require [semantic-namespace.compound.identity :as cid]
            [semantic-namespace.app.calendar-availability :as cal]
            [semantic-namespace.runtime.core :as rt]
            [clojure.set :as set]))

(defn all-dev-ids []
  (map #(:dev/id (second %)) @cid/registry))

(defn all-context-keys []
  (->> @cid/registry vals (mapcat ::cal/context) (remove nil?) set))

(defn all-response-keys []
  (->> @cid/registry vals (mapcat ::cal/response) (remove nil?) set))

(defn endpoint-context-keys []
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id :semantic-namespace/endpoint)))
       (mapcat (fn [[_ v]] (::cal/context v)))
       set))

;; =============================================================================
;; DATA FLOW AXIOMS
;; =============================================================================

(defn axiom-context-satisfiable
  "Every context key must be either an endpoint input or produced by some function."
  []
  (let [endpoint-inputs (endpoint-context-keys)
        produced (all-response-keys)
        available (set/union endpoint-inputs produced)
        all-needed (all-context-keys)
        unsatisfied (set/difference all-needed available)]
    (when (seq unsatisfied)
      {:axiom :context-satisfiable
       :violation :unsatisfied-context
       :missing unsatisfied
       :message (str "These context keys are needed but never produced: " unsatisfied)})))

(defn axiom-no-orphan-responses
  "Every response key should be consumed by something (warning, not error)."
  []
  (let [produced (all-response-keys)
        consumed (all-context-keys)
        orphans (set/difference produced consumed)]
    (when (seq orphans)
      {:axiom :no-orphan-responses
       :violation :orphan-outputs
       :orphans orphans
       :severity :warning
       :message (str "These response keys are produced but never consumed: " orphans)})))

(defn axiom-no-circular-data-flow
  "Data flow graph must be acyclic."
  []
  (let [fns (rt/all-with-aspect :semantic-namespace/function)
        deps-map (into {} (map (fn [id] [id (rt/compute-data-deps id)]) fns))]
    (letfn [(has-cycle? [id visited path]
              (cond
                (contains? path id) {:cycle (conj (vec path) id)}
                (contains? visited id) nil
                :else (some #(has-cycle? % (conj visited id) (conj path id))
                            (get deps-map id #{}))))]
      (some #(has-cycle? % #{} #{}) fns))))

;; =============================================================================
;; DEPENDENCY AXIOMS
;; =============================================================================

(defn axiom-deps-exist
  "All ::deps must reference existing dev-ids."
  []
  (let [all-ids (set (all-dev-ids))
        violations (for [[_ v] @cid/registry
                         :let [dev-id (:dev/id v)
                               deps (::cal/deps v)]
                         :when deps
                         :let [missing (set/difference deps all-ids)]
                         :when (seq missing)]
                     {:dev-id dev-id :missing-deps missing})]
    (when (seq violations)
      {:axiom :deps-exist
       :violation :missing-dependencies
       :details violations
       :message "Some dependencies reference non-existent entities"})))

(defn axiom-no-circular-deps
  "Component/function dependency graph must be acyclic."
  []
  (let [all-ids (all-dev-ids)
        deps-map (into {} (map (fn [id] [id (or (rt/deps-for id) #{})]) all-ids))]
    (letfn [(has-cycle? [id visited path]
              (cond
                (contains? path id) {:cycle (conj (vec path) id)}
                (contains? visited id) nil
                :else (some #(has-cycle? % (conj visited id) (conj path id))
                            (get deps-map id #{}))))]
      (some #(has-cycle? % #{} #{}) all-ids))))

;; =============================================================================
;; SEMANTIC CONSISTENCY AXIOMS  
;; =============================================================================

(defn axiom-external-fn-needs-external-dep
  "Functions marked :integration/external should depend on an :integration/external component."
  []
  (let [external-fns (rt/all-with-aspect :integration/external)
        external-components (->> (rt/all-with-aspect :semantic-namespace/component)
                                 (filter #(rt/has-aspect? % :integration/external))
                                 set)
        violations (for [fn-id external-fns
                         :when (rt/has-aspect? fn-id :semantic-namespace/function)
                         :let [deps (rt/deps-for fn-id)
                               has-external-dep? (seq (set/intersection (set deps) external-components))]
                         :when (not has-external-dep?)]
                     fn-id)]
    (when (seq violations)
      {:axiom :external-fn-needs-external-dep
       :violation :missing-external-dependency
       :functions violations
       :message "External functions should depend on external components"})))

(defn axiom-pure-has-no-deps
  "Functions marked :effect/pure should have no component dependencies."
  []
  (let [pure-fns (filter #(rt/has-aspect? % :effect/pure) (rt/all-with-aspect :semantic-namespace/function))
        violations (for [fn-id pure-fns
                         :let [deps (rt/deps-for fn-id)
                               component-deps (filter #(rt/has-aspect? % :semantic-namespace/component) deps)]
                         :when (seq component-deps)]
                     {:fn fn-id :component-deps component-deps})]
    (when (seq violations)
      {:axiom :pure-has-no-deps
       :violation :pure-fn-with-side-effects
       :details violations
       :message "Pure functions should not depend on components (which have side effects)"})))

(defn axiom-oauth-fn-has-oauth-dep
  "Functions using :protocol/oauth should have oauth component in dependency chain."
  []
  (let [oauth-fns (filter #(rt/has-aspect? % :semantic-namespace/function)
                          (rt/all-with-aspect :protocol/oauth))
        oauth-components (->> (rt/all-with-aspect :semantic-namespace/component)
                              (filter #(rt/has-aspect? % :protocol/oauth))
                              set)
        violations (for [fn-id oauth-fns
                         :let [deps (rt/deps-for fn-id)
                               has-oauth? (seq (set/intersection (set deps) oauth-components))]
                         :when (not has-oauth?)]
                     fn-id)]
    (when (seq violations)
      {:axiom :oauth-fn-has-oauth-dep
       :violation :missing-oauth-dependency
       :functions violations
       :message "OAuth functions should depend on OAuth component"})))

;; =============================================================================
;; TIER AXIOMS
;; =============================================================================

(defn axiom-components-are-foundation
  "Components should be :tier/foundation."
  []
  (let [components (rt/all-with-aspect :semantic-namespace/component)
        violations (remove #(rt/has-aspect? % :tier/foundation) components)]
    (when (seq violations)
      {:axiom :components-are-foundation
       :violation :wrong-tier
       :components violations
       :message "Components should be :tier/foundation"})))

(defn axiom-endpoints-are-api-tier
  "Endpoints should be :tier/api."
  []
  (let [endpoints (rt/all-with-aspect :semantic-namespace/endpoint)
        violations (remove #(rt/has-aspect? % :tier/api) endpoints)]
    (when (seq violations)
      {:axiom :endpoints-are-api-tier
       :violation :wrong-tier
       :endpoints violations
       :message "Endpoints should be :tier/api"})))

;; =============================================================================
;; COMPLETENESS AXIOMS
;; =============================================================================

(defn axiom-all-fns-reachable
  "Every function should be reachable from some endpoint."
  []
  (let [endpoints (rt/all-with-aspect :semantic-namespace/endpoint)
        all-fns (set (rt/all-with-aspect :semantic-namespace/function))
        reachable (atom #{})
        collect-reachable (fn collect [id]
                            (when-not (@reachable id)
                              (swap! reachable conj id)
                              (doseq [dep (rt/deps-for id)]
                                (collect dep))))]
    (doseq [ep endpoints]
      (collect-reachable ep))
    (let [unreachable (set/difference all-fns @reachable)]
      (when (seq unreachable)
        {:axiom :all-fns-reachable
         :violation :unreachable-functions
         :functions unreachable
         :message "Some functions are not reachable from any endpoint"}))))

;; =============================================================================
;; RUN ALL AXIOMS
;; =============================================================================

(def all-axioms
  [{:name :context-satisfiable :fn axiom-context-satisfiable :severity :error}
   {:name :no-orphan-responses :fn axiom-no-orphan-responses :severity :warning}
   {:name :no-circular-data-flow :fn axiom-no-circular-data-flow :severity :error}
   {:name :deps-exist :fn axiom-deps-exist :severity :error}
   {:name :no-circular-deps :fn axiom-no-circular-deps :severity :error}
   {:name :external-fn-needs-dep :fn axiom-external-fn-needs-external-dep :severity :error}
   {:name :pure-has-no-deps :fn axiom-pure-has-no-deps :severity :error}
   {:name :oauth-fn-has-oauth-dep :fn axiom-oauth-fn-has-oauth-dep :severity :error}
   {:name :components-are-foundation :fn axiom-components-are-foundation :severity :warning}
   {:name :endpoints-are-api-tier :fn axiom-endpoints-are-api-tier :severity :warning}
   {:name :all-fns-reachable :fn axiom-all-fns-reachable :severity :warning}])

(defn check-all []
  (let [results (for [{:keys [name fn severity]} all-axioms
                      :let [result (fn)]
                      :when result]
                  (assoc result :severity severity))]
    {:violations (vec results)
     :errors (filterv #(= :error (:severity %)) results)
     :warnings (filterv #(= :warning (:severity %)) results)
     :valid? (empty? (filter #(= :error (:severity %)) results))}))

(defn report []
  (let [{:keys [violations errors warnings valid?]} (check-all)]
    (println "\n=== AXIOM VALIDATION REPORT ===\n")
    (if valid?
      (println "âœ“ All error-level axioms pass")
      (do
        (println "âœ— ERRORS:")
        (doseq [e errors]
          (println "  -" (:axiom e) "-" (:message e)))))
    (when (seq warnings)
      (println "\nâš  WARNINGS:")
      (doseq [w warnings]
        (println "  -" (:axiom w) "-" (:message w))))
    (println "\nTotal:" (count violations) "issues (" (count errors) "errors," (count warnings) "warnings)")
    valid?))
