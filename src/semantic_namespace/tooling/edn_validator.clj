(ns semantic-namespace.tooling.edn-validator
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [semantic-namespace.compound.identity :as cid]))

(defn load-ontology [path]
  (edn/read-string (slurp path)))

(defn load-system [path]
  (edn/read-string (slurp path)))

(defn validate-entity-type
  "Check entity has required aspects for its type"
  [ontology [entity-id {:keys [identity]}]]
  (let [id-set (set identity)
        entity-types (:entity-types ontology)]
    (for [[type-kw {:keys [required-props implies]}] entity-types
          :when (contains? id-set type-kw)
          :let [violations (cond-> []
                             implies
                             (into (for [implied implies
                                         :when (not (contains? id-set implied))]
                                     {:entity entity-id
                                      :type type-kw
                                      :violation :missing-implied
                                      :expected implied})))]
          :when (seq violations)]
      violations)))

(defn validate-axiom
  "Check a single axiom against registry"
  [axiom registry]
  (let [{:keys [id applies-to rule]} axiom]
    (for [[reg-id v] registry
          :when (contains? reg-id applies-to)
          :let [violation (case id
                            :external-must-be-async
                            (when-not (contains? reg-id :temporal/async)
                              {:axiom id :entity reg-id :rule rule})

                            :async-needs-timeout
                            (when-not (contains? reg-id :temporal/timeout-configured)
                              {:axiom id :entity reg-id :rule rule})

                            :pii-needs-audit
                            (when-not (contains? reg-id :compliance/audited)
                              {:axiom id :entity reg-id :rule rule})

                            :endpoint-is-function
                            (when-not (contains? reg-id :semantic-namespace/function)
                              {:axiom id :entity reg-id :rule rule})

                            nil)]
          :when violation]
      violation)))

(defn validate-registry-against-ontology
  "Validate current registry against ontology axioms"
  [ontology-path]
  (let [ontology (load-ontology ontology-path)
        axioms (:axioms ontology)]
    (mapcat #(validate-axiom % @cid/registry) axioms)))

(defn compare-registry-to-system
  "Compare loaded registry to expected system EDN"
  [system-path id-key]
  (let [system (load-system system-path)
        expected-entities (:entities system)
        actual-ids (->> @cid/registry
                        (map (fn [[id v]] [(get v id-key) id]))
                        (into {}))]
    {:missing-in-registry (set/difference (set (keys expected-entities))
                                          (set (keys actual-ids)))
     :extra-in-registry (set/difference (set (keys actual-ids))
                                        (set (keys expected-entities)))
     :identity-mismatches
     (for [[entity-id expected] expected-entities
           :let [actual-identity (get actual-ids entity-id)]
           :when actual-identity
           :let [expected-set (set (:identity expected))]
           :when (not= expected-set actual-identity)]
       {:entity entity-id
        :expected expected-set
        :actual actual-identity
        :missing (set/difference expected-set actual-identity)
        :extra (set/difference actual-identity expected-set)})}))

(defn validate-all
  "Run all validations"
  [ontology-path system-path id-key]
  (let [axiom-violations (validate-registry-against-ontology ontology-path)
        comparison (compare-registry-to-system system-path id-key)]
    {:axiom-violations (vec axiom-violations)
     :comparison comparison
     :valid? (and (empty? axiom-violations)
                  (empty? (:missing-in-registry comparison))
                  (empty? (:identity-mismatches comparison)))}))
