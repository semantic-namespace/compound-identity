(ns semantic-namespace.runtime.ide
  "IDE integration API - clean interface for editor tooling.
   Returns EDN that editors can parse and display."
  (:require [semantic-namespace.compound.identity :as cid]
            [semantic-namespace.app.calendar-availability :as cal]
            [semantic-namespace.runtime.core :as rt]
            [semantic-namespace.runtime.axioms :as ax]
            [semantic-namespace.runtime.docs :as docs]
            [clojure.string :as str]))

;; =============================================================================
;; QUERY API
;; =============================================================================

(defn list-all-entities []
  "List all registered entities with their types."
  (vec (sort-by str
                (for [[id props] @cid/registry]
                  {:dev-id (:dev/id props)
                   :type (cond
                           (contains? id :semantic-namespace/endpoint) :endpoint
                           (contains? id :semantic-namespace/function) :function
                           (contains? id :semantic-namespace/component) :component
                           (contains? id :semantic-namespace/schema) :schema
                           :else :other)}))))

(defn list-aspects []
  "List all unique aspects used in the registry."
  (vec (sort
        (disj (->> @cid/registry keys (mapcat identity) set)
              :semantic-namespace/function
              :semantic-namespace/component
              :semantic-namespace/endpoint
              :semantic-namespace/schema
              :semantic-namespace/data-schema
              :semantic-namespace.contract/instance))))

(defn entities-with-aspect [aspect]
  "Find all entities with given aspect."
  (let [aspect-kw (if (string? aspect) (keyword aspect) aspect)]
    (vec (sort (rt/all-with-aspect aspect-kw)))))

(defn entity-info [dev-id]
  "Get full info about an entity."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)
        id (rt/identity-for dev-id-kw)
        props (rt/props-for dev-id-kw)]
    (when id
      {:dev-id dev-id-kw
       :identity (vec (sort id))
       :aspects (vec (sort (disj id :semantic-namespace/function
                                 :semantic-namespace/component
                                 :semantic-namespace/endpoint
                                 :semantic-namespace/schema)))
       :context (::cal/context props)
       :response (::cal/response props)
       :deps (vec (sort (::cal/deps props)))
       :fields (::cal/fields props)})))

(defn data-flow [dev-id]
  "Get data flow info for a function."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)]
    (rt/trace-data-flow dev-id-kw)))

(defn execution-order []
  "Get topologically sorted execution order."
  (rt/topo-sort-by-data (rt/all-with-aspect :semantic-namespace/function)))

;; =============================================================================
;; VALIDATION API
;; =============================================================================

(defn check-axioms []
  "Run all axiom checks and return results."
  (let [{:keys [valid? errors warnings]} (ax/check-all)]
    {:valid? valid?
     :error-count (count errors)
     :warning-count (count warnings)
     :errors (mapv #(select-keys % [:axiom :message]) errors)
     :warnings (mapv #(select-keys % [:axiom :message]) warnings)}))

(defn validate-entity [dev-id]
  "Check axioms relevant to a specific entity."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)
        {:keys [errors warnings]} (ax/check-all)
        relevant (fn [v] (str/includes? (str (:message v) (:details v)) (str dev-id-kw)))]
    {:dev-id dev-id-kw
     :errors (filterv relevant errors)
     :warnings (filterv relevant warnings)}))

;; =============================================================================
;; DOCUMENTATION API
;; =============================================================================

(defn entity-doc [dev-id]
  "Get documentation for an entity."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)]
    (docs/enrich-with-descriptions dev-id-kw)))

(defn system-summary []
  "Get system overview."
  (docs/system-overview))

(defn generate-markdown []
  "Generate full markdown documentation."
  (docs/generate-markdown))

;; =============================================================================
;; NAVIGATION API
;; =============================================================================

(defn dependents-of [dev-id]
  "Find what depends on this entity."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)]
    (vec (sort
          (for [other (ax/all-dev-ids)
                :let [deps (rt/deps-for other)]
                :when (and deps (contains? deps dev-id-kw))]
            other)))))

(defn dependencies-of [dev-id]
  "Find what this entity depends on."
  (let [dev-id-kw (if (string? dev-id) (keyword dev-id) dev-id)]
    (vec (sort (rt/deps-for dev-id-kw)))))

(defn producers-of [data-key]
  "Find functions that produce a data key."
  (let [data-key-kw (if (string? data-key) (keyword data-key) data-key)]
    (vec (sort
          (for [[_ props] @cid/registry
                :when (some #{data-key-kw} (::cal/response props))]
            (:dev/id props))))))

(defn consumers-of [data-key]
  "Find functions that consume a data key."
  (let [data-key-kw (if (string? data-key) (keyword data-key) data-key)]
    (vec (sort
          (for [[_ props] @cid/registry
                :when (some #{data-key-kw} (::cal/context props))]
            (:dev/id props))))))

;; =============================================================================
;; COMPLETION API
;; =============================================================================

(defn complete-dev-id [prefix]
  "Autocomplete dev-id from prefix."
  (let [all-ids (map str (ax/all-dev-ids))]
    (vec (filter #(str/starts-with? % prefix) all-ids))))

(defn complete-aspect [prefix]
  "Autocomplete aspect from prefix."
  (let [all-aspects (map str (list-aspects))]
    (vec (filter #(str/starts-with? % prefix) all-aspects))))

(defn complete-data-key [prefix]
  "Autocomplete data key from prefix."
  (let [all-keys (->> @cid/registry
                      vals
                      (mapcat #(concat (::cal/context %) (::cal/response %)))
                      (remove nil?)
                      (map str)
                      distinct
                      sort)]
    (vec (filter #(str/starts-with? % prefix) all-keys))))
