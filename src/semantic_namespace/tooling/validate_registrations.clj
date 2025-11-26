(ns semantic-namespace.tooling.validate-registrations
  "Validate semantic identity registrations in source files.
  
  Run after code is loaded to check for violations.
  Better than clj-kondo hooks because it has access to the actual registry."
  (:require [semantic-namespace.compound.identity :as cid]
            [clojure.string :as str]))

(defn validate-identity
  "Validate a single identity against rules"
  [identity-set value]
  (let [issues []]
    (-> issues
        ;; Check minimum aspects
        (into (when (< (count identity-set) 2)
                [{:level :error
                  :type :min-aspects
                  :message "Semantic identity must have at least 2 aspects"
                  :identity identity-set}]))

        ;; Check external without async
        (into (when (and (contains? identity-set :integration/external)
                         (not (contains? identity-set :temporal/async)))
                [{:level :warning
                  :type :axiom-violation
                  :message "External integrations must be async (add :temporal/async)"
                  :identity identity-set
                  :dev-id (:dev/id value)}]))

        ;; Check PII without audit
        (into (when (and (contains? identity-set :compliance/pii)
                         (not (contains? identity-set :compliance/audited)))
                [{:level :warning
                  :type :compliance-gap
                  :message "PII handling should be audited (add :compliance/audited)"
                  :identity identity-set
                  :dev-id (:dev/id value)}])))))

(defn find-near-collisions
  "Find identities that differ by only 1 aspect"
  []
  (let [all-identities (cid/all-identities)]
    (for [id1 all-identities
          id2 all-identities
          :when (not= id1 id2)
          :let [diff-add (clojure.set/difference id1 id2)
                diff-rem (clojure.set/difference id2 id1)]
          :when (and (= 1 (count diff-add))
                     (= 1 (count diff-rem)))]
      {:level :warning
       :type :near-collision
       :message (format "Near-collision: %s differs by only 1 aspect from %s (adds %s, removes %s)"
                        (:dev/id (cid/fetch id1))
                        (:dev/id (cid/fetch id2))
                        (first diff-add)
                        (first diff-rem))
       :identity1 id1
       :identity2 id2})))

(defn find-collisions
  "Find duplicate identities with different dev-ids"
  []
  ;; This can't happen with current register! implementation
  ;; but useful for future extensibility
  [])

(defn validate-all
  "Validate all registrations in the current registry"
  []
  (let [all-entries (cid/all-identities)
        identity-issues (mapcat (fn [identity-set]
                                  (let [value (cid/fetch identity-set)]
                                    (validate-identity identity-set value)))
                                all-entries)
        near-collisions (find-near-collisions)
        collisions (find-collisions)
        all-issues (concat identity-issues near-collisions collisions)
        errors (filter #(= :error (:level %)) all-issues)
        warnings (filter #(= :warning (:level %)) all-issues)]

    {:total-identities (count all-entries)
     :issues all-issues
     :error-count (count errors)
     :warning-count (count warnings)
     :errors errors
     :warnings warnings}))

(defn print-report
  "Print validation report to console"
  []
  (let [{:keys [total-identities errors warnings error-count warning-count]} (validate-all)]
    (println "\n=== Semantic Identity Validation Report ===")
    (println (format "Total identities: %d" total-identities))
    (println)

    (when (seq errors)
      (println "ERRORS:")
      (doseq [error errors]
        (println (format "  â�Œ [%s] %s"
                         (or (:dev-id error) (:type error))
                         (:message error)))
        (when (:identity error)
          (println (format "     Identity: %s" (str/join " " (sort (:identity error))))))
        (println))
      (println))

    (when (seq warnings)
      (println "WARNINGS:")
      (doseq [warning warnings]
        (println (format "  âš ï¸�  [%s] %s"
                         (or (:dev-id warning) (:type warning))
                         (:message warning)))
        (when (:identity warning)
          (println (format "     Identity: %s" (str/join " " (sort (:identity warning))))))
        (println)))

    (println (format "Summary: %d errors, %d warnings" error-count warning-count))
    (println)

    ;; Return exit code for CI
    (if (pos? error-count) 1 0)))

(defn validate!
  "Validate and throw if errors found (for CI)"
  []
  (let [{:keys [error-count errors]} (validate-all)]
    (when (pos? error-count)
      (print-report)
      (throw (ex-info "Semantic validation failed"
                      {:errors errors
                       :error-count error-count})))))

(comment
  ;; Run validation
  (print-report)

  ;; Get detailed results
  (validate-all)

  ;; For CI/CD
  (validate!))
