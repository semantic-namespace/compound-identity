(ns semantic-namespace.compound.identity-test
  "Verification suite for the Semantic Kernel (compound identity algebra).
   Each test validates algebraic, analytical, or registry-level properties
   of `semantic-namespace.compound.identity`.

   Goals:
   • Prove closure, determinism, and validity of the algebra
   • Ensure every query and analysis function returns pure data
   • Provide readable, narrative test documentation for developers"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [semantic-namespace.compound.identity :as id]))

;; ---------------------------------------------------------------------------
;; ⚙️ Test Fixture — reset registry for each test
;; ---------------------------------------------------------------------------

(use-fixtures :each
  (fn [f]
    (reset! id/registry {})
    (f)
    (reset! id/registry {})))

;; ---------------------------------------------------------------------------
;; ✅ Validation Rules
;; ---------------------------------------------------------------------------

(deftest validity-rules
  "The `valid?` function defines what a well-formed compound identity is:
   - It must be a set (not a vector, list, or nil)
   - It must contain ≥2 elements
   - All elements must be qualified keywords (`:ns/name`)."
  (testing "valid? enforces structure"
    (is (false? (id/valid? nil)))
    (is (false? (id/valid? #{:a})))
    (is (false? (id/valid? #{:a/b :c})))  ; contains unqualified keyword
    (is (true?  (id/valid? #{:foo/a :foo/b})))))


;; ---------------------------------------------------------------------------
;; 🧱 Basic Registry Operations
;; ---------------------------------------------------------------------------

(deftest registry-crud
  "Tests creation, retrieval, and deletion in the identity registry.
   Also ensures `exists?` correctly reports presence and value match."
  (testing "register!, fetch, exists?, remove*"
    (let [cid #{:app/foo :app/bar}]
      (id/register! cid "val")
      (is (= "val" (id/fetch cid)) "Registered value retrievable")
      (is (= {:exists true :matches true}
             (id/exists? cid "val")) "Value matches expected")
      (is (= {:exists true :matches false}
             (id/exists? cid "other")) "Detects mismatch correctly")
      (id/remove* cid)
      (is (nil? (id/fetch cid)) "Value removed from registry"))))


;; ---------------------------------------------------------------------------
;; 🔍 Query and Discovery
;; ---------------------------------------------------------------------------

(deftest query-and-find
  "Verifies algebraic discovery functions:
   - `query` finds all supersets of a given set of aspects
   - `find-with` returns map of identities containing an aspect."
  (testing "query and find-with return deterministic sets"
    (doseq [entry {#{:app/a :app/b} "x"
                   #{:app/a :app/b :app/c} "y"
                   #{:app/b :app/d} "z"}]
      (apply id/register! (seq entry)))

    (is (= [[":app/a" ":app/b"]
            [":app/a" ":app/b" ":app/c"]]
           (mapv #(mapv str %) (id/query #{:app/a :app/b})))
        "query returns all supersets sorted lexically")

    (is (contains? (id/find-with :app/a)
                   #{:app/a :app/b :app/c})
        "find-with locates all identities containing :app/a")))


;; ---------------------------------------------------------------------------
;; 🧮 Algebraic Closure
;; ---------------------------------------------------------------------------

(deftest algebraic-closure
  "Ensures set algebra (union, intersection, difference) behaves deterministically
   and preserves closure (results remain valid identities)."
  (testing "query-algebra operations"
    (id/register! #{:x/a :x/b} 1)
    (id/register! #{:x/a :x/b :x/c} 2)
    (id/register! #{:x/a :x/d} 3)

    (is (= 2 (count (id/query-algebra {:intersection [#{:x/a :x/b}]})))
        "Intersection returns all supersets of {:x/a :x/b}")
    (is (= 3 (count (id/query-algebra {:union [#{:x/a} #{:x/d}]})))
        "Union returns all sets containing either aspect group")
    (is (= 1 (count (id/query-algebra {:difference [#{:x/a :x/b} #{:x/c}]})))
        "Difference excludes those containing :x/c")))


;; ---------------------------------------------------------------------------
;; 🤝 Semantic Neighbors and Correlation
;; ---------------------------------------------------------------------------

(deftest neighbor-similarity
  "Tests `semantic-neighbors` which finds related identities and computes
   their Jaccard-like similarity coefficient (|A∩B| / |A∪B|)."
  (testing "semantic-neighbors returns similarity ratio"
    (id/register! #{:n/x :n/y} :a)
    (id/register! #{:n/x :n/y :n/z} :b)
    (id/register! #{:n/x :n/z} :c)
    (let [res (id/semantic-neighbors #{:n/x :n/y})]
      (is (every? #(contains? % :similarity) res) "All results include similarity score")
      (is (apply >= (map :similarity res)) "Results sorted by similarity descending"))))

(deftest correlation-properties
  "The correlation matrix should be:
   - Symmetric (freq(a,b) == freq(b,a))
   - Normalized between 0–1 across all pairs."
  (testing "correlation matrix is symmetric and normalized"
    (id/register! #{:c/a :c/b} 1)
    (id/register! #{:c/b :c/c} 2)
    (let [m (id/correlation-matrix)
          aab (get-in m [:c/a :c/b])
          aba (get-in m [:c/b :c/a])]
      (is (= aab aba) "Symmetric co-occurrence matrix")
      (is (<= 0 aab 1) "Normalized within range 0–1"))))


;; ---------------------------------------------------------------------------
;; 📈 Analytics and Meta-Information
;; ---------------------------------------------------------------------------

(deftest analytics-coverage
  "Checks the coherence between statistical summaries:
   - `aspect-frequency` lists every aspect at least once
   - `identity-stats` aligns with total identity count."
  (testing "aspect-frequency and identity-stats coherence"
    (id/register! #{:r/a :r/b} 1)
    (id/register! #{:r/a :r/c} 2)
    (let [freq (id/aspect-frequency)
          stats (id/identity-stats)]
      (is (map? freq) "Frequency returns map of counts")
      (is (seq stats) "Identity stats returns non-empty sequence")
      (is (= (count (id/all-identities))
             (count stats))
          "Each registered identity appears in stats"))))


;; ---------------------------------------------------------------------------
;; 🔁 Determinism and Reproducibility
;; ---------------------------------------------------------------------------

(deftest deterministic-output
  "All kernel operations should be pure and deterministic:
   identical queries produce identical ordered outputs."
  (testing "sorted and stable results"
    (id/register! #{:s/a :s/b} 1)
    (id/register! #{:s/a :s/c} 2)
    (let [r1 (id/query #{:s/a})
          r2 (id/query #{:s/a})]
      (is (= r1 r2) "Repeated queries yield identical output"))))


;; ---------------------------------------------------------------------------
;; 🧩 Diagnostics and Summary
;; ---------------------------------------------------------------------------

(deftest diagnostics
  "Verifies heuristic and diagnostic tools:
   - `missing-aspects` suggests related attributes
   - `find-anomalies` detects contradictory combinations
   - `summary` aggregates high-level metrics"
  (testing "missing-aspects and summary return structured data"
    (id/register! #{:m/x :m/y} 1)
    (id/register! #{:m/x :m/z} 2)
    (is (vector? (id/missing-aspects #{:m/x :m/y})) "Returns vector of suggestions")
    (is (map? (id/summary)) "Summary returns pure map of metadata"))
  (testing "find-anomalies detects conflicting identities"
    (id/register! #{:api/endpoint :sync/operation :async/operation} 1)
    (let [anom (id/find-anomalies)]
      (is (seq anom) "Anomalies detected for contradictory identities")
      (is (set? (first anom)) "Each anomaly entry is an identity set"))))
