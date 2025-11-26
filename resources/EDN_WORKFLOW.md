# EDN-Driven Development Workflow

## Overview

This project uses EDN files as a **declarative source of truth** for system semantics. The EDN files serve multiple purposes:

1. **Schema Definition** - Define allowed semantic vocabulary
2. **System Specification** - Declare expected entities and relationships
3. **Validation Baseline** - Verify loaded code matches expectations
4. **Tooling Interface** - Enable analysis without running Clojure

## Files

```
resources/
â”œâ”€â”€ calendar-availability-ontology.edn  # Semantic vocabulary & rules
â”œâ”€â”€ calendar-availability-system.edn    # Concrete system definition
â””â”€â”€ EDN_WORKFLOW.md                     # This file
```

## Ontology EDN Structure

The ontology defines **what is possible** in the system:

```edn
{:meta {:version "0.1.0" :system "calendar-availability"}
 
 ;; What kinds of entities exist
 :entity-types
 {:semantic-namespace/function
  {:description "Business logic function"
   :required-props [:context :response :deps :impl]
   :tiers [:tier/service :tier/api]}
  
  :semantic-namespace/endpoint
  {:implies [:semantic-namespace/function]}}
 
 ;; Semantic aspect vocabulary
 :aspect-categories
 {:tier {:values [:tier/foundation :tier/service :tier/api]
         :ordering [:tier/foundation :tier/service :tier/api]}
  :effect {:values [:effect/pure :effect/read :effect/write]}}
 
 ;; Invariants that must hold
 :axioms
 [{:id :external-must-be-async
   :applies-to :integration/external
   :rule "External integrations must be :temporal/async"}]}
```

## System EDN Structure

The system defines **what actually exists**:

```edn
{:meta {:version "0.1.0" :generated-at "2025-01-15T..."}
 
 :entities
 {:service.users/find-by-language
  {:identity [:domain/users :effect/read :operation/search 
              :semantic-namespace/function :tier/service]
   :context [:query/language]
   :response [:user/email :user/gcal-refresh-token]
   :deps #{:component/db}
   :impl :not-implemented
   :docs "Search users by language preference"}}}
```

## Development Workflows

### 1. Schema-First Development

Start with ontology, then implement:

```
1. Define entity types in ontology.edn
2. Add axioms for invariants
3. Create system.edn with entity stubs
4. Implement CLJ to match system.edn
5. Run validator to confirm alignment
```

### 2. Code-First with Export

Implement first, then capture as EDN:

```
1. Build CLJ with register! calls
2. Run export to generate system.edn
3. Review and refine ontology.edn
4. Commit both as source of truth
5. Future changes validated against EDN
```

### 3. Continuous Validation

CI pipeline integration:

```clojure
(require '[semantic-namespace.tooling.edn-validator :as v])

(defn ci-check []
  (let [result (v/validate-all 
                 "resources/calendar-availability-ontology.edn"
                 "resources/calendar-availability-system.edn"
                 :semantic-namespace.app.calendar-availability/id)]
    (when-not (:valid? result)
      (println "Validation failures:" result)
      (System/exit 1))))
```

## Validation Types

### Axiom Validation

Checks runtime registry against ontology axioms:

```clojure
(v/validate-registry-against-ontology "resources/ontology.edn")
;; => () ; empty = no violations
;; => [{:axiom :external-must-be-async :entity #{...} :rule "..."}]
```

### Drift Detection

Compares loaded CLJ registry to expected system.edn:

```clojure
(v/compare-registry-to-system "resources/system.edn" ::app/id)
;; => {:missing-in-registry #{}
;;     :extra-in-registry #{}
;;     :identity-mismatches []}
```

### Full Validation

Combined check:

```clojure
(v/validate-all ontology-path system-path id-key)
;; => {:valid? true
;;     :axiom-violations []
;;     :comparison {...}}
```

## Use Cases

### LLM/Tooling Integration

EDN files enable tools to understand system structure without executing Clojure:

```python
# Python tooling can read EDN
import edn_format
ontology = edn_format.loads(open("ontology.edn").read())
# Analyze, generate, validate...
```

### Architecture Documentation

EDN serves as living documentation:

- Entity types define the conceptual model
- Aspect categories are the semantic vocabulary
- Axioms encode architectural decisions
- System.edn is the current state

### Schema Evolution

Track changes over time:

```
git diff resources/calendar-availability-system.edn
# See exactly what entities/identities changed
```

### Pre-commit Hooks

Validate before committing:

```bash
#!/bin/bash
clojure -X:validate
if [ $? -ne 0 ]; then
  echo "EDN validation failed"
  exit 1
fi
```

## Regenerating EDN from Registry

When CLJ is source of truth:

```clojure
(require '[semantic-namespace.app.calendar-availability :as cal])
(cal/init-registry!)

;; Export current state
(defn export-system! []
  (spit "resources/calendar-availability-system.edn"
        (with-out-str (pprint (build-system-edn)))))
```

## Best Practices

1. **Version both files** - ontology.edn and system.edn should be in version control
2. **Validate in CI** - Catch drift early
3. **Ontology is stable** - Change infrequently, system.edn changes with features
4. **Axioms as guardrails** - Add axioms when you discover invariants
5. **Docs in EDN** - Keep `:docs` fields updated, they're queryable

## Error Messages

Common validation failures:

| Error | Meaning | Fix |
|-------|---------|-----|
| `:missing-in-registry` | EDN expects entity not in CLJ | Add registration or remove from EDN |
| `:extra-in-registry` | CLJ has entity not in EDN | Add to EDN or remove registration |
| `:identity-mismatches` | Aspects differ between EDN and CLJ | Align identity sets |
| `:axiom-violations` | Registry violates ontology rule | Fix CLJ to satisfy axiom |
