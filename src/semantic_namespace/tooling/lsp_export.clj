(ns semantic-namespace.tooling.lsp-export
  "Export semantic registry data for LSP consumption"
  (:require [semantic-namespace.compound.identity :as cid]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn export-registry-for-lsp
  "Export registry to .clj-kondo/.cache for LSP navigation"
  []
  (let [all-entries (cid/all-identities)
        export-data {:identities
                     (map (fn [identity-set]
                            (let [value (cid/fetch identity-set)
                                  dev-id (:dev/id value)]
                              {:dev-id (str dev-id)
                               :identity (vec (sort (map str identity-set)))
                               :aspects (into {} (map (fn [aspect]
                                                        [(namespace aspect) (name aspect)])
                                                      identity-set))
                               :docs (:docs/content value)
                               :tier (or (first (filter #(= "tier" (namespace %)) identity-set))
                                         :unknown)
                               :domain (or (first (filter #(= "domain" (namespace %)) identity-set))
                                           :unknown)}))
                          all-entries)
                     :aspects
                     (let [freq (cid/aspect-frequency)]
                       (map (fn [[aspect count]]
                              {:aspect (str aspect)
                               :namespace (namespace aspect)
                               :name (name aspect)
                               :usage-count count})
                            freq))
                     :metadata
                     {:total-identities (count all-entries)
                      :exported-at (System/currentTimeMillis)}}]

    ;; Write to .clj-kondo/.cache/semantic-registry.json
    (io/make-parents ".clj-kondo/.cache/semantic-registry.json")
    (with-open [w (io/writer ".clj-kondo/.cache/semantic-registry.json")]
      (json/write export-data w))

    ;; Also write a definitions map for go-to-definition
    ;; Map dev-id -> location information
    ;; This would need source location tracking in the registry
    (let [definitions (into {}
                            (map (fn [identity-set]
                                   (let [value (cid/fetch identity-set)
                                         dev-id (:dev/id value)]
                                     [(str dev-id)
                                      {:identity (vec (sort (map str identity-set)))
                                       :file (:source-file value)
                                       :line (:source-line value)
                                       :column (:source-column value)}]))
                                 all-entries))]
      (with-open [w (io/writer ".clj-kondo/.cache/semantic-definitions.json")]
        (json/write {:definitions definitions} w)))

    {:exported-identities (count all-entries)
     :exported-aspects (count (cid/aspect-frequency))
     :output-file ".clj-kondo/.cache/semantic-registry.json"}))

(defn watch-and-export
  "Watch registry for changes and auto-export"
  []
  (add-watch cid/registry ::lsp-export
             (fn [_ _ _ _]
               (export-registry-for-lsp))))

(defn unwatch
  "Stop watching registry"
  []
  (remove-watch cid/registry ::lsp-export))
