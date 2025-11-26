(ns semantic-namespace.tooling.lsp-helpers
  "Helper functions for IDE navigation of semantic identities.
  
  Works with standard clj-lsp - no custom LSP server needed!"
  (:require [semantic-namespace.compound.identity :as cid]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; =============================================================================
;; KEYWORD SEARCH - Find all usages of a dev-id
;; =============================================================================

(defn find-keyword-in-file
  "Find all occurrences of a keyword in a file"
  [file keyword-str]
  (try
    (let [content (slurp file)
          lines (str/split-lines content)]
      (keep-indexed
       (fn [idx line]
         (when (str/includes? line keyword-str)
           {:file (str file)
            :line (inc idx)
            :content (str/trim line)}))
       lines))
    (catch Exception _ nil)))

(defn find-dev-id-usages
  "Find all usages of a dev-id across the codebase"
  [dev-id]
  (let [keyword-str (str dev-id)
        source-dirs ["src" "test"]
        clj-files (for [dir source-dirs
                        :when (.exists (io/file dir))
                        file (file-seq (io/file dir))
                        :when (and (.isFile file)
                                   (or (.endsWith (.getName file) ".clj")
                                       (.endsWith (.getName file) ".cljs")
                                       (.endsWith (.getName file) ".cljc")))]
                    file)]
    (mapcat #(find-keyword-in-file % keyword-str) clj-files)))

;; =============================================================================
;; ASPECT SEARCH - Find all identities with a specific aspect
;; =============================================================================

(defn find-by-aspect
  "Find all identities containing an aspect, with their source info"
  [aspect]
  (for [identity-set (cid/all-identities)
        :when (contains? identity-set aspect)
        :let [value (cid/fetch identity-set)
              dev-id (:dev/id value)]]
    {:dev-id dev-id
     :identity identity-set
     :var (:source/var value)
     :ns (:source/ns value)
     :docs (:docs/content value)}))

;; =============================================================================
;; HOVER INFO - Generate hover text for IDE
;; =============================================================================

(defn hover-info
  "Generate hover information for a dev-id"
  [dev-id]
  (when-let [identity-set (first (filter #(= dev-id (:dev/id (cid/fetch %)))
                                         (cid/all-identities)))]
    (let [value (cid/fetch identity-set)
          aspects (sort identity-set)]
      (str/join "\n"
                ["**Semantic Identity**"
                 (str "**Dev-ID:** `" dev-id "`")
                 (str "**Aspects:** " (str/join ", " (map #(str "`" % "`") aspects)))
                 ""
                 (when-let [docs (:docs/content value)]
                   (str "**Documentation:**\n" docs))
                 ""
                 (when-let [tier (first (filter #(= "tier" (namespace %)) identity-set))]
                   (str "**Tier:** `" tier "`"))
                 (when-let [domain (first (filter #(= "domain" (namespace %)) identity-set))]
                   (str "**Domain:** `" domain "`"))
                 (when-let [var-sym (:source/var value)]
                   (str "**Var:** `" (:source/ns value) "/" var-sym "`"))]))))

;; =============================================================================
;; EXPORT FOR EXTERNAL TOOLS
;; =============================================================================

(defn export-definitions
  "Export definitions in a format external tools can use"
  []
  (for [identity-set (cid/all-identities)
        :let [value (cid/fetch identity-set)
              dev-id (:dev/id value)]]
    {:dev-id (str dev-id)
     :aspects (mapv str (sort identity-set))
     :var (str (:source/var value))
     :namespace (str (:source/ns value))
     :docs (:docs/content value)
     :searchable-string (str dev-id " " (str/join " " identity-set))}))

(defn write-search-index
  "Write a search index file for external tools"
  []
  (let [defs (export-definitions)
        index-file ".clj-kondo/.cache/semantic-search-index.edn"]
    (io/make-parents index-file)
    (spit index-file (pr-str defs))
    {:file index-file
     :entries (count defs)}))

;; =============================================================================
;; USAGE EXAMPLES
;; =============================================================================

(comment
  ;; Find all usages of a dev-id
  (find-dev-id-usages :component/database)
  ;; => [{:file "src/app/core.clj" :line 42 :content "(init component/database)"}
  ;;     {:file "src/app/db.clj" :line 10 :content "(defregistry component-database"}]

  ;; Find all components in foundation tier
  (find-by-aspect :tier/foundation)
  ;; => [{:dev-id :component/database :var component-database :ns my-app.db ...}]

  ;; Generate hover text
  (println (hover-info :component/database))
  ;; **Semantic Identity**
  ;; **Dev-ID:** `:component/database`
  ;; **Aspects:** `:domain/database`, `:semantic-namespace/component`, `:tier/foundation`
  ;; ...

  ;; Export for external search
  (write-search-index)
  ;; => {:file ".clj-kondo/.cache/semantic-search-index.edn" :entries 23}
  )
