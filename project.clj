(defproject com.github.semantic-namespace/compound-identity "0.1.1-SNAPSHOT"
  :description "semantic compound identity: identity as combination of semantic identities"
  :url "https://github.com/semantic-spec/compound-identity"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :creds :gpg}]]
  :profiles {:dev {:source-paths   ["dev/src"]
                   :repl-options   {:init-ns dev}
                   :dependencies [[org.clojure/tools.namespace "1.4.4"]]}})
