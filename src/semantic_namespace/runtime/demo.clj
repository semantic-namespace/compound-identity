(ns semantic-namespace.runtime.demo
  "Demonstration of semantic-driven runtime."
  (:require [semantic-namespace.compound.identity :as cid]
            [semantic-namespace.app.calendar-availability :as cal]
            [semantic-namespace.runtime.core :as rt]
            [semantic-namespace.runtime.executor :as exec]))

(defn setup! []
  (reset! cid/registry {})
  (cal/init-registry!))

(def impl-map
  {:fn/find-users-by-language
   (fn [ctx] {:user/email "alice@example.com" :user/gcal-refresh-token "refresh-abc"})

   :fn/refresh-oauth-token
   (fn [ctx] (Thread/sleep 50) {:oauth/access-token "access-xyz"})

   :fn/check-user-availability
   (fn [ctx] (Thread/sleep 50) {:scheduling/available? true})

   :fn/collect-available-users
   (fn [ctx] {:availability/users [(:user/email ctx)]})

   :endpoint/query-availability
   (fn [ctx] {:result (:availability/users ctx)})})

(defn demo-queries []
  (println "\n=== SEMANTIC QUERIES ===\n")

  (println "1. All external integrations:")
  (doseq [id (rt/all-with-aspect :integration/external)]
    (println "  " id))

  (println "\n2. All pure functions:")
  (doseq [id (rt/all-with-aspect :effect/pure)]
    (println "  " id))

  (println "\n3. Data flow for :fn/check-user-availability:")
  (doseq [{:keys [needs produced-by satisfied?]} (rt/trace-data-flow :fn/check-user-availability)]
    (println "  " needs "-> produced by:" (if satisfied? produced-by "NONE"))))

(defn demo-execution []
  (println "\n=== SEMANTIC-DRIVEN EXECUTION ===\n")

  (println "1. Missing context validation:")
  (println "  " (exec/execute :fn/find-users-by-language (impl-map :fn/find-users-by-language) {}))

  (println "\n2. Correct context:")
  (println "  " (exec/execute :fn/find-users-by-language (impl-map :fn/find-users-by-language) {:query/language "en"}))

  (println "\n3. External fn gets auto-timeout (100ms limit, 50ms call):")
  (println "  " (binding [exec/*timeout-ms* 100]
                  (exec/execute :fn/refresh-oauth-token (impl-map :fn/refresh-oauth-token) {:user/gcal-refresh-token "x"})))

  (println "\n4. External fn timeout triggers (10ms limit, 50ms call):")
  (println "  " (binding [exec/*timeout-ms* 10]
                  (exec/execute :fn/refresh-oauth-token (impl-map :fn/refresh-oauth-token) {:user/gcal-refresh-token "x"}))))

(defn demo-pipeline []
  (println "\n=== FULL PIPELINE ===\n")

  (println "Execution order (computed from data flow):")
  (doseq [id (rt/topo-sort-by-data (conj (rt/deps-for :endpoint/query-availability) :endpoint/query-availability))]
    (println "  " id))

  (println "\nPipeline result:")
  (let [pipeline (exec/build-pipeline :endpoint/query-availability impl-map)]
    (clojure.pprint/pprint (pipeline {:query/date "2024-01-15" :query/language "en"}))))

(defn run-demo []
  (setup!)
  (demo-queries)
  (demo-execution)
  (demo-pipeline))

(comment
  (run-demo))
