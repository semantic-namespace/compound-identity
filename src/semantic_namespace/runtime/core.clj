(ns semantic-namespace.runtime.core
  "Runtime that drives behavior from semantic registry."
  (:require [semantic-namespace.compound.identity :as cid]
            [semantic-namespace.app.calendar-availability :as cal]))

(defn fetch-by-dev-id [dev-id]
  (first (for [[id v] @cid/registry
               :when (= dev-id (:dev/id v))]
           [id v])))

(defn identity-for [dev-id]
  (first (fetch-by-dev-id dev-id)))

(defn props-for [dev-id]
  (second (fetch-by-dev-id dev-id)))

(defn has-aspect? [dev-id aspect]
  (contains? (identity-for dev-id) aspect))

(defn all-with-aspect [aspect]
  (->> @cid/registry
       (filter (fn [[id _]] (contains? id aspect)))
       (map (fn [[_ v]] (:dev/id v)))))

(defn deps-for [dev-id]
  (::cal/deps (props-for dev-id)))

(defn topo-sort
  "Topological sort of dev-ids by dependencies."
  [dev-ids]
  (let [deps-map (into {} (map (fn [id] [id (or (deps-for id) #{})]) dev-ids))
        sorted (atom [])
        visited (atom #{})]
    (letfn [(visit [id]
              (when-not (@visited id)
                (swap! visited conj id)
                (doseq [dep (get deps-map id #{})]
                  (visit dep))
                (swap! sorted conj id)))]
      (doseq [id dev-ids]
        (visit id)))
    @sorted))

(defn trace-data-flow
  "Trace what produces each context key for a function."
  [dev-id]
  (let [props (props-for dev-id)
        context (::cal/context props)
        all-fns (all-with-aspect :semantic-namespace/function)]
    (for [ctx-key context
          :let [producers (->> all-fns
                               (filter (fn [fid]
                                         (let [resp (::cal/response (props-for fid))]
                                           (some #{ctx-key} resp))))
                               vec)]]
      {:needs ctx-key
       :produced-by producers
       :satisfied? (seq producers)})))

(defn compute-data-deps
  "Compute dependencies based on context/response data flow."
  [dev-id]
  (let [props (props-for dev-id)
        context (set (::cal/context props))
        all-fns (all-with-aspect :semantic-namespace/function)]
    (->> all-fns
         (filter (fn [fid]
                   (let [resp (set (::cal/response (props-for fid)))]
                     (seq (clojure.set/intersection context resp)))))
         set)))

(defn topo-sort-by-data
  "Topological sort based on data flow (context/response)."
  [dev-ids]
  (let [deps-map (into {} (map (fn [id] [id (compute-data-deps id)]) dev-ids))
        sorted (atom [])
        visited (atom #{})]
    (letfn [(visit [id]
              (when-not (@visited id)
                (swap! visited conj id)
                (doseq [dep (get deps-map id #{})]
                  (when (contains? (set dev-ids) dep)
                    (visit dep)))
                (swap! sorted conj id)))]
      (doseq [id dev-ids]
        (visit id)))
    @sorted))
