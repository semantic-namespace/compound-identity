(ns semantic-namespace.compound.identity
  (:require [clojure.set :as set]))

(defonce registry (atom {}))

(defn query [find*]
  (let [find* (set find*)]
    (->> (keys @registry)
         (filter (fn [x] (set/superset? x find*)))
         (mapv (fn [x] (vec (sort (vec x))))))))

(defn remove [composed-identity]
  (swap! registry dissoc composed-identity))


(defn valid? [composed-identity]
  (and (set? composed-identity) (>= (count composed-identity) 2) (every? qualified-keyword? composed-identity)))

(defn exists? [compound-identity value]
  (let [internal-value (get @registry compound-identity)]
    [(boolean internal-value) (= internal-value value)]
    ))
