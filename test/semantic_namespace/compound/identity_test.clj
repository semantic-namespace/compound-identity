(ns semantic-namespace.compound.identity-test
  (:require [clojure.test :refer (deftest testing is)]
            [semantic-namespace.compound.identity :as identity]))
  
(deftest identity-test
  (testing "define specs, predicate and triple"
    (swap! identity/registry assoc #{:app/foo} "xxx")
    (swap! identity/registry assoc #{:app/foo :app/zoo} "yyyy")
    (swap! identity/registry assoc #{:app/foo :app/zoo :app/bis} "yyyybis")
    (swap! identity/registry assoc #{:app/zoo :app/zebra} "zzzz")
    
    (is (= (identity/query #{:app/foo :app/zoo})
           [[:app/foo :app/zoo] [:app/bis :app/foo :app/zoo]]))))


