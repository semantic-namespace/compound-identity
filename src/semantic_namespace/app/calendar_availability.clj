(ns semantic-namespace.app.calendar-availability
  (:require
   [semantic-namespace.compound.identity :as cid]))

(defn init-registry!
  "Minimal semantic registry for solo dev with OAuth support."
  []
  ;; Data Schemas
  (cid/register!
   :schema/users
   #{:semantic-namespace/data-schema :domain/users}
   {::fields [:user/id :user/email :user/language :user/gcal-refresh-token]})

  (cid/register!
   :schema/scheduling-query
   #{:semantic-namespace/data-schema :domain/scheduling}
   {::fields [:query/date :query/language]})

  (cid/register!
   :schema/availability
   #{:semantic-namespace/data-schema :domain/scheduling}
   {::fields [:availability/users]})

  ;; Components
  (cid/register!
   :component/db
   #{:semantic-namespace/component :tier/foundation :domain/users}
   {::deps #{}})

  (cid/register!
   :component/google-oauth
   #{:semantic-namespace/component :tier/foundation :domain/google :integration/external :protocol/oauth}
   {::deps #{}})

  (cid/register!
   :component/gcal-client
   #{:semantic-namespace/component :tier/foundation :domain/google :integration/external}
   {::deps #{:component/google-oauth}})

  ;; Functions
  (cid/register!
   :fn/find-users-by-language
   #{:semantic-namespace/function :tier/service :domain/users}
   {::context [:query/language]
    ::response [:user/email :user/gcal-refresh-token]
    ::deps #{:component/db}})

  (cid/register!
   :fn/refresh-oauth-token
   #{:semantic-namespace/function :tier/service :domain/google :integration/external :protocol/oauth}
   {::context [:user/gcal-refresh-token]
    ::response [:oauth/access-token]
    ::deps #{:component/google-oauth}})

  (cid/register!
   :fn/check-user-availability
   #{:semantic-namespace/function :tier/service :domain/scheduling :integration/external}
   {::context [:query/date :oauth/access-token]
    ::response [:scheduling/available?]
    ::deps #{:component/gcal-client}})

  (cid/register!
   :fn/collect-available-users
   #{:semantic-namespace/function :tier/service :domain/scheduling :effect/pure}
   {::context [:query/date :query/language :user/email]
    ::response [:availability/users]
    ::deps #{}})

  ;; Endpoint
  (cid/register!
   :endpoint/query-availability
   #{:semantic-namespace/endpoint :semantic-namespace/function :tier/api :domain/scheduling}
   {::context [:query/date :query/language]
    ::response [:availability/users]
    ::deps #{:fn/find-users-by-language
             :fn/refresh-oauth-token
             :fn/check-user-availability
             :fn/collect-available-users}}))

(defn function-identity? [id]
  (contains? id :semantic-namespace/function))

(defn endpoint-identity? [id]
  (contains? id :semantic-namespace/endpoint))

(defn external-integration? [id]
  (contains? id :integration/external))

(defn axiom-endpoint-is-function [[id _v]]
  (when (endpoint-identity? id)
    (when-not (function-identity? id)
      {:axiom ::endpoint-is-function
       :identity id
       :reason "Endpoint must include :semantic-namespace/function"})))

(def axioms [axiom-endpoint-is-function])

(defn check-axioms []
  (for [entry @cid/registry
        axiom axioms
        :let [violation (axiom entry)]
        :when violation]
    violation))
