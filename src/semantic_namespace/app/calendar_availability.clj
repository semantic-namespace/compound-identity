(ns semantic-namespace.app.calendar-availability
  (:require
   [semantic-namespace.compound.identity :as cid]))

(defn init-registry!
  "Initialize the calendar availability registry with compound identities."
  []
  ;; Data Schemas
  (cid/register!
   :schema.data/users
   #{:semantic-namespace/data-schema
     :semantic/docs
     :domain/users}
   {::fields [:user/id :user/email :user/language :user/gcal-refresh-token]
    :docs/content "User data schema with identity, contact, language, and Google Calendar token."})

  (cid/register!
   :schema.data.scheduling/query
   #{:semantic-namespace/data-schema
     :semantic/docs
     :domain/scheduling
     :scheduling/query}
   {::fields [:query/date :query/language]
    :docs/content "Scheduling query input: target date and language filter."})

  (cid/register!
   :schema.data.scheduling/availability
   #{:semantic-namespace/data-schema
     :semantic/docs
     :domain/scheduling
     :scheduling/availability}
   {::fields [:availability/users]
    :docs/content "Availability result: list of available users."})

  (cid/register!
   :schema.data.google/calendar-event
   #{:semantic-namespace/data-schema
     :semantic/docs
     :domain/google
     :google/calendar-event}
   {::fields [:gcal.event/start :gcal.event/end]
    :docs/content "Google Calendar event with start/end timestamps."})

  ;; Foundation Components
  (cid/register!
   :component.foundation.users/db
   #{:semantic-namespace/component
     :semantic/docs
     :tier/foundation
     :domain/users
     :integration/internal
     :effect/read
     :observability/metered}
   {::deps #{}
    ::impl ::not-implemented
    :docs/content "Database component for user persistence."})

  (cid/register!
   :component.foundation.google/oauth
   #{:semantic-namespace/component
     :semantic/docs
     :tier/foundation
     :domain/google
     :integration/external
     :protocol/http
     :protocol/oauth
     :effect/read
     :temporal/async
     :temporal/timeout-configured
     :observability/traced}
   {::deps #{}
    ::impl ::not-implemented
    :docs/content "Google OAuth2 component for API authorization."})

  (cid/register!
   :component.foundation.google/gcal-http
   #{:semantic-namespace/component
     :semantic/docs
     :tier/foundation
     :domain/google
     :domain/calendar
     :integration/external
     :protocol/http
     :effect/read
     :temporal/async
     :temporal/timeout-configured
     :observability/traced}
   {::deps #{}
    ::impl ::not-implemented
    :docs/content "Google Calendar HTTP client for event retrieval."})

  ;; Service Functions
  (cid/register!
   :service.users/find-by-language
   #{:semantic-namespace/function
     :semantic/docs
     :tier/service
     :domain/users
     :operation/search
     :effect/read
     :observability/metered}
   {::context [:query/language]
    ::response [:user/email :user/gcal-refresh-token]
    ::deps #{:component.foundation.users/db}
    ::impl ::not-implemented
    :docs/content "Search users by language preference."})

  (cid/register!
   :service.scheduling/check-user-day
   #{:semantic-namespace/function
     :semantic/docs
     :tier/service
     :domain/scheduling
     :operation/validate
     :effect/read
     :integration/external
     :temporal/async
     :temporal/timeout-configured
     :observability/traced
     :compliance/pii
     :compliance/audited}
   {::context [:query/date :user/gcal-refresh-token]
    ::response [:scheduling/available?]
    ::deps #{:component.foundation.google/gcal-http}
    ::impl ::not-implemented
    :docs/content "Check user availability for a date via Google Calendar."})

  (cid/register!
   :service.scheduling/collect-free-users
   #{:semantic-namespace/function
     :semantic/docs
     :tier/service
     :domain/scheduling
     :operation/process
     :effect/pure
     :observability/metered}
   {::context [:query/date :query/language :user/email]
    ::response [:availability/users]
    ::deps #{}
    ::impl ::not-implemented
    :docs/content "Aggregate available users into final result."})

  ;; API Endpoint
  (cid/register!
   :endpoint.api.scheduling/query-availability
   #{:semantic-namespace/endpoint
     :semantic-namespace/function
     :semantic/docs
     :tier/api
     :domain/scheduling
     :operation/search
     :effect/read
     :protocol/http
     :authorization/required
     :observability/logged
     :observability/metered
     :capacity/rate-limited
     :compliance/pii
     :compliance/audited}
   {::context [:query/date :query/language]
    ::response [:availability/users]
    ::deps #{:service.users/find-by-language
             :service.scheduling/check-user-day
             :service.scheduling/collect-free-users}
    ::impl ::not-implemented
    :docs/content "HTTP endpoint for querying user availability."})

  ;; Error Handlers
  (cid/register!
   :handler.error.http/google
   #{:semantic-namespace/error-handler
     :semantic/docs
     :domain/google
     :protocol/http}
   {::impl ::not-implemented
    :docs/content "Handle Google API HTTP failures."})

  (cid/register!
   :handler.error.timeout/scheduling
   #{:semantic-namespace/error-handler
     :semantic/docs
     :domain/scheduling
     :temporal/timeout}
   {::impl ::not-implemented
    :docs/content "Handle scheduling timeout errors."})

  (cid/register!
   :handler.error.auth/unauthorized
   #{:semantic-namespace/error-handler
     :semantic/docs
     :domain/api
     :authorization/unauthorized}
   {::impl ::not-implemented
    :docs/content "Handle unauthorized API requests."})

  (cid/register!
   :handler.error.capacity/rate-limited
   #{:semantic-namespace/error-handler
     :semantic/docs
     :domain/api
     :capacity/rate-limited}
   {::impl ::not-implemented
    :docs/content "Handle rate limit exceeded errors."}))

(defn function-identity?
  [id]
  (contains? id :semantic-namespace/function))

(defn endpoint-identity?
  [id]
  (contains? id :semantic-namespace/endpoint))

(defn external-integration?
  [id]
  (contains? id :integration/external))

(defn async?
  [id]
  (contains? id :temporal/async))

(defn axiom-function-has-impl
  [[id v]]
  (when (function-identity? id)
    (when-not (contains? v ::impl)
      {:axiom ::function-has-impl
       :identity id
       :reason "Function identity missing ::impl key"})))

(defn axiom-external-must-be-async
  [[id _v]]
  (when (external-integration? id)
    (when-not (async? id)
      {:axiom ::external-must-be-async
       :identity id
       :reason "External integration should be :temporal/async"})))

(defn axiom-endpoint-is-function
  [[id _v]]
  (when (endpoint-identity? id)
    (when-not (function-identity? id)
      {:axiom ::endpoint-is-function
       :identity id
       :reason "Endpoint identity should also include :semantic-namespace/function"})))

(def axioms
  [axiom-function-has-impl
   axiom-external-must-be-async
   axiom-endpoint-is-function])

(defn check-axioms
  "Run all axioms over the registry.
   Returns a seq of violation maps."
  []
  (for [entry @cid/registry
        axiom axioms
        :let [violation (axiom entry)]
        :when violation]
    violation))
