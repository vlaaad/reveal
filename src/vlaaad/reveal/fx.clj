(ns vlaaad.reveal.fx
  (:require [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.component :as fx.component])
  (:import [java.util UUID]))

(def ext-with-process
  "Extension lifecycle that provides \"local mutable state\" capability

  Expected props:
  - `:id` (optional) — state identifier
  - `:args` (optional) — input args to `:start` function
  - `:start` (required) — function that starts the process. Will receive 3 args as input:
    - id from `:id` prop
    - args from `:args` prop
    - event handler
  - `:desc (required)` that will additionally receive the state"
  (reify fx.lifecycle/Lifecycle
    (create [_ {:keys [desc start args id] :or {id ::undefined}} opts]
      (let [process-id (if (= ::undefined id) (UUID/randomUUID) id)
            map-event-handler (:fx.opt/map-event-handler opts)
            ret (start process-id args map-event-handler)
            state (-> map-event-handler :*state deref (get process-id))]
        (with-meta {:stop-fn (when (fn? ret) ret)
                    :process-id process-id
                    :fx.opt/map-event-handler map-event-handler
                    :id id
                    :start start
                    :args args
                    :child (fx.lifecycle/create fx.lifecycle/dynamic (into desc state) opts)}
                   {`fx.component/instance #(-> % :child fx.component/instance)})))
    (advance [this component {:keys [desc start args id] :or {id ::undefined} :as this-desc} opts]
      (let [map-event-handler (:fx.opt/map-event-handler opts)]
        (if (and (= start (:start component))
                 (= args (:args component))
                 (= id (:id component))
                 (= map-event-handler (:fx.opt/map-event-handler component)))
          (let [state (-> map-event-handler :*state deref (get (:process-id component)))]
            (update component :child #(fx.lifecycle/advance fx.lifecycle/dynamic % (into desc state) opts)))
          (do
            (fx.lifecycle/delete this component opts)
            (fx.lifecycle/create this this-desc opts)))))
    (delete [_ component opts]
      (when-let [stop-fn (:stop-fn component)]
        (stop-fn))
      (fx.lifecycle/delete fx.lifecycle/dynamic (:child component) opts))))