(ns vlaaad.reveal.fx
  (:require [cljfx.lifecycle :as fx.lifecycle]
            [cljfx.component :as fx.component]
            [vlaaad.reveal.event :as event])
  (:import [java.util UUID]
           [java.util.function UnaryOperator]
           [javafx.scene.control TextFormatter$Change]))

(def ext-with-process
  "Extension lifecycle that provides \"local mutable state\" capability with a process

  Expected props:
  - `:id` (optional) — state identifier
  - `:args` (optional) — input args to `:start` function
  - `:start` (required) — function that starts the process. Will receive 3 args as input:
    - id from `:id` prop
    - args from `:args` prop
    - event handler
    If returns a function, it will be called when process is stopped
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

(defmethod event/handle ::init-local-state [{:keys [id local-state]}]
  #(assoc % id {:id id :local-state local-state}))

(defmethod event/handle ::update-local-state [{:keys [id fn]}]
  #(cond-> % (contains? % id) (update-in [id :local-state] fn)))

(defmethod event/handle ::dispose-local-state [{:keys [id]}]
  #(dissoc % id))

(defn- init-local-state! [id initial-state handler]
  (handler {::event/type ::init-local-state :id id :local-state initial-state})
  #(handler {::event/type ::dispose-local-state :id id}))

(defn- local-state-view [{:keys [id desc local-state]}]
  (assoc desc :local-state local-state
              :on-local-state-changed {::event/type ::update-local-state
                                       :id id}))

(defn ext-with-local-state
  "Extension lifecycle that provides simple \"local mutable state\" capability

  Expected props
  - :initial-state (optional, default nil) - initial state, resets the accumulated
    local state if changed
  - :desc (required) - cljfx desc map, will receive 2 additional properties:
    - :local-state with current local state value
    - :on-local-state-changed map event that expect extra :fn key when dispatched,
      which should be a function that transforms the local state to another value"
  [{:keys [initial-state desc]}]
  {:fx/type ext-with-process
   :args initial-state
   :start init-local-state!
   :desc {:fx/type local-state-view
          :desc desc}})

(defn- format! [^TextFormatter$Change change]
  (when (.isContentChange change)
    (let [range-start (.getRangeStart change)
          range-end (.getRangeEnd change)
          control-text (.getControlText change)
          look-back (fn [start pred]
                      (loop [matches 0]
                        (let [i (- start matches)]
                          (cond
                            (neg? i) matches
                            (pred (.charAt control-text i)) (recur (inc matches))
                            :else matches))))
          inputs-char (odd? (look-back (dec range-start) #{\\}))
          move-right? (fn [char]
                        (and (not (.isDeleted change))
                             (< range-end (.length control-text))
                             (= char (.charAt control-text range-end))))
          move-right! (fn []
                        (.setText change "")
                        (.selectRange change (inc range-end) (inc range-end)))]
      (cond
        (not (.isAdded change))
        (when (and (= 1 (- range-end range-start))
                   (< range-end (.length control-text)))
          (let [deleted-char (.charAt control-text range-start)
                next-char (.charAt control-text range-end)]
            (case [deleted-char next-char]
              ([\" \"] [\{ \}] [\[ \]] [\( \)])
              (.setRange change range-start (inc range-end))
              nil)))

        inputs-char
        nil

        :else
        (case (.getText change)
          "\"" (if (move-right? \") (move-right!) (.setText change "\"\""))
          "[" (.setText change "[]")
          "]" (when (move-right? \]) (move-right!))
          "{" (.setText change "{}")
          "}" (when (move-right? \}) (move-right!))
          "(" (.setText change "()")
          ")" (when (move-right? \)) (move-right!))
          "\n" (let [spaces
                     (loop [i (dec range-start)
                            stack 0]
                       (cond
                         (neg? stack) (inc (look-back i (complement #{\newline})))
                         (neg? i) 0
                         :else (let [ch (.charAt control-text i)]
                                 (recur
                                   (dec i)
                                   (cond
                                     (and (#{\[ \{ \(} ch) (even? (look-back (dec i) #{\\})))
                                     (dec stack)

                                     (and (#{\] \} \)} ch) (even? (look-back (dec i) #{\\})))
                                     (inc stack)

                                     :else
                                     stack)))))]
                 (doto change
                   (.setText (str \newline (apply str (repeat spaces \space))))
                   (.selectRange (+ spaces range-start 1) (+ spaces range-start 1))))
          nil)))))

(def code-text-formatter-filter
  (reify UnaryOperator
    (apply [_ v]
      (doto v format!))))

(def code-text-formatter
  {:fx/type :text-formatter
   :filter code-text-formatter-filter})