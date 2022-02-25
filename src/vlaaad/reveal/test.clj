(ns vlaaad.reveal.test
  {:oh-my true}
  (:require [clojure.test :as t]
            [vlaaad.reveal.view :as view]
            [clojure.java.classpath :as cp]
            [clojure.tools.namespace.find :as find]
            [clojure.string :as str]
            [vlaaad.reveal.stream :as stream]
            [clojure.main :as m]
            [vlaaad.reveal.event :as event]
            [vlaaad.reveal.action :as action]
            [vlaaad.reveal.style :as style]
            [cljfx.api :as fx]
            [cljfx.ext.tree-view :as fx.ext.tree-view]
            [vlaaad.reveal.action-popup :as action-popup]
            [vlaaad.reveal.ui :as ui]
            [cljfx.prop :as fx.prop]
            [cljfx.mutator :as fx.mutator]
            [cljfx.lifecycle :as fx.lifecycle])
  (:import [clojure.lang Var Namespace]
           [javafx.scene.input KeyEvent KeyCode]
           [javafx.event Event EventDispatcher]
           [javafx.scene.control TreeView TreeCell TreeItem]
           [javafx.scene Node]
           [javafx.css PseudoClass]
           [java.util Collection]))

(defn- scan-classpath [{:keys [include exclude]}]
  (let [ns-filter (every-pred
                    (if include
                      (->> include
                           (map (fn [x]
                                  (if (simple-symbol? x)
                                    #(= x (:name %))
                                    (constantly true))))
                           (apply some-fn))
                      (constantly true))
                    (if exclude
                      (->> exclude
                           (map (fn [x]
                                  (cond
                                    (simple-symbol? x) #(= x (:name %))
                                    (keyword? x) #(x (:meta %))
                                    :else (constantly false))))
                           (apply some-fn)
                           complement)
                      (constantly true)))
        nses (->> (cp/classpath-directories)
                  (remove #(str/includes? (str %) ".gitlibs"))
                  (find/find-ns-decls)
                  (map (fn [[_ name & rest]]
                         {:name name
                          :meta (into (or (meta name) {})
                                      (filter map?)
                                      rest)}))
                  (filter ns-filter)
                  (map :name))
        _ (run! require nses)
        var-filter (every-pred
                     #(:test (meta %))
                     (if include
                       (->> include
                            (map (fn [x]
                                   (cond
                                     (qualified-symbol? x) #(= x (symbol %))
                                     (keyword? x) #(or (x (meta %))
                                                       (x (meta (.-ns ^Var %))))
                                     :else (constantly true))))
                            (apply some-fn))
                       (constantly true))
                     (if exclude
                       (->> exclude
                            (map (fn [x]
                                   (cond
                                     (qualified-symbol? x) #(= x (symbol %))
                                     (keyword? x) #(x (meta %))
                                     :else (constantly false))))
                            (apply some-fn)
                            complement)
                       (constantly true)))
        vars (->> nses
                  (map ns-publics)
                  (mapcat vals)
                  (filter var-filter)
                  set)]
    vars))

(defn- test->ns+var* [test]
  (cond
    (= :everything test) (test->ns+var* {})
    (simple-symbol? test) {:ns #{(or (find-ns test) (do (require test) (the-ns test)))}}
    (instance? Namespace test) {:ns #{test}}
    (qualified-symbol? test) {:var #{(requiring-resolve test)}}
    (instance? Var test) {:var #{test}}
    (map? test) {:var (scan-classpath test)}
    (coll? test) (apply merge-with into (map test->ns+var* test))
    :else (throw (ex-info (str "Don't know how to test " test) {:on test}))))

(defn- test->ns+var [test]
  (let [{:keys [ns]
         :or {ns #{}}
         :as ret} (test->ns+var* test)]
    (-> ret
        (update :ns #(sort-by str %))
        (update :var (fn [vars]
                       (->> vars
                            (remove #(ns (.-ns ^Var %)))
                            (sort-by #(:line (meta %)))))))))

(defn failure-path-comparator [a b]
  (let [max-length (max (count a) (count b))]
    (loop [i 0]
      (let [ret (compare (get a i) (get b i))]
        (if (and (zero? ret) (< i max-length))
          (recur (inc i))
          ret)))))

(def ^:private fresh-state
  {:running false
   :ctx ()
   :pass 0
   :fail 0
   :test 0
   :output []
   :selection [:auto []]
   :failure-paths (sorted-set-by failure-path-comparator)})

(defn runner [test]
  {:test test
   :state (atom fresh-state)})

(defn- update-children-in [nodes path ctx f & args]
  (let [n (count ctx)]
    (if (zero? n)
      (apply f nodes path args)
      (let [name (peek ctx)]
        (if (= name (:name (peek nodes)))
          (let [i (dec (count nodes))]
            (apply update-in nodes [i :children] update-children-in (conj path i) (pop ctx) f args))
          (let [path (conj path (count nodes))]
            (conj nodes {:name name
                         :path path
                         :children (apply update-children-in [] path (pop ctx) f args)})))))))

(defn- add-branch [nodes ctx branch-name & {:as attrs}]
  (update-children-in nodes [] ctx (fn [nodes path]
                                     (conj nodes (assoc attrs
                                                   :name branch-name
                                                   :path (conj path (count nodes))
                                                   :children [])))))

(defn- add-value [nodes ctx value]
  (update-children-in nodes [] ctx (fn [nodes path]
                                     (conj nodes {:value value
                                                  :path (conj path (count nodes))}))))

(defn- update-branch [nodes ctx f & args]
  (let [parents (into () (reverse (butlast ctx)))
        name (last ctx)]
    (update-children-in nodes [] parents (fn [nodes path]
                                           (if (= name (:name (peek nodes)))
                                             (apply update nodes (dec (count nodes)) f args)
                                             (conj nodes (apply f {:name name
                                                                   :path (conj path (count nodes))
                                                                   :children []} args)))))))

(defn- update-in-path [nodes path f & args]
  (apply update-in nodes (interpose :children path) f args))

(defn- update-every-in-path [nodes path f & args]
  (reduce
    (fn [nodes path]
      (apply update-in-path nodes path f args))
    nodes
    (drop 1 (reductions conj [] path))))

(defn- update-every-branch [nodes ctx f & args]
  (reduce
    (fn [nodes ctx]
      (apply update-branch nodes ctx f args))
    nodes
    (drop 1 (reductions conj [] (reverse ctx)))))

(defn- last-path [nodes]
  (loop [path []
         nodes nodes]
    (let [i (dec (count nodes))]
      (if (neg? i)
        path
        (recur (conj path i) (:children (nodes i) []))))))

(defn- mark-failure-path [{:keys [output] :as state}]
  (update state :failure-paths conj (last-path output)))

(defn- process-state [state e]
  (case (:type e)
    :start (-> state
               (update :ctx conj (:name e))
               (update :output add-branch (into () (:ctx state)) (:name e) :start (System/currentTimeMillis))
               (cond-> (:test e)
                 (update :test inc)))
    :stop (-> state
              (update :ctx pop)
              (update :output update-branch (into () (:ctx state)) assoc :end (System/currentTimeMillis)))
    :pass (-> state
              (update :pass inc)
              (update :output add-value (into (:ctx e) (:ctx state)) e))
    :fail (-> state
              (update :fail inc)
              (update :output add-value (into (:ctx e) (:ctx state)) e)
              (update :output update-every-branch (into (:ctx e) (:ctx state)) assoc :fail true)
              mark-failure-path)
    :out (update state :output add-value (into (:ctx e) (:ctx state)) e)))

(defn test! [{:keys [state test]}]
  (event/daemon-future
    (let [old @state]
      (when (and (not (:running old))
                 (compare-and-set! state old (assoc fresh-state :running true)))
        (let [process #(swap! state process-state %)]
          (try
            (let [{:keys [ns var]} (test->ns+var* test)]
              (binding [*out* (PrintWriter-on #(process {:type :out
                                                         :message (str/trim-newline %)
                                                         :ctx (vec t/*testing-contexts*)})
                                              nil)
                        t/*test-out* (PrintWriter-on #(process {:type :out
                                                                :message (str/trim-newline %)
                                                                :ctx (vec t/*testing-contexts*)})
                                                     nil)
                        t/report (fn [x]
                                   (when-let [e (case (:type x)
                                                  :begin-test-ns {:type :start :name (str (.getName ^Namespace (:ns x)))}
                                                  :begin-test-var {:type :start :name (str (symbol (:var x))) :test true}
                                                  :end-test-ns {:type :stop}
                                                  :end-test-var {:type :stop}
                                                  :pass (assoc x :type :pass :ctx (vec t/*testing-contexts*))
                                                  :fail (assoc x :type :fail :ctx (vec t/*testing-contexts*))
                                                  :error (assoc x :type :fail :ctx (vec t/*testing-contexts*))
                                                  nil)]
                                     (process e)))]
                (run! t/test-ns ns)
                (t/test-vars var)))
            (catch Exception e
              (process {:type :fail
                        :ctx (vec t/*testing-contexts*)
                        :message "Uncaught exception, not in assertion."
                        :actual e}))
            (finally (swap! state assoc :running false)))))))
  nil)

(defn- describe-cell [e]
  (case (:type e)
    :ctx (cond-> {:style-class ["cell" "tree-cell" "reveal-test-tree-cell"]
                  :pseudo-classes #{(if (:fail e) :fail :pass)}
                  :content-display :right
                  :text (:name e)}
           (and (:start e) (:end e))
           (assoc :graphic {:fx/type :label
                            :style-class "reveal-test-time-label"
                            :text (str (- (:end e) (:start e)) "ms")}))

    :pass {:style-class ["cell" "tree-cell" "reveal-test-tree-cell"]
           :pseudo-classes #{:pass}
           :text (stream/->str (:expected e))}
    :fail {:style-class ["cell" "tree-cell" "reveal-test-tree-cell"]
           :pseudo-classes #{:fail}
           :text (let [actual (:actual e)]
                   (str
                     (when-let [msg (:message e)]
                       (str msg " "))
                     (if (instance? Throwable actual)
                       (first (str/split-lines (m/ex-str (m/ex-triage (Throwable->map actual)))))
                       (stream/->str (:expected e)))))}
    :out {:style-class ["cell" "tree-cell" "reveal-test-tree-cell"]
          :pseudo-classes #{:out}
          :text (:message e)}
    {}))

(def ext-with-appended-children
  (fx/make-ext-with-props
    {:children (fx.prop/make
                 (fx.mutator/setter
                   (fn [^TreeItem item new-children]
                     (let [children (.getChildren item)]
                       (if (and (<= (count children) (count new-children))
                                (= children (subvec new-children 0 (count children))))
                         (.addAll children ^Collection (subvec new-children (count children)))
                         (.setAll children ^Collection new-children)))))
                 fx.lifecycle/dynamics)}))

(defn- tree-item [props]
  {:fx/type ext-with-appended-children
   :props {:children (:children props [])}
   :desc (-> props
             (assoc :fx/type :tree-item)
             (dissoc :children))})

(defmethod event/handle ::change-expanded [{:keys [path runner fx/event]}]
  (swap! (:state runner) update :output update-in-path path assoc :expanded event)
  identity)

(defn- node->tree-item [runner node]
  (if (:name node)
    (let [expanded (:expanded node (or (:fail node) (not (:end node))))]
      {:fx/type tree-item
       :expanded expanded
       :on-expanded-changed {::event/type ::change-expanded
                             :path (:path node)
                             :runner runner}
       :value (-> node
                  (assoc :type :ctx)
                  (dissoc :children))
       :children (if expanded
                   (mapv #(node->tree-item runner %) (:children node))
                   [{:fx/type tree-item}])})
    {:fx/type tree-item
     :value (assoc (:value node) :path (:path node))}))

(defmethod event/handle ::run [{:keys [runner]}]
  (test! runner)
  identity)

(defn- init-tree-view! [^TreeView tree-view]
  (let [dispatcher (.getEventDispatcher tree-view)]
    (-> tree-view
        (.setEventDispatcher
          (reify EventDispatcher
            (dispatchEvent [_ e next]
              (if (and (instance? KeyEvent e)
                       (= KeyEvent/KEY_PRESSED (.getEventType e)))
                (let [^KeyEvent e e]
                  (if (or (.isShortcutDown e)
                          (#{KeyCode/ESCAPE} (.getCode e)))
                    e
                    (.dispatchEvent dispatcher e next)))
                (.dispatchEvent dispatcher e next))))))))

(defn- select-tree-bounds-and-value! [^Event e]
  (let [^TreeView view (.getSource e)]
    (when-let [^TreeCell cell (->> (.lookupAll view ".tree-cell:selected")
                                   (some #(when (contains? (.getPseudoClassStates ^Node %)
                                                           (PseudoClass/getPseudoClass "selected"))
                                            %)))]
      (when-let [^Node node (.lookup cell ".tree-cell > .text")]
        {:bounds (.localToScreen node (.getBoundsInLocal node))
         :value (.getItem cell)}))))

(def ext-with-selected-path
  (fx/make-ext-with-props
    {:selected-path (fx.prop/make
                      (fx.mutator/setter
                        (fn [^TreeView view [mode path scroll]]
                          (when path
                            (let [^TreeItem item (reduce (fn [^TreeItem acc i]
                                                           (-> acc .getChildren (.get i)))
                                                         (.getRoot view)
                                                         path)]
                              (when (or (= mode :auto) scroll)
                                (when-let [f (.lookup view ".virtual-flow")]
                                  ;; using reflection on purpose to support javafx 8 and 9
                                  ;; where VirtualFlow class is in different packages
                                  (.scrollTo f (.getRow view item))))
                              (.select (.getSelectionModel view) item)))))
                      fx.lifecycle/scalar)}))

(defmethod event/handle ::select [{:keys [runner ^TreeItem fx/event]}]
  (when event
    (let [path (:path (.getValue event))]
      (swap! (:state runner) (fn [state]
                               (if (= path (last-path (:output state)))
                                 (assoc state :selection [:auto path])
                                 (assoc state :selection [:manual path]))))))
  identity)

(defmethod event/handle ::handle-key-press [{:keys [runner ^KeyEvent fx/event]}]
  (when (.isAltDown event)
    (condp = (.getCode event)
      KeyCode/UP (swap! (:state runner) (fn [{:keys [failure-paths selection] :as state}]
                                          (let [[_ path] selection]
                                            (if-let [prev (first (rsubseq failure-paths < path))]
                                              (-> state
                                                  (assoc :selection [:manual prev true])
                                                  (update :output update-every-in-path (pop prev) assoc :expanded true))
                                              state))))
      KeyCode/DOWN (swap! (:state runner) (fn [{:keys [failure-paths selection] :as state}]
                                            (let [[_ path] selection]
                                              (if-let [next (first (subseq failure-paths > path))]
                                                (-> state
                                                    (assoc :selection [:manual next true])
                                                    (update :output update-every-in-path (pop next) assoc :expanded true))
                                                state))))
      nil))
  identity)

(defn- result-tree-view [{:keys [runner]}]
  (let [{:keys [state]} runner]
    {:fx/type view/observable-view
     :ref state
     :fn (fn [{:keys [output failure-paths selection]}]
           {:fx/type action-popup/ext
            :select select-tree-bounds-and-value!
            :desc {:fx/type fx/ext-on-instance-lifecycle
                   :on-created init-tree-view!
                   :desc {:fx/type ext-with-selected-path
                          :props {:selected-path (let [[mode path] selection]
                                                   (case mode
                                                     :manual selection
                                                     :auto [mode (-> failure-paths
                                                                     (subseq > path)
                                                                     first
                                                                     (or (last-path output)))]))}
                          :desc {:fx/type fx.ext.tree-view/with-selection-props
                                 :props {:on-selected-item-changed {::event/type ::select
                                                                    :runner runner}}
                                 :desc {:fx/type :tree-view
                                        :cell-factory {:fx/cell-type :tree-cell
                                                       :describe describe-cell}
                                        :on-key-pressed {::event/type ::handle-key-press
                                                         :runner runner}
                                        :fixed-cell-size 20
                                        :show-root false
                                        :root {:fx/type tree-item
                                               :value :root
                                               :children (mapv #(node->tree-item runner %) output)}}}}}})}))

;; region controls

(defn- icon-view [{:keys [icon]}]
  {:fx/type :region
   :style-class "reveal-test-icon"
   :min-width :use-pref-size
   :max-width :use-pref-size
   :min-height :use-pref-size
   :max-height :use-pref-size
   :pseudo-classes #{icon}})

(defn- counter-view [{:keys [count icon color text]}]
  {:fx/type :label
   :style-class "reveal-test-label"
   :pseudo-classes #{color}
   :text (str count)
   :tooltip {:fx/type :tooltip
             :text (str count " " text)}
   :content-display :left
   :graphic {:fx/type icon-view
             :icon icon}})

(defn- open-view-button-view [{:keys [view-id view-index graphic runner]}]
  {:fx/type :button
   :style-class ["button" "reveal-test-open-view-button"]
   :graphic graphic
   :on-action {::event/type :vlaaad.reveal.ui/view
               :view-id view-id
               :view-index view-index
               :form (list 'test-results (:test runner))
               :value {:fx/type result-tree-view
                       :runner runner}}})

(defn controls-view [{:keys [runner open-view-button]}]
  (let [{:keys [state]} runner]
    {:fx/type view/observable-view
     :ref state
     :fn (fn [{:keys [test fail pass running]}]
           (let [color (cond
                         (pos? fail) :error
                         (and (not running) (pos? pass)) :success
                         :else :default)
                 buttons {:fx/type :h-box
                          :alignment :center-left
                          :spacing style/default-padding
                          :children [{:fx/type counter-view
                                      :count fail
                                      :text "failed"
                                      :icon :fail
                                      :color color}
                                     {:fx/type counter-view
                                      :count pass
                                      :text "passed"
                                      :icon :pass
                                      :color color}
                                     {:fx/type counter-view
                                      :count test
                                      :icon :test
                                      :text "tests"
                                      :color color}]}]
             {:fx/type :h-box
              :alignment :center-left
              :children [{:fx/type :button
                          :graphic {:fx/type fx/ext-let-refs
                                    :refs {::icon {:fx/type icon-view
                                                   :icon (if running :running :start)}}
                                    :desc {:fx/type fx/ext-let-refs
                                           :refs {::transition {:fx/type :rotate-transition
                                                                :status (if running :running :stopped)
                                                                :from-angle 0
                                                                :to-angle -360
                                                                :cycle-count :indefinite
                                                                :interpolator :linear
                                                                :duration [1 :s]
                                                                :jump-to (if running :zero [0 :s])
                                                                :node {:fx/type fx/ext-get-ref
                                                                       :ref ::icon}}}
                                           :desc {:fx/type fx/ext-get-ref
                                                  :ref ::icon}}}
                          :on-action {::event/type ::run
                                      :runner runner}}
                         (if open-view-button
                           {:fx/type fx/ext-get-env
                            :h-box/hgrow :always
                            :env {:vlaaad.reveal.ui/id :view-id :vlaaad.reveal.ui/index :view-index}
                            :desc {:fx/type open-view-button-view
                                   :graphic buttons
                                   :runner runner}}
                           (assoc buttons :padding {:left 10 :right 10}))]}))}))

;; endregion

;; simple

(defn runner-view [{:keys [runner]}]
  {:fx/type :v-box
   :children [{:fx/type controls-view
               :open-view-button false
               :runner runner}
              {:fx/type result-tree-view
               :v-box/vgrow :always
               :runner runner}]})

;;easy
(defn test-view [{:keys [test auto-run]
                  :or {test :everything
                       auto-run false}}]
  (let [r (runner test)]
    (when auto-run (test! r))
    {:fx/type runner-view
     :runner r}))

(defn sticker [{:keys [test auto-run]
                :or {test :everything
                     auto-run false}
                :as opts}]
  (let [r (runner test)]
    (when auto-run (test! r))
    (ui/sticker {:fx/type controls-view
                 :open-view-button true
                 :runner r}
                (merge
                  {:title (str "test " test)
                   :default-size {:width 234
                                  :height 46}}
                  (dissoc opts :test :auto-run)))))

(comment
  (sticker {:test '{:exclude [vlaaad.reveal.nrepl]}}))

;; actions

(defn- testable-var? [x]
  (boolean (:test (meta x))))

(action/defaction ::action/test [x]
  (cond
    (and (or (and (simple-symbol? x)
                  (find-ns x))
             (instance? Namespace x))
         (some testable-var? (vals (ns-publics x))))
    #(-> {:fx/type test-view :test x :auto-run true})
    (and (instance? Var x)
         (testable-var? x))
    #(-> {:fx/type test-view :test x :auto-run true})
    (and (qualified-symbol? x)
         (let [var (resolve x)]
           (and var (testable-var? var))))
    #(-> {:fx/type test-view :test x :auto-run true})))