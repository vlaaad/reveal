(ns e03-chess-server-popups
  (:require [vlaaad.reveal.ext :as rx]))

;; We are developing a server for chess Battle Royale where each player plays its own
;; piece. During development we want to be able to see every game on the server as
;; checkered board, and we want to be able to inspect every piece's state if necessary

(def server-state ;; room -> board
  (atom {1 {[0 4] {:username "alice"
                   :color :white
                   :piece :queen}
            [2 2] {:username "bob"
                   :color :white
                   :piece :knight}
            [4 6] {:username "cecilia"
                   :color :black
                   :piece :rook}
            [3 0] {:username "david"
                   :color :black
                   :piece :knight}}
         2 {[1 2] {:username "eva"
                   :color :white
                   :piece :king}
            [3 4] {:username "greg"
                   :color :white
                   :piece :pawn}
            [4 5] {:username "hilda"
                   :color :black
                   :piece :rook}
            [2 7] {:username "ian"
                   :color :black
                   :piece :bishop}}}
        :meta {::server true}))

;; build a view for the server state

(def piece->symbol
  {:queen "♛"
   :king "♚"
   :rook "♜"
   :bishop "♝"
   :knight "♞"
   :pawn "♟"})

(defn grid [children]
  {:fx/type :grid-pane
   :hgap 1
   :vgap 1
   :column-constraints (repeat 8 {:fx/type :column-constraints
                                  :halignment :center
                                  :min-width 30})
   :row-constraints (repeat 8 {:fx/type :row-constraints
                               :valignment :center
                               :min-height 30})
   :children children})

(rx/defaction ::boards [x]
  (when (::server (meta x))
    #(rx/view-as-is
       {:fx/type rx/observable-view
        :ref x
        :fn (fn [state]
              {:fx/type :scroll-pane
               :fit-to-width true
               :content
               {:fx/type :flow-pane
                :hgap 5
                :vgap 5
                :children
                (for [[id board :as id+board] (sort-by key state)]
                  {:fx/type :v-box
                   :children
                   [{:fx/type rx/popup-view
                     :value id+board
                     :desc {:fx/type :label
                            :text (str "board #" id)}}
                    {:fx/type :stack-pane
                     :children
                     [(grid
                        (for [x (range 8)
                              y (range 8)]
                          {:fx/type :region
                           :grid-pane/column x
                           :grid-pane/row y
                           :style {:-fx-background-color (if (even? (+ x y)) "#888" "#999")}}))
                      (grid (for [[[x y] player :as coordinate+player] board]
                              {:fx/type rx/popup-view
                               :grid-pane/column x
                               :grid-pane/row y
                               :value coordinate+player
                               :desc {:fx/type :label
                                      :style {:-fx-font-size 20
                                              :-fx-text-fill (:color player)}
                                      :text (piece->symbol (:piece player))}}))]}]})}})})))

;; game logic

(defn move [state board-id from to]
  (let [from-player (get-in state [board-id from])]
    (when-not from-player
      (throw (ex-info "No one to move" {:state state :board-id board-id :from from})))
    (-> state
        (update board-id dissoc from)
        (assoc-in [board-id to] from-player))))

(defn end [state board-id]
  (dissoc state board-id))

;; REPL helpers

(defn move! [board-id from to]
  (swap! server-state move board-id from to))

(defn end! [board-id]
  (swap! server-state end board-id))

(comment
  ;; evaluate server state and select "boards" action
  server-state
  ;; At this point you should see 2 chess boards that reflect server state.
  ;; You can use Tab to move focus between pieces and board headers.
  ;; Pressing Space or Enter will open a popup. Alternatively, you can
  ;; right-click on a player's piece or a header to request a popup.

  ;; try moving a figure
  (move! 1 [2 2] [3 0])
  ;; try closing another game
  (end! 2))
