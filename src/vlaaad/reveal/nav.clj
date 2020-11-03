(ns vlaaad.reveal.nav)

(def ^:private vec-conj (fnil conj []))

(defn- grid-conj [grid node]
  (let [n (count grid)]
    (if (zero? n)
      (vec-conj grid [node])
      (update grid (dec n) conj node))))

(defn- last-coordinate [grid]
  [(dec (count grid)) (dec (count (peek grid)))])

(defn cursor [nav id]
  (get-in nav [::id->cursor id]))

(defn coordinate [nav id]
  (get-in nav [::id->coordinate id]))

(defn parent [nav id]
  (get-in nav [::id->parent id]))

(defn id [nav cursor]
  (get-in nav [::cursor->id cursor]))

(defn grid [nav id]
  (get-in nav [::id->grid id]))

(defn at-last-row? [nav cursor]
  (let [id (id nav cursor)
        parent (parent nav id)
        grid (grid nav parent)
        row ((coordinate nav id) 0)]
    (= row (dec (count grid)))))

(defn add-lines [nav start-y lines]
  (let [id->grid (volatile! (transient (::id->grid nav {})))
        id->coordinate (volatile! (transient (::id->coordinate nav {})))
        id->parent (volatile! (transient (::id->parent nav {})))
        id->cursor (volatile! (transient (::id->cursor nav {})))
        cursor->id (volatile! (transient (::cursor->id nav {})))
        latest-id (volatile! (::latest-id nav))
        latest-ids! (fn []
                      (loop [acc nil
                             id @latest-id]
                        (if id
                          (recur (conj acc id) (@id->parent id))
                          (vec acc))))
        add-row! (fn [parent id]
                   (let [grid (-> @id->grid (get parent) (vec-conj [id]))]
                     (vswap! id->grid assoc! parent grid)
                     (vswap! id->coordinate assoc! id (last-coordinate grid))
                     (vswap! id->parent assoc! id parent)))
        ensure-parents! (fn [parent-ids]
                          (loop [ids parent-ids]
                            (let [id (peek ids)]
                              (if (or (nil? id) (contains? @id->parent id))
                                nil
                                (let [ids (pop ids)]
                                  (add-row! (peek ids) id)
                                  (recur ids))))))
        add-col! (fn [parent id]
                   (let [grid (-> @id->grid (get parent) (grid-conj id))]
                     (vswap! id->grid assoc! parent grid)
                     (vswap! id->coordinate assoc! id (last-coordinate grid))
                     (vswap! id->parent assoc! id parent)))
        add-cursor! (fn [id cursor]
                      (when-not (@id->cursor id)
                        (vswap! id->cursor assoc! id cursor))
                      (vswap! cursor->id assoc! cursor id)
                      (vreset! latest-id id))]
    (reduce-kv
      (fn [_ y line]
        (reduce-kv
          (fn [_ x region]
            (when (:selectable region)
              (let [{:keys [ids start-row]} (:nav region)
                    ids (if (= ids [])
                          (let [ids (latest-ids!)]
                            (if (= ids [])
                              [-1]
                              ids))
                          ids)
                    id (peek ids)
                    parent-ids (pop ids)]
                (ensure-parents! parent-ids)
                (when-not (contains? @id->parent id)
                  ((if start-row add-row! add-col!) (peek parent-ids) id))
                (add-cursor! id [(+ start-y y) x]))))
          nil
          line))
      nil
      lines)
    {::id->grid (persistent! @id->grid)
     ::id->coordinate (persistent! @id->coordinate)
     ::id->parent (persistent! @id->parent)
     ::id->cursor (persistent! @id->cursor)
     ::cursor->id (persistent! @cursor->id)
     ::latest-id @latest-id}))