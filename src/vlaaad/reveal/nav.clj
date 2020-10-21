(ns vlaaad.reveal.nav)

(def ^:private vec-conj (fnil conj []))

(defn- grid-conj [grid node]
  (let [n (count grid)]
    (if (zero? n)
      (vec-conj grid [node])
      (update grid (dec n) conj node))))

(defn- last-coordinate [grid]
  [(dec (count grid)) (dec (count (peek grid)))])

(defn add-row [nav parent id]
  (let [grid (-> nav
                 ::id->grid
                 (get parent)
                 (vec-conj [id]))]
    (-> nav
        (assoc-in [::id->grid parent] grid)
        (assoc-in [::id->coordinate id] (last-coordinate grid))
        (assoc-in [::id->parent id] parent))))

(defn add-col [nav parent id]
  (let [grid (-> nav
                 ::id->grid
                 (get parent)
                 (grid-conj id))]
    (-> nav
        (assoc-in [::id->grid parent] grid)
        (assoc-in [::id->coordinate id] (last-coordinate grid))
        (assoc-in [::id->parent id] parent))))

(defn add-cursor [nav id cursor]
  (-> nav
      (update-in [::id->cursor id] #(or % cursor))
      (assoc-in [::cursor->id cursor] id)
      (assoc ::latest-id id)))

(defn ensure-parents [nav parent-ids]
  (loop [nav nav
         ids parent-ids]
    (let [id (peek ids)]
      (if (or (nil? id) (contains? (::id->parent nav) id))
        nav
        (let [ids (pop ids)
              parent (peek ids)]
          (recur (add-row nav parent id) ids))))))

(defn latest-ids [nav]
  (loop [acc nil
         id (::latest-id nav)]
    (if id
      (recur
        (conj acc id)
        ((::id->parent nav) id))
      (vec acc))))

(defn has? [nav id]
  (contains? (::id->parent nav) id))

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
