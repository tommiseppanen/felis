(ns felis.data
  (:require [reagent.core :as reagent :refer [atom]]))

(defonce todos (atom (sorted-map)))
(defonce counter (atom 0))
(defonce current-parent (atom 2))
(defonce current-todo (atom 0))

;; ------------------------------
;; Adding & removing of items

(defn insert-to-vector [coll item pos]
  (vec (concat (subvec coll 0 pos) [item] (subvec coll pos))))

(defn insert-todo!
  "Inserts todo item into specified position of the parent
  childs. Returns the added todo."
  [parent-id name position]
  (let [id (swap! counter inc)
        new-item {:id id :name name :childs [] :done false :state-modified (js/Date)}]
    (do
      (swap! todos assoc id new-item)
      (when (< 0 parent-id)
        (swap! todos update-in [parent-id :childs] insert-to-vector id position))
      new-item)))

(defn add-todo [parent-id name]
  (insert-todo! parent-id name 0))

(defn get-all-childs []
  (let [childs (:childs (get @todos @current-parent))]
    (doall (map #(get @todos %) childs))))

(defn append-empty-todo []
  (insert-todo! @current-parent "" (count (get-all-childs))))

(defn toggle-todo! [id]
  (swap! todos update-in [id :done] not)
  (swap! todos assoc-in [id :state-modified] (js/Date)))

(defn save-todo! [id name]
  (swap! todos assoc-in [id :name] name))

(defn remove-child! [parent child]
  (swap! todos update-in [parent :childs] #(vec (remove #{child} %))))

(defn list-contains? [item list]
  (not (nil? (some #{item} list))))

(defn remove-from-childs [id]
  (let [items (filter
                #(list-contains? id (:childs (second %)))
                @todos)]
    (doseq [single items]
      (remove-child! (:id (second single)) id))))

(defn delete-todo! [id]
  (do
    (remove-from-childs id)
    (swap! todos dissoc id)))

;; ------------------------------
;; Helpers

(defn current-todo-name []
  (:name (get @todos @current-todo)))

(defn current-name-empty? []
  (empty? (current-todo-name)))

(defn set-current-todo! [id]
  (reset! current-todo id))

(defn filter-todos [done]
  (filter #(= done (:done %)) (get-all-childs)))

(defn get-items []
  (filter-todos false))

(defn contains-child? [item child-id]
  (some #(= child-id %) (:childs item)))

(defn get-grand-parent []
  (first (filter #(contains-child? % @current-parent) (vals @todos))))

(defn index-of [id coll]
  (count (take-while #(not= id (:id %)) coll)))

(defn swap-todo-pos! [id direction]
  (let [items (filter-todos false)
        maximum (dec (count items))
        new-position (direction (index-of id items))
        clipped-pos (max (min new-position maximum) 0)
        index (index-of (:id (nth items clipped-pos)) (get-all-childs))]
    (do
      (remove-child! @current-parent id)
      (swap! todos update-in [@current-parent :childs]
             insert-to-vector id index))))

(defn get-done-todos []
  (reverse
    (sort-by :state-modified
             (filter-todos true))))
;; ------------------------------
;; Navigation from parent to childs

(defn focus-first [done-list]
  (if done-list
    (let [done-todos (get-done-todos)]
      (when (not-empty done-todos)
        (set-current-todo! (:id (first done-todos)))))
    (set-current-todo! (:id (first (get-items))))))

(defn drill-up []
  (let [current-id @current-parent
        grand-parent (:id (get-grand-parent))]
    (when (not (nil? grand-parent))
      (do
        (reset! current-parent grand-parent)
        (set-current-todo! current-id)))))

(defn drill-down []
  (do
    (reset! current-parent @current-todo)
    (if (not-empty (get-items))
      (focus-first false)
      (set-current-todo! (:id (add-todo @current-parent ""))))))

;; ------------------------------
;; Navigation between list items

(defn next-item-done [index coll]
  (if (< index 0)
    (append-empty-todo)
    (if (>= index (count coll))
      (nth coll (dec index))
      (nth coll index))))

(defn next-item-not-done [index coll]
  (if (>= index (count coll))
    (append-empty-todo)
    (if (< index 0)
      (add-todo @current-parent "")
      (nth coll index))))

(defn next-item [direction id coll in-done-list]
  (let [index (direction (index-of id coll))]
    (if in-done-list
      (next-item-done index coll)
      (next-item-not-done index coll))))

(defn next-todo-id [direction id]
  (let [done (:done (get @todos id))
        coll (filter-todos done)]
    (:id
      (if done
        (next-item direction id (reverse (sort-by :state-modified coll)) done)
        (next-item direction id coll done)))))
