(ns felis.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [cljsjs.react :as react]
            [felis.data :as data])
  (:import goog.History))

;; ------------------------------

(defn log-this [value]
  (.log js/console value))

(defn get-parsed-value [value]
  (-> value str))

(defn todo-input [{:keys [name done last-not-done on-save on-stop on-move on-delete
                          on-add-new on-swap on-toggle]}]
  (let [old-value (atom name)
        jump #(do (on-delete) (data/focus-first true))
        move #(if (data/current-name-empty?)
               (do (on-move %) (on-delete))
               (on-move %))
        jump-to-done? #(and last-not-done
                            (data/current-name-empty? )
                            (not-empty (data/get-done-todos)))
        navigate-down #(if (jump-to-done?) (jump) (move inc))
        cancel #(do
                 (if (empty? @old-value) (on-delete) (on-save @old-value))
                 (on-stop))
        toggle #(if done (on-toggle) (do (on-move inc) (on-toggle)))]
    (fn []
      [:input {:value       (data/current-todo-name)
               :on-change   #(on-save (get-parsed-value (-> % .-target .-value)))
               :placeholder "Enter task (Esc cancels)"
               :on-key-down #(let [ctrl (.-ctrlKey %)
                                   shift (.-shiftKey %)
                                   alt (.-altKey %)
                                   ctrl-shift (and shift ctrl)
                                   ctrl-alt (and alt ctrl)]
                              (case (.-which %)
                                13 (if ctrl (toggle) (when-not done (on-add-new)))
                                27 (cancel)
                                37 (when ctrl-alt (data/drill-up))
                                38 (if ctrl-shift (when-not done (on-swap dec)) (move dec))
                                39 (when ctrl-alt (data/drill-down))
                                40 (if ctrl-shift (when-not done (on-swap inc)) (navigate-down))
                                89 (when (and ctrl (not done)) (do (on-move inc) (on-delete)))
                                36 (when ctrl (data/focus-first false))
                                nil))}])))

(def todo-edit (with-meta todo-input
                          {:component-did-mount #(.focus (reagent/dom-node %))}))

;; ------------------------------

(defn last-of-not-done? [id]
  (let [not-done-todos (data/filter-todos false)
        index (data/index-of id not-done-todos)
        item-count (count not-done-todos)]
    (>= index (dec item-count))))

(defn todo-item []
  (let []
    (fn [{:keys [id name]}]
      [:li
       (when (not= @data/current-todo id)
         [:div.view
          [:label {:on-click #(data/set-current-todo! id)} name]])
       (when (= @data/current-todo id)
         [todo-edit {:name             name
                     :done             (:done (get @data/todos id))
                     :last-not-done    (last-of-not-done? id)
                     :on-swap          #(data/swap-todo-pos! id %)
                     :on-save          #(data/save-todo! id %)
                     :on-delete        #(data/delete-todo! id)
                     :on-add-new       #(data/set-current-todo!
                                         (let [index (inc (data/index-of id (data/get-all-childs)))]
                                           (:id (data/insert-todo! @data/current-parent "" index))))
                     :on-move          #(data/set-current-todo! (data/next-todo-id % id))
                     :on-toggle        #(data/toggle-todo! id)
                     :on-stop          #(data/set-current-todo! 0)}])])))

(defn lister [items]
  [:ol
   (for [item items]
     (let [id (:id item)]
       ^{:key id}
       [todo-item item]))])

(defn home-page []
  (fn []
    (let [items (data/get-items)]
      [:div [:h2 (:name (get @data/todos @data/current-parent))]
       [:div
        [:input {:type     "submit" :value "Add"
                 :on-click #(data/set-current-todo! (:id (data/add-todo @data/current-parent "")))}]
        [lister items]]
       [:div [:h3 "Done"]
        [lister (data/get-done-todos)]]])))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
                    (session/put! :current-page #'home-page))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn create-defaults []
  (data/add-todo 0 "Default project")
  (data/add-todo 1 "Group 1")
  (data/add-todo 2 "Task 1"))

(defn init! []
  (hook-browser-navigation!)
  (mount-root)
  (create-defaults)
  (data/focus-first false))
