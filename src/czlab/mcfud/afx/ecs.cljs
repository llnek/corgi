;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.ecs

  (:require [czlab.mcfud.afx.core
             :as c :refer [do-with do->nil do-with-atom
                           n# _1 _2 fn_1 in? =? cc+ cc+1 raise! num??]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-jspool
  "New js-object pool.  Each object has a property
  that points back to the parent - pool object.
  The caller needs to provide 2 functions -
  1. constructor - how to create new objects
  2. rinse - upon reclaiming the object, need to clean up object.
  Each object is injected with a back-ptr back to the pool."
  [ctor rinse & [batch]]
  {:pre [(fn? ctor)(fn? rinse)]}
  (do-with [p (atom {:batch (num?? batch 10)
                     :ctor ctor :rinse rinse
                     :size 0 :next 0 :slots #js[]})]
    (swap! p
           #(assoc %
                   :grow
                   (fn [pool]
                     (let [{:keys [next size
                                   batch slots]} @pool
                           size'
                           (if (< next size)
                             size
                             (do (dotimes [_ batch]
                                   (.push slots (oset! (ctor)
                                                       "!____pool" pool)))
                                 (+ size batch)))]
                       [(aget slots next) (+ 1 next) size']))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pool-count
  "Count used objects." [pool] (:next @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pool-size
  "Size of pool." [pool] (:size @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pool-take!
  "Take a object from the pool.  If there is no free object,
  the pool will grow in size."
  [pool]
  (do-with-atom [out (atom nil)]
    (swap! pool
           #(let [{:keys [grow]} %
                  [obj n' z'] (grow pool)]
              ;take a free obj, set it's slot,
              ;up the pool's free ptr
              (oset! obj "!____slot" (- n' 1))
              (oset! obj "!____status" true)
              (reset! out obj)
              (assoc % :next n' :size z')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pool-drop!
  "Put object back into the pool."
  [pool obj]
  (do-with [pool]
    (if (and (some? obj)
             (oget obj "?____status")
             (=? (oget obj "?____pool") pool))
      ;jiggle the free slot to reuse the one just dropped
      (swap! pool
             (fn [{:keys [rinse next slots] :as root}]
               (let [n' (- next 1)
                     tail (aget slots n')
                     slot' (oget tail "?____slot")
                     epos' (oget obj "?____slot")]
                 ;set free ptr to dropped,
                 ;move the tail to old slot
                 (if (fn? rinse) (rinse obj))
                 (aset slots n' obj)
                 (aset slots epos' tail)
                 ;swap the 2 slots
                 (oset! tail "!____slot" epos')
                 (oset! obj "!____slot" slot')
                 (oset! obj "!____status" false)
                 (assoc root :next n')))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn new-entity-store
    "Create a new entity-component store. Entities are
    just labels, not objects, such as strings or numbers.
    In this case, we use a sequence number.
    Components are just simple js data objects, no methods.
    Each component is registered by an id and a function
    returning that data object, such as
    :person-name (fn [] #js {:name \"\" :lastname \"\"}).
    Instances of each component-type are stored in its
    own object-pool.
    Optionally, a rinse function is provided to clean up
    the object for reuse.
    A registry is used to keep track of all component
    types - :registry {:c1 c1ctor :c2 c2ctor ...}
    A tree is used to store all component instances -
    :data {:c1 {1 i1 2 i2 ...} :c4 {5 i1 9 i2} ...}.
    Templates can be added to predefine an entity such
    as :t1 {:components [:c4 :c8 :c9]}
    :t3 {:components [:c6 :c2]}.
    For runtime, systems can be added to manipulate
    entities and components."
    []
    (atom {;list of entities
           :entities #{}
           ;component templates
           :templates {}
           ;component types
           :registry {}
           ;component instances
           :data {}
           ;run time systems
           :systems []
           ;entity identity
           :entity-uid 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gen-uid
  "Next entity id."
  [ecs]
  (do-with-atom [out (atom 0)]
    (swap! ecs
           (fn [{uid :entity-uid :as root}]
             (reset! out uid)
             (assoc root :entity-uid (+ 1 uid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- retused
  "Put object(s) back into the store."
  [obj]
  (cond (or (list? obj)
            (vector? obj))
        (doseq [c obj] (retused c))
        (map? obj)
        (retused (vals obj))
        (object? obj)
        (when-some [p (oget obj "?____pool")] (pool-drop! p obj) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rement
  "Remove entity and all its components."
  [data entities ent]
  (loop [data' data
         [cid & cs] (keys data')]
    (if (nil? cid)
      [data' (disj entities ent)]
      (let [d (get data' cid)]
        (recur (if-some [e (get d ent)]
                 (do (retused e)
                     (update-in data' [cid] #(dissoc % ent))) data') cs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove-entity!
  "Remove an entity, or entities."
  [ecs entity & more]
  (do-with [ecs]
    (let [{:keys [data entities]} @ecs]
      (loop [data' data
             ents entities
             [e & es] (cc+1 entity more)]
          (if (nil? e)
            (swap! ecs #(assoc %
                               :data data' :entities ents))
            (let [[dt' es']
                  (rement data' ents e)] (recur dt' es' es)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-component!
  "Add a component definition, or definitions."
  [ecs id funcs & more]
  {:pre [(sequential? funcs)]}
  (do-with [ecs]
    (swap! ecs
           (fn [{:keys [registry data] :as root}]
             (->> (cc+ [id funcs] more)
                  (partition 2)
                  (map (fn [[k fs]]
                         (vector k
                                 (new-jspool (_1 fs)
                                             (or (_2 fs) identity)))))
                  (into {})
                  (merge registry)
                  (assoc root :registry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove-component!
  "Remove a component, or components.  All existing instances are purged."
  [ecs cid & more]
  (do-with [ecs]
    (swap! ecs
           (fn [{:keys [data registry] :as root}]
             (let [cids (cc+1 cid more)]
               (doseq [c cids
                       :let [v (get data c)]
                       :when (some? v)] (retused v))
               (assoc root
                      :data (apply dissoc data cids)
                      :registry (apply dissoc registry cids)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add-to-entity
  "Add a component to an entity, or components."
  [root entity cids]
  (loop [[c & cs] cids
         data (:data root)
         rego (:registry root)]
    (if (nil? c)
      (assoc root :data data)
      (let [r (get rego c)]
        (assert (some? r)
                (str "Unknown component: " c))
        (recur cs
               (update-in data
                          [c]
                          #(assoc %
                                  entity (pool-take! r))) rego)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add->entity!
  "Add a component to an entity, or components."
  [ecs entity cid & more]
  (do-with [ecs]
    (swap! ecs #(add-to-entity % entity (cc+1 cid more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-entity!
  "Create a new entity with this component, or components."
  [ecs & cids]
  (do-with [entity (gen-uid ecs)]
    (swap! ecs
           (fn [root]
             (-> (update-in root
                            [:entities]
                            #(conj % entity))
                 (add-to-entity entity cids))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove->entity!
  "Remove component from entity, or components."
  [ecs entity cid & more]
  (do-with [ecs]
    (swap! ecs
           (fn [{:keys [data] :as root}]
             (loop [[c & cs] (cc+1 cid more) data' data]
               (if (nil? c)
                 (assoc root :data data')
                 (recur cs
                        (if-some [v (get (get data' c) entity)]
                          (do (retused v)
                              (update-in data'
                                         [c]
                                         #(dissoc % entity))) data'))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-entity-data
  "Get the component data."
  [ecs entity cid]
  (let [{:keys [data]} @ecs]
    (get (get data cid) entity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update-entity!
  "Apply function to the component data."
  [ecs entity cid func]
  {:pre [(fn? func)]}
  (when-some [c (get-entity-data ecs entity cid)] (func c) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-components-data
  "Get all component data."
  [ecs cid]
  (let [{:keys [data]} @ecs]
    (if-some [c (get data cid)] (vals c) [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-component-keys
  "List all component-ids."
  [ecs] (keys (:registry @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-component
  "Find the component definition."
  [ecs cid] (get (:registry @ecs) cid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn component-in-entity?
  "If entity has this component, or components."
  [ecs entity cid & more]
  (let [{:keys [data]} @ecs]
    (every? #(if-some
               [co (get data %)]
               (in? co entity)) (cc+1 cid more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-entities
  "Find all entities with this component, or components."
  [ecs cid & more]
  (let [{:keys [data]} @ecs cids (cc+1 cid more)]
    ;make sure all components are registered
    (if (every? #(in? data %) cids)
      (let [ccs (sort (c/compare-asc* #(count %))
                      (mapv #(get data %) cids))
            c0 (_1 ccs)
            ccsz (n# ccs)]
        ;;find the smallest data tree via sort
        (loop [[eid & es] (keys c0) ret (c/tvec*)]
          (if (nil? eid)
            (c/ps! ret)
            ;; look for intersection
            (recur es
                   (loop [[c & cs] ccs arr ret sum 0]
                     (if (nil? c)
                       ;; if found in all caches...
                       (if (= sum ccsz) (conj! arr eid) arr)
                       (recur cs
                              arr
                              (if (or (=? c c0)
                                      (in? c eid)) (+ 1 sum) sum)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-template!
  "Add a entity template."
  [ecs id template & more]
  (do-with [ecs]
    (swap! ecs
           (fn [root]
             (update-in root
                        [:templates]
                        (fn [t]
                          (->> (cc+ [id template] more)
                               (partition 2)
                               (mapv #(vec %))
                               (into {})
                               (merge t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-template-keys
  "List all template-ids."
  [ecs] (keys (:templates @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-template
  "Find this template."
  [ecs tid]
  (get (:templates @ecs) tid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove-template!
  "Remove this template, or templates."
  [ecs tid & more]
  (do-with [ecs]
    (swap! ecs
           (fn [{:keys [templates] :as root}]
             (update-in root
                        [:templates]
                        #(apply dissoc % (cc+1 tid more)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-template-entity
  "Create a entity from this template."
  [ecs tid & [initor]]
  (if-some [t (get (:templates @ecs) tid)]
    (do-with
      [e (apply new-entity!
                (cc+1 ecs (:components t)))] (if (fn? initor) (initor e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn push-system!
  "Add a new system, or systems."
  [ecs system & more]
  (do-with [ecs]
    (swap! ecs
           #(assoc %
                   :systems
                   (vec (cc+ (:systems %) [system] more))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-system!
  "Remove a system, or systems."
  [ecs system & more]
  (do-with [ecs]
    (swap! ecs
           (fn [{:keys [systems] :as root}]
             (let [yy (cc+1 system more)]
               (assoc root
                      :systems
                      (filterv (fn [s]
                                 (not-any? #(=? % s) yy)) systems)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-all-systems
  "Run all systems."
  [ecs dt]
  (doseq [s (:systems @ecs)] (s ecs dt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


