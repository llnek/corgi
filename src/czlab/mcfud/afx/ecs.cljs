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

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [do-with n# defvoid defvoid-]])

  (:require [czlab.mcfud.afx.core
             :as ec :refer [debug* raise! num??]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newObjectPool

  "Make a new js-object pool."
  [ctor rinse & [batch]]
  {:pre [(fn? ctor)(fn? rinse)]}

  (do-with
    [a (atom {:batch (num?? batch 10) :size 0 :next 0
              :slots #js[] :ctor ctor :rinse rinse})]
    (let
      [g (fn [b sz len]
           (dotimes [_ len]
             (.push b (oset! (ctor)
                             "!____pool" a))) (+ sz len))]
      (swap! a #(merge % {:grow g})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countUsedInPool

  "Get the number of objects currently in used."
  [pool]

  (:next @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sizeOfPool

  "Get the size of the pool."
  [pool]

  (:size @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn takeFromPool!

  "Take a object from the pool.  If no free objects are available,
  the pool will grow in size."
  [pool]

  (let [out (atom nil)]
    (swap! pool
           (fn [{:keys [grow size
                        batch next slots] :as root}]
             (let [n' (+ 1 next)
                   sz (if (< next size)
                        size
                        (grow slots size batch))
                   obj (aget slots next)]
               ;take a free obj, set it's slot, up the pool's free ptr
               (oset! obj "!____status" true)
               (oset! obj "!____slot" next)
               (reset! out obj)
               (merge root
                      {:next n'}
                      (if-not (= sz size) {:size sz})))))
    (deref out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid returnToPool!

  "Return an object back into the pool."
  [pool obj]

  (if (and (some? obj)
           (oget obj "?____status")
           (identical? (oget obj "?____pool") pool))
    ;jiggle the free slot to reuse the one just dropped
    (swap! pool
           (fn [{:keys [rinse next slots] :as root}]
             (let [n' (- next 1)
                   tail (aget slots n')
                   slot' (oget tail "?____slot")
                   epos' (oget obj "?____slot")]
               ;set the free ptr to the dropped, move the tail to old slot
               (rinse obj)
               (aset slots n' obj)
               (aset slots epos' tail)
               ;swap the 2 slots
               (oset! tail "!____slot" epos')
               (oset! obj "!____slot" slot')
               (oset! obj "!____status" false)
               (merge root {:next n'}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newEntityStore

  "Create a new entity-component store."
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
(defn- genUid

  "Get the next entity id."
  [ecs]

  (let [out (atom 0)]
    (swap! ecs
           (fn [{uid :entity-uid :as root}]
             (reset! out uid)
             (assoc root :entity-uid (+ 1 uid)))) (deref out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- retUsed

  "Return object(s) back into the store."
  [obj]

  (cond
    (or (vector? obj)
        (list? obj))
    (doseq [c obj] (retUsed c))
    (map? obj)
    (retUsed (vals obj))
    (object? obj)
    (if-some [p (oget obj "?____pool")] (returnToPool! p obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- remEnt

  "Remove entity by removing all its components."
  [{:keys [data registry] :as root} ents]

  (let
    [dt (loop [dt data
               [k & xs] (keys registry)]
          (if-not (some? k)
            dt
            (let [cOrig (get dt k)
                  ct (loop [ct cOrig
                            [e & xs] ents]
                       (if-not (some? e)
                         ct
                         (recur (if-some [v (get ct e)]
                                  (do (retUsed v)
                                      (dissoc ct e))
                                  ct)
                                xs)))]
              (recur (if (identical? ct cOrig)
                       dt
                       (assoc dt k ct))
                     xs))))
     rt (if (identical? dt data)
          root
          (assoc root :data dt))]
    (assoc rt
           :entities (apply disj (:entities rt) ents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid removeEntity

  "Remove an entity, or entities."
  [ecs entity & more]

  (swap! ecs #(remEnt % (concat [entity] more))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid addComponent

  "Add a component definition, or definitions."
  [ecs id component & more]

  (swap! ecs
         (fn [root]
           (->> (concat [id component] more)
                (partition 2)
                (map (fn [a] (vec a)))
                (vec)
                (into {})
                (merge (:registry root))
                (assoc root :registry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid removeComponent

  "Remove a component, or components.  All existing instances are purged."
  [ecs id & more]

  (swap! ecs
         (fn [{:keys [data registry] :as root}]
           (let [cids (concat [id] more)]
             (doseq [c cids
                     :let [v (get data c)]
                     :when (some? v)] (retUsed v))
             (-> root
                 (assoc :data (apply dissoc data cids))
                 (assoc :registry (apply dissoc registry cids)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid addToEntity!

  "Add a component to an entity, or components."
  [ecs entity componentDecl & moreDecls]

  (swap! ecs
         (fn [{:keys [data registry] :as root}]
           (->> (loop [dtree data
                       [dc & xs]
                       (concat [componentDecl] moreDecls)]
                  (if-not (some? dc)
                    dtree
                    (let [[cid & args] dc
                          ctor (get registry cid)]
                      (assert (fn? ctor)
                              (str "Unknown component: " cid))
                      (let [co (-> (apply ctor args)
                                   (oset! "!____entity" entity))]
                        (recur (update-in dtree
                                          [cid]
                                          #(assoc % entity co))
                               xs)))))
                (assoc root :data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newEntity

  "Create a new entity with this component, or components."
  [ecs componentDecl & moreDecls]

  (do-with [entity (genUid ecs)]
    (apply addToEntity!
           (concat [ecs entity componentDecl] moreDecls))
    (swap! ecs
           #(update-in %
                       [:entities]
                       (fn [c] (conj c entity))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid removeFromEntity!

  "Remove component from entity, or components."
  [ecs entity & componentIds]

  (swap! ecs
         (fn [{:keys [data] :as root}]
           (let
             [dt (loop [dtree data
                        [cid & xs] componentIds]
                   (if-not (some? cid)
                     dtree
                     (let [co (get dtree cid)]
                       (recur (if-some [v (if (some? co)
                                          (get co entity))]
                                (do (retUsed v)
                                    (update-in dtree
                                               [cid] #(dissoc % entity)))
                                dtree)
                              xs))))]
             (if (identical? dt data) root (assoc root :data dt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getEntityData

  "Get the component data belonging to this entity."
  [ecs entity componentId]

  (let [{:keys [data]} @ecs]
    (if-some [ct (get data componentId)] (get ct entity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateEntity

  "Apply update function to the component data in this entity."
  [ecs entity componentId func]
  {:pre [(fn? func)]}

  (when-some [c (getEntityData ecs entity componentId)] (func c) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentsData

  "Get all instances of this component."
  [ecs componentId]

  (let [{:keys [data]} @ecs]
    (if-some [c (get data componentId)] (vals c) [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentKeys

  "List all component-ids."
  [ecs]

  (keys (:registry @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findComponent

  "Find the component definition."
  [ecs componentId]

  (get (:registry @ecs) componentId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn componentInEntity?

  "True if entity has this component, or components."
  [ecs entity componentId & moreIds]

  (let [{:keys [data]} @ecs]
    (every? #(if-some
               [co (get data %)]
               (contains? co entity))
            (concat [componentId] moreIds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findEntities

  "Find all entities with this component, or components."
  [ecs componentId & moreIds]

  (let [{:keys [data]} @ecs
        ret (array)
        cids (concat [componentId] moreIds)]
    (when (every? #(contains? data %) cids)
      (let
        [ccs (sort #(cond
                      (< (first %1) (first %2)) -1
                      (> (first %1) (first %2)) 1 :else 0)
                   (mapv #(let [v (get data %)] [(n# v) v]) cids))
         ccsz (n# ccs)
         [_ c0] (if (pos? ccsz) (first ccs))]
        ;;use the shortest cache as the baseline
        (when (some? c0)
          (doseq [eid (keys c0)
                  :let [sum (atom 0)]]
            ;; look for intersection
            (doseq [c ccs]
              (if (or (identical? c c0)
                      (contains? c eid))
                (swap! sum inc)))
            ;; if found in all caches...
            (if (= @sum ccsz)
              (.push ret eid))))))
    (vec ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid addTemplate

  "Add a entity template."
  [ecs id template & more]

  (swap! ecs
         #(->> (concat [id template] more)
               (partition 2)
               (mapv (fn [a] (vec a)))
               (into {})
               (merge (:templates %))
               (assoc % :templates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTemplateKeys

  "List all template-ids."
  [ecs]

  (keys (:templates @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findTemplate

  "Find this template."
  [ecs templateId]

  (get (:templates @ecs) templateId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid removeTemplate

  "Remove this template, or templates."
  [ecs id & moreIds]

  (swap! ecs
         #(assoc %
                 :templates
                 (apply dissoc
                        (concat [(:templates %) id] moreIds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newTemplateEntity

  "Create a entity from this template."
  [ecs id]

  (let [{:keys [templates]} @ecs]
    (when-some [t (get templates id)]
      (let [{:keys [components initor]} t]
        (do-with [e (apply newEntity
                           (concat [ecs] components))]
                 (if (fn? initor) (initor ecs e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addSystem

  "Add a new system, or systems."
  [ecs system & more]

  (swap! ecs
         #(assoc %
                 :systems
                 (vec (concat (:systems %) [system] more)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid removeSystem!

  "Remove a system, or systems."
  [ecs system & more]

  (let [yy (concat [system] more)]
    (swap! ecs
           (fn [root]
             (->> (:systems root)
                  (filterv (fn [s]
                             (not-any?
                               #(identical? % s) yy)))
                  (assoc root :systems))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid run

  "Run all systems."
  [ecs dt]

  (doseq [s (:systems @ecs)] (s ecs dt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


