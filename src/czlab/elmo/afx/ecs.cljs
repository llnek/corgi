;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}
  czlab.elmo.ecs.core
  (:require ["kirby" :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createPool "" [ctor rinse batch]
  {:pre [(fn? ctor)(fn? rinse)]}
  (let [a (atom {:batch batch :size 0
                 :next 0 :slots #js[]
                 :ctor ctor :rinse rinse})
        g #(dotimes [_ batch]
             (.push % (oset! (ctor)
                             "____pool" a)))]
    (swap! a #(merge % {:grow g}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPoolSize "" [pool] (:size @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPoolUsed "" [pool] (:next @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn takeFromPool! "" [pool]
  (let [out #js[]]
    (->>
      (fn [{:keys [grow size next slots] :as root}]
        (let [next1 (inc next)
              [sz _]
              (if (>= next size)
                [(+ size (:batch root)) (grow slots)]
                [size slots])]
          ;take a free object, set it's slot,
          ;up the pool's free ptr
          (.push out (nth slots next))
          (doto (aget out 0)
            (oset! "____slot" next)
            (oset! "____status" true))
          (merge root
                 (if (= sz size)
                   {:next next1}
                   {:next next1 :size sz}))))
      (swap! pool))
    (aget out 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn returnToPool! "" [pool obj]
  (if (and (some? obj)
           (oget obj "____status")
           (identical? obj.____pool pool))
    ;jiggle the free slot to reuse the one just dropped
    (->>
      (fn [{:keys [rinse next slots] :as root}]
        (let [next1 (dec next)
              _ (rinse obj)
              tail (aget slots next1)
              slot' (oget tail "____slot")
              epos' (oget obj "____slot")]
          ;set the free ptr to the dropped
          ;move the tail to old slot
          (aset slots next1 obj)
          (aset slots epos' tail)
          ;swap the 2 slots
          (oset! tail "____slot" epos')
          (oset! obj "____slot" slot')
          (oset! obj "____status" false)
          (merge root {:next next1})))
      (swap! pool))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createECS "" []
  (atom {:entities #{}
         :templates {}
         :registry {}
         :data {}
         :systems [] :uid 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- genUid "" ^String [ecs]
  (with-local-vars [ret 0])
  (swap! ecs
         (fn [{:keys [uid] :as root}]
           (set! ret uid)
           (assoc! root :uid (+1 uid)))) (str "e@" ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- retUsed "" [obj]
  (cond
    (objectMap? obj)
    (retUsed (vals obj))
    (array? obj)
    (each (fn [c]
            (if (and c c.____pool)
              (returnToPool c.____pool c))) obj)
    (object? obj)
    (if obj.____pool
      (returnToPool obj.____pool obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createEntity "" [ecs componentDecl & moreDecls]
  (dosync
    (with-local-vars [entity (genUid ecs)])
    (apply addToEntity
           this
           (concat [ecs entity] componentDecl moreDecls))
    (swap! ecs
           (fn [{:keys [entities] :as root}]
             (conj! entities entity) root)) entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeEntity "" [ecs entity & more]
  (with-local-vars [c ? ents (concat [entity] more)])
  (swap! ecs
         (fn [{:keys [entities
                      data registry] :as root}]
           (each
             #(if-some [co (get data %)]
                (each (fn [e]
                        (retUsed (get co e))
                        (dissoc! co e)) ents)) (keys registry))
           (each #(disj! entities %) ents) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addComponent "" [ecs id component & more]
  (swap! ecs
         (fn [{:keys [registry] :as root}]
           (each (fn [[a b]]
                   (assoc! registry a b))
                 (partition 2 (concat [id component] more))) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeComponent "" [ecs id & more]
  (swap! ecs
         (fn [{:keys [data registry] :as root}]
           (each (fn [c]
                   (dissoc! registry c)
                   (retUsed (get data c))
                   (dissoc! data c)) (concat [id] more)) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addToEntity "" [ecs entity componentDecl & moreDecls]
  (locals me this)
  (swap! ecs
         (fn [{:keys [data registry] :as root}]
           (with-local-vars [ctor nil co nil])
           (each (fn [[cid & xs]]
                   (set! ctor (get registry cid))
                   (if-not ctor (raise! "Unknown component " cid))
                   (if-not (.has data cid) (assoc! data cid {}))
                   (set! co (apply ctor me xs))
                   (oset! co :____entity entity)
                   (assoc! (get data cid) entity co))
                 (concat [componentDecl] moreDecls)) root)) entity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeFromEntity "" [ecs entity & componentIds]
  (swap! ecs
         (fn [{:keys [data] :as root}]
           (each #(when-some [co (get data %)]
                    (retUsed (get co entity))
                    (dissoc! co entity)) componentIds) root)) entity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getEntityData "" [ecs entity componentId]
  (with-local-vars [d (get @ecs :data)
                    c (get d componentId)])
  (if c (get c entity) undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateEntity "" [ecs entity componentId func]
  (swap! ecs
         (fn [{:keys [data] :as root}]
           (if-some [c (getEntityData
                         ecs entity componentId)] (func c)) root)) entity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentsData "" [ecs componentId]
  (with-local-vars [d (get @ecs :data)])
  (if-some [c (get d componentId)] (vals c) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentKeys "" [ecs] (keys (get @ecs :registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findComponent
  "" [ecs componentId] (get (get @ecs :registry) componentId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn componentInEntity? "" [ecs entity componentId & moreIds]
  (with-local-vars [d (get @ecs :data)])
  (not (some #(if-some [co (get d %)]
                (not (.has co entity)) #t)
             (concat [componentId] moreIds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findEntities "" [ecs componentId & moreIds]
  (with-local-vars [pmks ? ccsz ? eid ?
                    cnt ? c ? sum ?
                    {:keys [data entities]} (deref ecs)
                    pmin MAX-INT
                    missed? #f pm nil ccs [] ret []
                    cids (concat [componentId] moreIds)])
  ;;find shortest cache, doing an intersection
  (floop [cid cids]
    (set! c (get data cid)
          cnt (count c))
    (if (nichts? c)
      (do->break! (true! missed?))
      (do (when (< cnt pmin)
            (set! pmin cnt pm c))
          (conj! ccs c))))
  ;;use the shortest cache as the baseline
  (set! ccsz (n# ccs))
  (when (and (> ccsz 0)
             (not missed?))
    (set! pmks (keys pm))
    (doseq [eid pmks
            :let [sum 0]]
      ;; look for intersection
      (doseq [c ccs]
        (if (= c pm)
          (++ sum)
          (if (.has c eid) (++ sum))))
      ;; if found in all caches...
      (if (= sum ccsz) ;; all matched
        (conj! ret e)))) ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addTemplate "" [ecs id template & more]
  (swap! ecs
         (fn [{:keys [templates] :as root}]
           (each (fn [[a b]]
                   (assoc! templates a b))
                 (partition 2
                            (concat [id template] more))) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTemplateKeys "" [ecs] (keys (get @ecs :templates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findTemplate
  "" [ecs templateId] (get (get @ecs :templates) templateId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeTemplate "" [ecs id & moreIds]
  (swap! ecs
         (fn [{:keys [templates] :as root}]
           (each #(dissoc! templates %)
                 (concat [id] moreIds)) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createTemplateEntity "" [ecs id]
  (with-local-vars [me this entity nil])
  (swap! ecs
         (fn [{:keys [templates] :as root}]
           (with-local-vars [t (get templates id)])
           (set! entity
                 (apply createEntity
                        me
                        (concat [ecs] t.components)))
           (if (fn? t.initor)
             (t.initor ecs entity)) root)) entity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addSystem "" [ecs system & more]
  {:pre [(fn? system)]}
  (swap! ecs
         (fn [{:keys [systems] :as root}]
           (each #(conj! systems %)
                 (concat [system] more)) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeSystem "" [ecs system & more]
  (swap! ecs
         (fn [{:keys [systems] :as root}]
           (each #(with-local-vars
                    [i (.indexOf systems %)]
                    (if-not (neg? i) (.splice systems i 1)))
                 (concat [system] more)) root)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [ecs dt]
  (each #(% ecs dt) (get @ecs :systems)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


