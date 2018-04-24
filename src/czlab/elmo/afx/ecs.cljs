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
(defn- genUid "" [ecs]
  (let [out #js[]]
    (swap! ecs
           (fn [{:keys [uid] :as root}]
             (.push out uid)
             (assoc root :uid (inc uid))))
    (str "e@" (aget out 0))))

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
  (let [entity (genUid ecs)]
    (apply addToEntity
           (concat [ecs entity componentDecl] moreDecls))
    (swap! ecs
           #(update-in %
                       [:entities]
                       (fn [c] (conj c entity)))) entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- remEnt "" [root ents]
  (let
    [[dt? dt]
     (loop [dtreeChgCnt 0
            dtree (:data root)
            [k & xs] (keys (:registry root))]
       (if-not (some? k)
        [(pos? dtreeChgCnt) dtree]
        (let
          [[ct? ct]
           (loop [ctreeCngCnt 0
                  ctree (get dtree k)
                  [e & xs] ents]
             (if-not (some? e)
               [(pos? ctreeCngCnt) ctree]
               (let
                 [c (get ctree e)]
                 (if (some? c) (retUsed c))
                 (recur
                   (if (some? c)
                     (inc ctreeChgCnt) ctreeChgCnt)
                   (if (some? c)
                     (dissoc ctree e) ctree) xs))))]
          (recur
            (if ct?
              (inc dtreeCngCnt) dtreeCngCnt)
            (if ct?
              (assoc dtree k ctree) dtree) xs))))
     r0 (if dt?
          (assoc root :data dt) root)]
    (assoc r0
           :entities
           (apply disj (:entities r0) ents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeEntity "" [ecs entity & more]
  (swap! ecs
         #(remEnt %1
                  (concat [entity] more))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addComponent "" [ecs id component & more]
  (swap!
    ecs
    (fn [root]
      (assoc root
             :registry
             (loop [rego (:registry root)
                    [kv & xs]
                    (partition 2
                               (concat [id component] more))]
               (if-not (some? kv)
                 rego
                 (recur (assoc rego
                               (first kv)
                               (last kv)) xs))))))
  ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeComponent "" [ecs id & more]
  (swap!
    ecs
    (fn [{:keys [data registry] :as root}]
      (let [cids (concat [id] more)]
        (doseq [c cids
                :let [v (get data c)]
                :when (some? v)] (retUsed v))
        (-> root
            (assoc :data (apply dissoc data cids))
            (assoc :registry (apply dissoc registry cids))))))
  ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addToEntity "" [ecs entity componentDecl & moreDecls]
  (swap!
    ecs
    (fn [{:keys [registry] :as root}]
      (assoc root
             :data
             (loop [dtree (:data root)
                    [dc & xs]
                    (concat [componentDecl] moreDecls)]
               (if-not (some? dc)
                 dtree
                 (let
                   [[cid & args] dc
                    ctor (get registry cid)
                    _ (if-not (fn? ctor)
                        (raise! "Unknown component " cid))
                    co (apply ctor args)
                    _ (oset! co "____entity" entity)]
                   (recur
                     (update-in dtree
                                [cid]
                                #(assoc % entity co)) xs)))))))
  ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeFromEntity "" [ecs entity & componentIds]
  (swap!
    ecs
    (fn [{:keys [data] :as root}]
      (let
        [[dt? dt]
         (loop [touchedCnt 0
                dtree (:data root)
                [cid & xs] componentIds]
           (if-not (some? cid)
             [(pos? touched) dtree]
             (let [co (get dtree cid)
                   v (if (some? co) (get co entity))]
               (if (some? v) (retUsed v))
               (recur
                 (if (some? v)
                   (inc touchedCnt) touchedCnt)
                 (if (some? v)
                   (update-in dtree
                              [cid]
                              #(dissoc % entity))
                   dtree)
                 xs))))]
        (if dt?
          (assoc root :data dt) root))))
  ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getEntityData "" [ecs entity componentId]
  (if-some
    [ct (-> (:data @ecs)
            (get componentId))] (get ct entity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateEntity "" [ecs entity componentId func]
  (let [c (getEntityData
            ecs entity componentId)]
    (if (some? c) (func c)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentsData "" [ecs componentId]
  (if-some [c (-> (:data @ecs)
                  (get componentId))] (vals c) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentKeys "" [ecs] (keys (:registry @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findComponent
  "" [ecs componentId] (get (:registry @ecs) componentId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn componentInEntity?
  "" [ecs entity componentId & moreIds]
  (let [d (:data @ecs)]
    (not (some #(if-some [co (get d %)]
                  (not (contains? co entity)) true)
               (concat [componentId] moreIds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findEntities "" [ecs componentId & moreIds]
  (let [data (:data @ecs)
        ret (atom [])
        cids (concat [componentId] moreIds)]
    (when (every? #(contains? data %) cids)
      (let
        [ccs (sort #(cond
                      (< (first %1) (first %2)) -1
                      (> (first %1) (first %2)) 1
                      :else 0)
                   (map #(let [v (get data %)]
                           [(count v) v]) cids))
         ccsz (count ccs)
         [_ c0] (if (pos? ccsz) (first ccs))]
        ;;use the shortest cache as the baseline
        (if (some? c0)
          (doseq [eid (keys c0)
                  :let [sum (atom 0)]]
            ;; look for intersection
            (doseq [c ccs]
              (if (or (identical? c c0)
                      (contains? c eid))
                (swap! sum inc)))
            ;; if found in all caches...
            (if (= @sum ccsz)
              (swap! ret conj eid))))))
    @ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addTemplate "" [ecs id template & more]
  (swap!
    ecs
    (fn [root]
      (assoc root
             :templates
             (loop [tree (:templates root)
                    [p & xs]
                    (partition
                      2
                      (concat [id template] more))]
               (if-not (some? p)
                 tree
                 (recur
                   (assoc tree
                          (first p)
                          (last p)) xs))))))
  ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTemplateKeys "" [ecs] (keys (:templates @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findTemplate
  "" [ecs templateId] (get (:templates @ecs) templateId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeTemplate "" [ecs id & moreIds]
  (swap!
    ecs
    (fn [{:keys [templates] :as root}]
      (assoc root
             :templates
             (apply dissoc
                    templates
                    (concat [id] moreIds)))))
  ecs)

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
  (doseq [s (:systems @ecs)] (s ecs dt)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


