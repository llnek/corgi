;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.ecs

  (:require-macros [czlab.elmo.afx.core :as ec :refer [n#]])
  (:require [czlab.elmo.afx.core :as ec :refer [raise!]]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createPool "" [ctor rinse & [batch]]
  {:pre [(fn? ctor)(fn? rinse)]}
  (let [a (atom {::batch (or batch 10) ::size 0 ::next 0
                 ;easier to use jsarray
                 ::slots #js[] ::ctor ctor ::rinse rinse})
        g (fn [b]
            (dotimes [_ (::batch @a)]
               (.push b (oset! (ctor) "!____pool" a)))
             [(+ (::size @a) (::batch @a)) b])]
    (swap! a #(merge % {::grow g})) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countUsedInPool "" [pool] (::next @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sizeOfPool "" [pool] (::size @pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn takeFromPool! "" [pool]
  (let [out (atom nil)]
    (swap! pool
           (fn [{:keys [::grow ::size
                        ::next ::slots] :as root}]
             (let [next1 (inc next)
                   [sz buf]
                   (if (>= next size)
                     (grow slots) [size slots])]
               ;take a free obj, set it's slot, up the pool's free ptr
               (reset! out (aget buf next))
               (doto @out
                 (oset! "!____slot" next)
                 (oset! "!____status" true))
               (merge root
                      (if (= sz size)
                        {::next next1}
                        {::next next1 ::size sz}))))) @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn returnToPool! "" [pool obj]
  (if (and (some? obj)
           (oget obj "?____status")
           (identical? (oget obj "?____pool") pool))
    ;jiggle the free slot to reuse the one just dropped
    (swap! pool
           (fn [{:keys [::rinse ::next ::slots] :as root}]
             (let [next1 (dec next)
                   _ (rinse obj)
                   tail (aget slots next1)
                   slot' (oget tail "?____slot")
                   epos' (oget obj "?____slot")]
               ;set the free ptr to the dropped, move the tail to old slot
               (aset slots next1 obj)
               (aset slots epos' tail)
               ;swap the 2 slots
               (oset! tail "!____slot" epos')
               (oset! obj "!____slot" slot')
               (oset! obj "!____status" false)
               (merge root {::next next1}))))) pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createECS "" []
  (atom {::entities #{}
         ::templates {}
         ::registry {}
         ::data {}
         ::systems [] ::uid 1}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- genUid "" [ecs]
  (let [out (atom 0)]
    (swap! ecs
           (fn [{:keys [::uid] :as root}]
             (reset! out uid)
             (assoc root ::uid (inc uid)))) @out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- retUsed "" [obj]
  (cond
    (vector? obj) (doseq [c obj] (retUsed c))
    (map? obj) (retUsed (vals obj))
    (object? obj) (if-some
                    [p (oget obj "?____pool")] (returnToPool! p obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- remEnt "" [{:keys [data registry] :as root} ents]
  (let
    [dt (loop [dt data [k & xs] (keys registry)]
          (if-not (some? k)
            dt ;;exit loop
            (let [cOrig (get dt k)
                  ct (loop [ct cOrig [e & xs] ents]
                       (if-not (some? e)
                         ct ;;exit loop
                         (recur (if-some [v (get ct e)]
                                  (do (retUsed v) (dissoc ct e)) ct) xs)))]
              (recur (if (identical? ct cOrig) dt (assoc dt k ct)) xs))))
     rt (if (identical? dt data) root (assoc root ::data dt))]
    (assoc rt
           ::entities (apply disj (::entities rt) ents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeEntity "" [ecs entity & more]
  (swap! ecs #(remEnt % (concat [entity] more))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addComponent "" [ecs id component & more]
  (swap! ecs
         #(->> (concat [id component] more)
               (partition 2)
               (map (fn [a] (vec a)))
               (vec)
               (into {})
               (merge (::registry %))
               (assoc % ::registry))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeComponent "" [ecs id & more]
  (swap!
    ecs
    (fn [{:keys [::data ::registry] :as root}]
      (let [cids (concat [id] more)]
        (doseq [c cids
                :let [v (get data c)]
                :when (some? v)] (retUsed v))
        (-> root
            (assoc ::data (apply dissoc data cids))
            (assoc ::registry (apply dissoc registry cids)))))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addToEntity "" [ecs entity componentDecl & moreDecls]
  (swap! ecs
         (fn [{:keys [::data ::registry] :as root}]
           (->> (loop [dtree data
                       [dc & xs]
                       (concat [componentDecl] moreDecls)]
                  (if-not (some? dc)
                    dtree ;exit loop
                    (let [[cid & args] dc
                          ctor (get registry cid)
                          _ (if-not (fn? ctor)
                              (raise! "Unknown component " cid))
                          co (-> (apply ctor args)
                                 (oset! "!____entity" entity))]
                      (recur (update-in dtree
                                        [cid] #(assoc % entity co)) xs))))
                (assoc root ::data)))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createEntity "" [ecs componentDecl & moreDecls]
  (let [entity (genUid ecs)]
    (apply addToEntity
           (concat [ecs entity componentDecl] moreDecls))
    (swap! ecs
           #(update-in %
                       [::entities]
                       (fn [c] (conj c entity)))) entity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeFromEntity "" [ecs entity & componentIds]
  (swap! ecs
         (fn
           [{:keys [::data] :as root}]
           (let
             [dt (loop [dtree data
                        [cid & xs] componentIds]
                   (if-not (some? cid)
                   dtree ;exit loop
                   (let [co (get dtree cid)]
                     (recur (if-some [v (if (some? co)
                                          (get co entity))]
                              (do (retUsed v)
                                  (update-in dtree
                                             [cid] #(dissoc % entity)))
                              dtree)
                            xs))))]
        (if (identical? dt data) root (assoc root ::data dt))))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getEntityData "" [ecs entity componentId]
  (if-some
    [ct (-> (::data @ecs)
            (get componentId))] (get ct entity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateEntity "" [ecs entity componentId func]
  (when-some [c (getEntityData ecs entity componentId)] (func c) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentsData "" [ecs componentId]
  (if-some [c (-> (::data @ecs) (get componentId))] (vals c) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getComponentKeys "" [ecs] (keys (::registry @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findComponent
  "" [ecs componentId] (get (::registry @ecs) componentId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn componentInEntity?
  "" [ecs entity componentId & moreIds]
  (let [d (::data @ecs)]
    (not (some #(if-some [co (get d %)]
                  (not (contains? co entity)) true) (concat [componentId] moreIds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findEntities "" [ecs componentId & moreIds]
  (let [data (::data @ecs)
        ret (atom [])
        cids (concat [componentId] moreIds)]
    (when (every? #(contains? data %) cids)
      (let
        [ccs (sort #(cond
                      (< (first %1) (first %2)) -1
                      (> (first %1) (first %2)) 1 :else 0)
                   (map #(let [v (get data %)] [(n# v) v]) cids))
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
              (swap! ret conj eid))))))
    @ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addTemplate "" [ecs id template & more]
  (swap! ecs
         #(->> (concat [id template] more)
               (partition 2)
               (map (fn [a] (vec a)))
               (vec)
               (into {})
               (merge (::templates %))
               (assoc % ::templates))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTemplateKeys "" [ecs] (keys (::templates @ecs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn findTemplate
  "" [ecs templateId] (get (::templates @ecs) templateId))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeTemplate "" [ecs id & moreIds]
  (swap! ecs
         #(assoc %
                 ::templates
                 (apply dissoc (concat [(::templates %) id] moreIds)))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createTemplateEntity "" [ecs id]
  (let [{:keys [::components ::initor] :as t} (get (::templates @ecs) id)
        e (if (some? t) (apply createEntity (concat [ecs] components)))]
    (if (and (some? e)
             (fn? initor)) (initor ecs e)) e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addSystem "" [ecs system & more]
  (swap! ecs #(assoc %
                     ::systems
                     (concat (::systems %) (concat [system] more)))) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeSystem "" [ecs system & more]
  (let [yy (concat [system] more)]
    (swap! ecs
           (fn [root]
             (->> (::systems root)
                  (filter (fn [s] (not-any?
                                    #(identical? % s) yy)))
                  (assoc root ::systems)))) ecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [ecs dt]
  (doseq [s (::systems @ecs)] (s ecs dt)) ecs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


