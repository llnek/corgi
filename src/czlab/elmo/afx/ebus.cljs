;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.ebus

  (:require-macros [czlab.elmo.afx.core :as ec :refer [if-some+]])
  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core :as ec :refer []]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def _SEED (atom 0))
(defn- nextSEQ "" [] (swap! _SEED inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def re-space #"\s+")
(def re-slash #"\/")
(def re-dot #"\.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split* "" [topic]
  (if (string? topic)
    (->> (cs/split topic re-dot)
         (filter #(not-empty %))) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkSubSCR "" [topic listener options]
  {:pre [(fn? listener)]}
  (let [{:keys [repeat?]} options]
    (-> {:action listener}
        (merge {:id (str "s#" (nextSEQ))
                :repeat? repeat?
                :async? false :topic topic :status (atom 1)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- mkLevelNode "" [] {:levels {} :subcs {}})
(defn- mkTreeNode "" [] {:topics {} :subcs {}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addOneSub "" [node sub]
  (update-in! node [:subcs] assoc (:id sub) sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- remOneSub "" [node sub]
  (update-in! node [:subcs] dissoc (:id sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- interleavePath "" [paths] (mapcat #([:levels %]) paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listen
  "Subscribe to a topic."
  [root qos topic listener options]
  (addTopic root qos (mkSubSCR topic listener options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run "" [subcs topic msg]
  (doseq [[_ z] subcs
          :let [{:keys [repeat? action status]} z]
          :when (zero? @status)]
    (action (:topic z) topic msg)
    (if-not repeat? (reset! status -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- walk "" [branch pathTokens topic msg tst]
  (let [{:keys [levels subcs]} branch
        [p & more] pathTokens
        cur (get levels p)
        s1 (get levels "*")
        s1c (:levels s1)
        s2 (get levels "**")]
    (when s2
      (if tst
        (swap! tst inc)
        (run (:subcs s2) topic msg)))
    (if s1
      (cond
        (and (empty? more)
             (empty? s1c))
        (if tst
          (swap! tst inc)
          (run (:subcs s1) topic msg))
        (and (not-empty s1c)
             (not-empty more))
        (walk s1 more topic msg tst)))
    (when cur
      (if (not-empty more)
        (walk cur more topic msg tst)
        (if tst
          (swap! tst inc)
          (run (:subcs cur) topic msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resume* "" [root hd]
  (let [sub (get-in root [:subcs hd])
        st (if sub (:status sub))]
    (if (and (some? st)
             (zero? @st)) (reset! st 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pause* "" [root hd]
  (let [sub (get-in root [:subcs hd])
        st (if sub (:status sub))]
    (if (and (some? st)
             (pos? @st)) (reset! st 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addOneTopic "" [top qos {:keys [topic id] :as sub}]
  (if (= :rbus qos)
    (let [path (interleavePath (split* topic))]
      (-> (update-in top path addOneSub sub)
          (update-in [:subcs] assoc id sub)))
    (-> (update-in top [:topics topic] assoc id sub)
        (update-in [:subcs] assoc id sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addTopic "" [root qos sub]
  (swap! root addOneTopic qos sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- delTopic "" [top qos {:keys [topic id] :as sub}]
  (if (= :rbus qos)
    (let [path (interleavePath (split* topic))]
      (-> (update-in top path remOneSub sub)
          (update-in [:subcs] dissoc id)))
    (-> (update-in top [:topics topic] dissoc id)
        (update-in [:subcs] dissoc id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn RvBus "Event bus - subject based." [options]
  (atom {:state (mkLevelNode)
         :qos :rv
         :options (or options {})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn EvBus "Event bus." [options]
  (atom {:state (mkTreeNode)
         :qos :ev
         :options (or options {})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sub*
  "One time only subscription"
  [bus topic listener]
  (listen bus topic listener false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sub+
  "Standard subscription"
  [bus topic listener]
  (listen bus topic listener true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pub
  "Send a message" [bus topic msg]
  (let [{:keys [qos] @bus}]
    (if (= :rv qos)
      (if-some+
        [tokens (split* topic)] (walk s tokens topic msg nil))
      (if-some [sub
                (get-in s
                        [:topics topic])] (run sub topic msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resume
  "Resume this subscriber" [bus handle] (resume* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (pause
    "Pause this subscriber"
    [handle] (pause* (deref @@state) handle))

  (unsub
    "Remove this subscriber"
    [handle]
    (var s (deref @@state))
    (if-some [x (get-in s [:subcs handle])]
      (swap! s delTopic qos x)) nil)

  (match?
    "Internal: test only"
    [topic]
    (var s (deref @@state))
    (if (== :rbus @@qos)
      (let [tokens (split* topic)
            z (atom 0)]
        (if (not-empty tokens)
          (walk s tokens topic nil z))
        (pos? @z))
      (contains? (get s :topics) topic)))

  (dbg
    "Internal: test only"
    [] (K/prn (deref @@state)))

  (finz
    "Remove all" []
    (do->nil (reset! @@state
                     (if (== :rbus @@qos)
                       (mkLevelNode)
                       (mkTreeNode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createEventBus
  "A Publish Subscribe event manager.  If subject based is
  used, a more advanced matching scheme will be used - such as
  wild-card matches."
  [&options]
  (var {:keys [subjectBased?]} (opt?? options {}))
  (new EventBus
       (if subjectBased? :rbus :ebus) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


