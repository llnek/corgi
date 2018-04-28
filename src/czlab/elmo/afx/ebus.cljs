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

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [if-some+]])
  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core :as ec :refer []]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def _SEED (atom 0))
(defn- nextSEQ
  "" [] (swap! _SEED inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def re-space #"\s+")
(def re-dot #"\.")
(def re-slash #"\/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split*
  "" [topic] (if (string? topic)
               (->> (cs/split topic re-dot)
                    (filter #(not-empty %))) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkSubSCR
  "" [topic listener repeat?]
  {:pre [(fn? listener)]}

  {:id (str "s#" (nextSEQ))
   :action listener
   :async? false
   :repeat? repeat?
   :topic topic :status (atom 1)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- mkLevelNode "" [] {:levels {} :subcs {}})
(defn- mkTreeNode "" [] {:topics {} :subcs {}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addOneSub
  "" [node sub]
  (update-in node [:subcs] assoc (:id sub) sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- remOneSub
  "" [node sub]
  (update-in node [:subcs] dissoc (:id sub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- interleavePath
  "" [paths] (mapcat (fn [p] [:levels p]) paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run "" [subcs topic msg]
  (doseq [[_ z] subcs
          :let [{:keys [repeat? action status]} z]
          :when (pos? @status)]
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
(defn- addOneTopic "" [root qos {:keys [topic id] :as sub}]
  (let [{:keys [state]} root]
    (assoc root
           :state
           (if (= :rv qos)
             (let [path (interleavePath (split* topic))]
               (-> (update-in state path addOneSub sub)
                   (update-in [:subcs] assoc id sub)))
             (-> (update-in state [:topics topic] assoc id sub)
                 (update-in [:subcs] assoc id sub))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addTopic "" [bus qos sub]
  (swap! bus addOneTopic qos sub) (:id sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- delTopic "" [state qos {:keys [topic id] :as sub}]
  (if (= :rv qos)
    (let [path (interleavePath (split* topic))]
      (-> (update-in state path remOneSub sub)
          (update-in [:subcs] dissoc id)))
    (-> (update-in state [:topics topic] dissoc id)
        (update-in [:subcs] dissoc id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listen
  "Subscribe to a topic."
  [bus qos topic listener repeat?]
  (addTopic bus qos (mkSubSCR topic listener repeat?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn RvBus
  "Event bus - subject based." [options]
  (atom {:state (mkLevelNode) :qos :rv :options (or options {})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn EvBus
  "Event bus." [options]
  (atom {:state (mkTreeNode) :qos :ev :options (or options {})}))

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
  (let [{:keys [qos state] @bus}]
    (if (= :rv qos)
      (if-some+
        [tokens (split* topic)] (walk state tokens topic msg nil))
      (if-some [sub
                (get-in state
                        [:topics topic])] (run sub topic msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resume
  "Resume this subscriber" [bus handle] (resume* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pause
  "Pause this subscriber" [bus handle] (pause* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unsub
  "Remove this subscriber"
  [bus handle]
  (swap! bus
         (fn [{:keys [state] :as root}]
           (if-some [x (get-in state [:subcs handle])]
             (assoc root
                    :state
                    (delTopic state qos x)) root))) bus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match?
  "Internal: test only" [bus topic]
  (let [{:keys [qos state]} @bus]
    (if (= :rv qos)
      (let [tokens (split* topic)
            z (atom 0)]
        (if (not-empty tokens)
          (walk state tokens topic nil z))
        (pos? @z))
      (contains? (:topics state) topic))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dbg "Internal: test only" [bus] (pr-str @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn finz "Remove all" [bus]
  (swap! bus
         (fn [{:keys [qos] :as root}]
           (assoc root
                  :state (if (= :rv qos)
                           (mkLevelNode) (mkTreeNode))))) bus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


