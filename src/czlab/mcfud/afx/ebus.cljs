;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.ebus

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid defvoid- if-some+]])

  (:require [clojure.string :as cs]
            [czlab.mcfud.afx.core :as ec :refer [raise! debug*]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private _SEED (atom 0))
(defn- nextSEQ

  "Return the next sequence number."
  []

  (swap! _SEED inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private re-subj #"^[0-9a-zA-Z_\-]+(\.(\*|[0-9a-zA-Z_\-]+))*(\.>)?$")
(def ^:private re-space #"\s+")
(def ^:private re-dot #"\.")
(def ^:private re-slash #"/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split*

  "Split the topic string into its sub-parts."
  [topic]

  (filterv #(not-empty %) (cs/split topic re-dot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- tibrv?

  "True if the bus is a rv-bus."
  [bus]

  (= ::rv (:qos @bus)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mksub

  "Make a subscriber object."
  [topic lnr r?]
  {:pre [(fn? lnr)]}

  (assert (re-matches re-subj topic)
          (str "Error in topic: " topic))
  (hash-map :status (atom 1)
            :async? false
            :action lnr
            :repeat? r?
            :topic topic
            :id (str "s#" (nextSEQ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- mkBusNode

  "Make the root bus node."
  [qos]

  (merge (condp = qos
           ::rv {:levels {}}
           ::ev {:topics {}}
           (raise! "bad qos")) {:qos qos :subcs {}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- plevels

  "Create a vector like [:levels a :levels b :levels c]
  or [:topics topic] for functions like get-in or update-in"
  [bus topic]

  (if-not (tibrv? bus)
    [:topics topic]
    (-> (->> (split* topic)
             (mapcat #(vector :levels %))) (vec) (conj :subcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- run

  "Iterate through the list of subscribers
  and apply the action with the event data."
  [subcs msgTopic msgs]

  (doseq [[_ z] subcs
          :let [{:keys [repeat? topic
                        action status]} z]
          :when (pos? @status)]
    (apply action (concat [topic msgTopic] msgs))
    ;for one time only subsc, flag it dead
    (if-not repeat? (reset! status -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- walk

  "Traverse the topic tree, looking for matching patterns."
  [branch path topic msgs tst]

  ;;(debug* (str "walking " (pr-str branch)))
  ;;(debug* (str "path " (pr-str path)))
  (let [{:keys [levels subcs]} branch
        [p & more] path
        {s1 "*"
         s2 ">"} levels
        {s1c :levels} s1]
    ;(debug* (str "s1 " (pr-str s1)))
    ;(debug* (str "s2 " (pr-str s2)))
    ;(debug* (str "s1c " (pr-str s1c)))
    ;check for ".>" stuff
    (when (some? s2)
      (if (some? tst)
        (swap! tst inc)
        (do ;;(debug* ".> stuff called to run")
            (run (:subcs s2) topic msgs))))
    ;check for ".*" stuff
    (when (some? s1)
      ;;(debug* "checking .* stuff")
      (cond (and (empty? more)
                 (empty? s1c))
            (if (some? tst)
              (swap! tst inc)
              (do ;;(debug* ".* stuff called to run")
                  (run (:subcs s1) topic msgs)))
            (and (not-empty s1c)
                 (not-empty more))
            (do ;;(debug* "walk .* stuff")
                (walk s1 more topic msgs tst))))
    ;check for matching
    (when-some [cur (get levels p)]
      (if (not-empty more)
        (do ;;(debug* "walk match more")
            (walk cur more topic msgs tst))
        (if (some? tst)
          (swap! tst inc)
          (do ;;(debug* "run match cur")
              (run (:subcs cur) topic msgs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- resume*

  "Flag subscriber on"
  [bus hd]

  (if-some [sub (get-in @bus [:subcs hd])]
    (let [{:keys [status]} sub]
      (if (zero? @status) (reset! status 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- pause*

  "Flag subscriber off"
  [bus hd]

  (if-some [sub (get-in @bus [:subcs hd])]
    (let [{:keys [status]} sub]
      (if (pos? @status) (reset! status 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listen

  "Subscribe to a topic."
  [bus topic lsnr r?]

  (let [path (plevels bus topic)
        {:keys [id] :as sub} (mksub topic lsnr r?)]
    (swap! bus
           (fn [root]
             (-> (update-in root path assoc id sub)
                 (update-in [:subcs] assoc id sub))))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;bus api
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
(defvoid pub

  "Send a message"
  [bus topic & msgs]

  (if (tibrv? bus)
    (if-some+
      [path (split* topic)] (walk @bus path topic msgs nil))
    (if-some [sub
              (get-in @bus
                      [:topics topic])] (run sub topic msgs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resume

  "Resume this subscriber"
  [bus handle]

  (resume* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pause

  "Pause this subscriber"
  [bus handle]

  (pause* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid unsub

  "Remove this subscriber"
  [bus handle]

  (if-some [sub (get-in @bus
                         [:subcs handle])]
    (swap! bus
           (fn [root]
             (let [{:keys [topic id]} sub
                   path (plevels bus topic)]
               (-> (update-in root path dissoc id)
                   (update-in [:subcs] dissoc id)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match?

  "Internal: test only"
  [bus topic]

  (if (tibrv? bus)
    (when-some [z (atom 0)]
      (if-some+ [path (split* topic)]
                (walk @bus path topic nil z))
      (pos? @z))
    (contains? (:topics @bus) topic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dbg

  "Internal: test only"
  [bus]

  (pr-str @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid unsubAll!

  "Remove all"
  [bus]

  (swap! bus #(merge % (mkBusNode (:qos @bus)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newTibrvBus

  "Subject based."
  [& [options]]

  (atom (merge (mkBusNode ::rv) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newEventBus

  "Event bus."
  [& [options]]

  (atom (merge (mkBusNode ::ev) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


