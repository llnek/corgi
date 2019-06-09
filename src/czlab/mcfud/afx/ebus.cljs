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

  (:require [clojure.string :as cs]
            [czlab.mcfud.afx.core
             :as c :refer [in? raise! debug*
                           do-with cc+ if-some+ let->nil]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private _SEED (atom 0))
(defn- next-seq [] (swap! _SEED inc))

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
(defn- rv?
  "If bus is a rv-bus?"
  [bus] (= ::rv (:qos @bus)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mksub
  "Make a subscriber object."
  [topic lnr r?]
  {:pre [(fn? lnr)]}
  (assert (re-matches re-subj topic)
          (str "Error in topic: " topic))
  {:status (atom 1)
   :async? false
   :action lnr
   :repeat? r?
   :topic topic
   :id (str "s#" (next-seq))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- bus-node
  "Make the root bus node.
  rvbus
  +++++
  The tree os subscribers are deeply nested.
  {:levels {\"a\"
                 {:levels {\"*\"
                                {:levels {\">\"
                                               {:subcs {\"s#12\" {}}}}}}}}
   :qos :czlab.mcfud.afx.ebus/rv
   :subcs {\"s#12\" {}}}
  evbus
  +++++
  {:topics {\"a.b.c\" {\"s#1\" {}}
            \"x.y\" {\"s#2\" {}}}
   :qos :czlab.mcfud.afx.ebus/ev
   :subcs {\"s#1\" {} \"s#2\" {}}}"
  [qos]
  {:qos qos :subcs {} (condp = qos
                        ::rv :levels
                        ::ev :topics (raise! "bad qos")) {}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- plevels
  "Create a vector like [:levels a :levels b :levels c]
  or [:topics topic] for functions like get-in or update-in"
  [bus topic]
  (if-not (rv? bus)
    [:topics topic]
    (-> (->> (split* topic)
             (mapcat #(vector :levels %))) (vec) (conj :subcs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run
  "Iterate through the list of subscribers
  and apply the action with the event data."
  [subcs msgTopic msgs]
  (doseq [[_ z] subcs
          :let [{:keys [repeat? topic
                        action status]} z]
          :when (pos? @status)]
    (apply action (cc+ [topic msgTopic] msgs))
    ;for one time only subsc, flag it dead
    (if-not repeat? (reset! status -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- walk
  "Traverse the topic tree, looking for matching patterns."
  [branch path topic msgs tst]
  ;;(debug* (str "walking " (pr-str branch)))
  ;;(debug* (str "path " (pr-str path)))
  (let->nil
    [{:keys [levels subcs]} branch
     {s1 "*" s2 ">"} levels
     [p & more] path
     {s1c :levels} s1]
    ;(debug* (str "s1 " (pr-str s1)))
    ;(debug* (str "s2 " (pr-str s2)))
    ;(debug* (str "s1c " (pr-str s1c)))
    ;check for ".>" stuff
    (when (some? s2)
      (if (some? tst)
        (swap! tst inc)
        (run (:subcs s2) topic msgs)))
    ;check for ".*" stuff
    (when (some? s1)
      ;;(debug* "checking .* stuff")
      (cond (and (empty? more)
                 (empty? s1c))
            (if (some? tst)
              (swap! tst inc)
              (run (:subcs s1) topic msgs))
            (and (not-empty s1c)
                 (not-empty more))
            (walk s1 more topic msgs tst)))
    ;check for matching
    (when-some [cur (get levels p)]
      (if (not-empty more)
        (walk cur more topic msgs tst)
        (if (some? tst)
          (swap! tst inc)
          (run (:subcs cur) topic msgs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resume*
  "Flag subscriber on"
  [bus hd]
  (if-some [sub (get-in @bus [:subcs hd])]
    (let->nil [{:keys [status]} sub]
      (if (zero? @status) (reset! status 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pause*
  "Flag subscriber off"
  [bus hd]
  (if-some [sub (get-in @bus [:subcs hd])]
    (let->nil [{:keys [status]} sub]
      (if (pos? @status) (reset! status 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listen
  "Subscribe to a topic."
  [bus topic lsnr r?]
  (let [path (plevels bus topic)
        sub (mksub topic lsnr r?)]
    (do-with [id (:id sub)]
      (swap! bus
             #(-> (update-in % path assoc id sub)
                  (update-in [:subcs] assoc id sub))))))

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
(defn pub
  "Send a message"
  [bus topic & msgs]
  (do-with [bus]
    (if (rv? bus)
      (if-some+
        [path (split* topic)] (walk @bus path topic msgs nil))
      (if-some [sub
                (get-in @bus
                        [:topics topic])] (run sub topic msgs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resume!
  "Resume this subscriber"
  [bus handle] (resume* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pause!
  "Pause this subscriber"
  [bus handle] (pause* bus handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unsub!
  "Remove this subscriber"
  [bus handle]
  (do-with [bus]
    (if-some [sub (get-in @bus
                          [:subcs handle])]
      (swap! bus
             #(let [{:keys [topic id]} sub
                    path (plevels bus topic)]
                (-> (update-in % path dissoc id)
                    (update-in [:subcs] dissoc id)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match?
  "Internal: test only.  Tests if a topic matches any subscriptions."
  [bus topic]
  (if (rv? bus)
    (when-some [z (atom 0)]
      (if-some+ [path (split* topic)]
                (walk @bus path topic nil z))
      (pos? @z))
    (in? (:topics @bus) topic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dbg
  "Internal: test only"
  [bus]
  (pr-str @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unsub-all!
  "Remove all.  All subscribers are removed."
  [bus]
  (do-with [bus] (swap! bus #(merge % (bus-node (:qos @bus))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-tibrv-bus
  "Subject based. Topics are treated as segments separated by a
  period.  A star is used to represent a wildcard segment, and
  a > is used to represent subsequent match-all.
  e.g.  a.b.c - matches \"a.b.c\" only
        a.*.c - matches \"a.b.c\", \"a.z.c\" ...etc
        a.> - matches \"a.b\", \"a.b.c.d\", \"a.z.e\".
  A subscription made to a topic can then be triggered by
  various instances of topics."
  [& [options]]
  (atom (merge (bus-node ::rv) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-event-bus
  "Event bus.  Simple pub-sub by matching on a topic string."
  [& [options]]
  (atom (merge (bus-node ::ev) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


