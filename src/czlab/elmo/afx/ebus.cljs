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

  (:require-macros
    [czlab.elmo.afx.core :as ec :refer [if-some+]])
  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core :as ec :refer [raise!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def _SEED (atom 0))
(defn- nextSEQ
  "" [] (swap! _SEED inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def re-subj #"^[0-9a-zA-Z_\-]+(\.(\*|[0-9a-zA-Z_\-]+))*(\.>)?$")
(def re-space #"\s+")
(def re-dot #"\.")
(def re-slash #"/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- split*
  "" [topic] (if (string? topic)
               (->> (cs/split topic re-dot)
                    (filter #(not-empty %))) []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mksub
  "" [topic lnr r?]
  {:pre [(fn? lnr)]}
  (if (nil? (re-matches re-subj topic))
    (raise! "Syntax error in topic: " topic))
  {::id (str "s#" (nextSEQ))
   ::async? false
   ::action lnr ::repeat? r? ::topic topic ::status (atom 1)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nodes - children
;; subscribers
(defn- mkLevelNode "" [] {::levels {} ::subcs {}})
(defn- mkTreeNode "" [] {::topics {} ::subcs {}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- plevels
  "Create a vector like [:levels a :levels b :levels c]
  for functions like get-in or update-in"
  [topic]
  (-> #(vector ::levels %)
      (mapcat (split* topic)) (vec) (conj ::subcs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run "" [subcs msgTopic msgs]
  (doseq [[_ z] subcs
          :let [{:keys [::repeat? ::topic
                        ::action ::status]} z]
          :when (pos? @status)]
    (apply action (concat [topic msgTopic] msgs))
    ;for one time only subsc, flag it dead
    (if-not repeat? (reset! status -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- walk "" [branch path topic msgs tst]
  (let [{:keys [::levels ::subcs]} branch
        [p & more] path
        cur (get levels p)
        s1 (get levels "*")
        s1c (::levels s1)
        s2 (get levels ">")]
    ;(js/console.log (str "walking " (pr-str branch)))
    (when s2
      (if (some? tst)
        (swap! tst inc)
        (run (::subcs s2) topic msgs)))
    (if s1
      (cond (and (empty? more)
                 (empty? s1c))
            (if (some? tst)
              (swap! tst inc)
              (run (::subcs s1) topic msgs))
            (and (not-empty s1c)
                 (not-empty more))
            (walk s1 more topic msgs tst)))
    (when (some? cur)
      (if (not-empty more)
        (walk cur more topic msgs tst)
        (if (some? tst)
          (swap! tst inc)
          (run (::subcs cur) topic msgs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resume* "Flag subscriber on" [bus hd]
  (if-some [sub (get-in @bus [::subcs hd])]
    (let [{:keys [::status]} sub]
      (if (zero? @status) (reset! status 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pause* "Flag subscriber off" [bus hd]
  (if-some [sub (get-in @bus [::subcs hd])]
    (let [{:keys [::status]} sub]
      (if (pos? @status) (reset! status 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listen
  "Subscribe to a topic."
  [bus topic lsnr r?]
  (let [sub (mksub topic lsnr r?)
        {:keys [::id]} sub {:keys [::qos]} @bus]
    (swap! bus
           #(-> (update-in %
                           (if (= ::rv qos)
                             (plevels topic)
                             [::topics topic]) assoc id sub)
                (update-in [::subcs] assoc id sub)))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;bus api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sub*
  "One time only subscription"
  [bus topic listener] (listen bus topic listener false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sub+
  "Standard subscription"
  [bus topic listener] (listen bus topic listener true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pub
  "Send a message" [bus topic & msgs]
  (if (= ::rv (::qos @bus))
    (if-some+
      [path (split* topic)] (walk @bus path topic msgs nil))
    (if-some [sub
              (get-in @bus
                      [::topics topic])] (run sub topic msgs))))

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
  (when-some [sub (get-in @bus
                          [::subcs handle])]
    (swap! bus
           #(let [{:keys [::qos]} %
                  {:keys [::topic ::id]} sub]
              (-> (update-in %
                             (if (= ::rv qos)
                               (plevels topic)
                               [::topics topic]) dissoc id)
                  (update-in [::subcs] dissoc id)))))
  bus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn match?
  "Internal: test only" [bus topic]
  (if (= ::rv (::qos @bus))
    (let [path (split* topic)
          z (atom 0)]
      (if (not-empty path)
        (walk @bus path topic nil z)) (pos? @z))
    (contains? (::topics @bus) topic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dbg "Internal: test only" [bus] (pr-str @bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn unsubAll! "Remove all" [bus]
  (let [{:keys [::qos]} @bus]
    (swap! bus
           #(merge %
                   (if (= ::rv qos)
                     (mkLevelNode)
                     (mkTreeNode)))) bus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createRvBus
  "Event bus - subject based." [& [options]]
  (atom (merge (mkLevelNode) {::qos ::rv} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createEvBus
  "Event bus." [& [options]]
  (atom (merge (mkTreeNode) {::qos ::ev} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


