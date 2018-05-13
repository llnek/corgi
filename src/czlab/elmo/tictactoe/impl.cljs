;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.impl

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [do->true each-indexed f#* n# _1 _2 do-with]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-bottom oget-right
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.ccsx :as cx]
    [czlab.elmo.afx.ecs :as ecs]
    [czlab.elmo.afx.ebus :as ebus]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- click->cell "" [gridPos x y]
  (cx/info* "grid pos === " gridPos)
  (let [ret (some
              (fn [i]
                (let [r (nth gridPos i)]
                  (if (and (>= x (oget-left r))
                           (<= x (oget-right r))
                           (>= y (oget-bottom r))
                           (<= y (oget-top r)))
                    [i])))
              (range (n# gridPos)))]
    (if (not-empty ret) (_1 ret) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onClick "" [gridPos topic msgTopic & msgs]
  (let [evt (_1 msgs)
        e (ocall evt "getLocation")
        [x y] [(oget-x e) (oget-y e)]
        _ (js/console.log "clicked -> " x ", " y)
        cell (click->cell gridPos x y)]
    (if-not (neg? cell)
      (cx/info* "cell==== " cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onTouch "" [gridPos topic msgTopic & msgs]
  (let [evt (_1 msgs)
        e (ocall evt "getLocation")
        [x y] [(oget-x e) (oget-y e)]
        _ (js/console.log "clicked -> " x ", " y)
        cell (click->cell gridPos x y)]
    (if-not (neg? cell)
      (cx/info* "cell==== " cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn motion "" [ecs dt]
  (let [scene (deref cx/*game-scene*)
        state (oget scene "?gstate")
        {:keys [whoAreYou whosTurn evQ]} @state]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [scene]
  (let [state (oget scene "?gstate")
        {:keys [gpos ebus ecs evQ]} @state
        cb (fn [& xs] (.push evQ xs))]
    (cx/info* "XXXgrid pos === " gpos)
    ;(swap! state #(assoc % :whosTurn (if (pos? (ec/randSign)) 1 2)))
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus
                 "mouse.up"
                 (fn [& xs] (apply onClick gpos xs))))
    (if (cx/onTouchOne ebus)
      (ebus/sub+ ebus
                 "touch.one.end"
                 (fn [& xs] (apply onTouch gpos xs))))
    (ecs/addSystem ecs motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




