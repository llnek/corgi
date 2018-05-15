;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.game

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [_1 _2 f#* do-with each-indexed]]
    [czlab.elmo.afx.ccsx :as cx :refer [sprite* attr*]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [*xcfg*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.afx.ecs :as ecs]
            [czlab.elmo.afx.ebus :as ebus]
            [czlab.elmo.tictactoe.impl :as impl]
            [czlab.elmo.tictactoe.misc :as mc]
            [czlab.elmo.tictactoe.hud :as hud]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bgLayer "" []
  (do-with [layer (new js/cc.Layer)]
    (cx/addItem layer
                (-> (sprite* (cx/gimg :game-bg))
                    (cx/setXXX! {:pos (cx/centerPos)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- showGrid "" [layer]
  (let [{:keys [GRID-SIZE CV-Z]} (:game @*xcfg*)]
    (->
      (reduce
        (fn [acc mp]
          (let [pt (cx/vbox4MID mp)
                sp (-> (sprite* "#z.png")
                       (cx/setXXX! {:pos pt}))]
            (cx/addItem layer sp)
            (conj! acc [sp pt CV-Z])))
        (transient [])
        (mc/mapGridPos GRID-SIZE 1))
      (persistent! ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- arenaLayer "" [state]
  (do-with [layer (new js/cc.Layer)]
    (let [cs (showGrid layer)]
      (swap! state #(assoc % :cells cs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initOnce "" [state]
  (f#* (impl/init state)
       (swap! state #(assoc % :running? true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" [mode px py & more]
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [GRID-SIZE CV-Z]} (:game @*xcfg*)
          zmks [:ptype :pvalue :pcolor :pid :pname]
          sz (* GRID-SIZE GRID-SIZE)
          state (atom {:gspace (mc/mapGoalSpace GRID-SIZE)
                       :gpos (mc/mapGridPos GRID-SIZE 1)
                       :ebus (ebus/createEvBus)
                       :ecs (ecs/createECS)
                       :grid (ec/fillArray CV-Z sz)
                       :running? false
                       :gmode mode
                       :selected -1
                       :evQ (array)
                       :whoAmI nil :whosTurn nil :cells nil
                       :scores {(_1 px) 0 (_1 py) 0}
                       (_1 px) (zipmap zmks (rest px))
                       (_1 py) (zipmap zmks (rest py))})
          bl (bgLayer)
          gl (arenaLayer state)
          hud (hud/hudLayer (_1 px) (_1 py) state 0 0)]
      (reset! cx/*game-scene* scene)
      (cx/addItem scene bl "bg" -1)
      (cx/addItem scene gl "arena" 1)
      (cx/addItem scene hud "hud" 2)
      (attr* scene
             #js{:gstate state
                 :update #(ecs/updateECS (:ecs @state) %)})
      (ocall! scene "scheduleOnce" (initOnce state) 0.1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



