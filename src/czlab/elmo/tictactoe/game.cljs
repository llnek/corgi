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
     :as ec :refer [_1 _2 f#* do-with]]
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
(defn- showGrid "" [state layer]
  (let [{:keys [GRID-SIZE CV-Z]} (:game @*xcfg*)
        {:keys [gpos]} @state]
    (->
      (reduce
        (fn [acc mp]
          (let [pt (cx/vbox4MID mp)
                sp (-> (sprite* "#z.png")
                       (cx/setXXX! {:pos pt}))]
            (cx/addItem layer sp)
            (conj! acc [sp pt CV-Z])))
        (transient [])
        gpos)
      (persistent! ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- arenaLayer "" [state]
  (do-with [layer (new js/cc.Layer)]
    (let [cs (showGrid state layer)]
      (swap! state #(assoc % :cells cs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initOnce "" [state]
  (f#* (impl/init state)
       (swap! state #(assoc % :running? true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" [mode px py & more]
  (do-with [scene (new js/cc.Scene)]
    (cx/info* "player 1 = " (prn-str px))
    (cx/info* "player 2 = " (prn-str py))
    (let [{:keys [GRID-SIZE CV-Z]} (:game @*xcfg*)
          adon? (get-in @*xcfg* [:AD :on?])
          B (if adon? (cx/ebox4) (cx/vbox4))
          zmks [:ptype :pvalue :pcolor :pid :pname]
          sz (* GRID-SIZE GRID-SIZE)
          state (atom {:gspace (mc/mapGoalSpace GRID-SIZE)
                       :gpos (mc/mapGridPos B GRID-SIZE 1)
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
      (reset! cx/*game-arena* gl)
      (cx/addItem scene bl "bg" -1)
      (cx/addItem scene gl "arena" 1)
      (cx/addItem scene hud "hud" 2)
      (attr* scene
             #js{:gstate state
                 :update #(ecs/updateECS (:ecs @state) %)})
      (when adon?
        (let [a (cx/enableAD! scene)]
          (cx/addItem hud a)))
      (ocall! scene "scheduleOnce" (initOnce state) 0.1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



