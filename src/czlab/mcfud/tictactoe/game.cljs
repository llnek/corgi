;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.mcfud.tictactoe.game

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid- defvoid _1 _2 f#* do-with]]
                   [czlab.mcfud.cc.ccsx :as cx :refer [pos! sprite* attr*]])

  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [vrect mid-rect addItem xcfg]]
            [czlab.mcfud.afx.core :as ec]
            [czlab.mcfud.afx.ecs :as ecs]
            [czlab.mcfud.afx.ebus :as ebus]
            [czlab.mcfud.tictactoe.impl :as impl]
            [czlab.mcfud.tictactoe.misc :as mc]
            [czlab.mcfud.tictactoe.hud :as hud]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bgLayer

  ""
  []

  (do-with [layer (new js/cc.Layer)]
    (addItem layer
             (-> (sprite* (cx/gimg :game-bg))
                 (pos! (mid-rect (vrect)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- showGrid

  ""
  [state layer]

  (let [{:keys [GRID-SIZE CV-Z]} (:game @xcfg)]
    (persistent! (reduce (fn [acc mp]
                           (let [pt (mid-rect mp)
                                 sp (-> (sprite* "#z.png")
                                        (pos! (vec2->cp pt)))]
                             (addItem layer sp)
                             (conj! acc [sp pt CV-Z])))
                         (transient [])
                         (:gpos @state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- arenaLayer

  ""
  [state]

  (do-with [layer (new js/cc.Layer)]
           (swap! state
                  #(assoc % :cells (showGrid state layer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initOnce

  ""
  [state]

  (f#* (impl/init state)
       (swap! state #(assoc % :running? true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene

  ""
  [mode px py & more]

  (cx/debug* "player 1 = " (ec/clj->json px))
  (cx/debug* "player 2 = " (ec/clj->json py))
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [GRID-SIZE CV-Z]} (:game @xcfg)
          [px' & px_] px
          [py' & py_] py
          B (vrect)
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
                       :cells nil
                       :whoAmI nil
                       :whosTurn nil
                       :scores {px' 0 py' 0}
                       px' (zipmap zmks px_)
                       py' (zipmap zmks py_)})
          bl (bgLayer)
          gl (arenaLayer state)
          hud (hud/hudLayer px' py' state 0 0)]
      (reset! cx/game-scene scene)
      (reset! cx/game-arena gl)
      (addItem scene bl "bg" -1)
      (addItem scene gl "arena" 1)
      (addItem scene hud "hud" 2)
      (attr* scene
             #js{:gstate state
                 :update #(ecs/run (:ecs @state) %)})
      (ocall! scene "scheduleOnce" (initOnce state) 0.1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



