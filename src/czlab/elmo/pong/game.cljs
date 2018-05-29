;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.pong.game

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [_1 _2 f#* do-with]]
    [czlab.elmo.afx.ccsx :as cx :refer [sprite* attr*]])
  (:require [czlab.elmo.afx.ccsx
             :as cx :refer [*xcfg* *game-scene* *game-arena*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.afx.ecs :as ecs]
            [czlab.elmo.afx.ebus :as ebus]
            [czlab.elmo.pong.hud :as hud]
            [czlab.elmo.pong.impl :as impl]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bgLayer "" []
  (do-with [layer (new js/cc.Layer)]
    (cx/addItem layer
                (-> (sprite* (cx/gimg :game-bg))
                    (cx/setXXX! {:pos (cx/centerPos)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- arenaLayer "" [state]
  (do-with [layer (new js/cc.Layer)]
    (let [WHITE (js/cc.color 255 255 255)
          {:keys [arena]} @state
          {:keys [top left right bottom]} arena
          {:keys [x y] :as cp } (cx/vbox4MID arena)
          r (new js/cc.DrawNode)]
      (ocall! r
              "drawRect"
              (js/cc.p left bottom) (js/cc.p right top) nil 64 WHITE)
      (ocall! r "drawSegment" (js/cc.p left y ) (js/cc.p right y) 16 WHITE)
      (cx/addItem layer r "border" -1)

           )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initOnce "" [state]
  (f#* (impl/init state)
       (swap! state #(assoc % :running? true))
       (ocall! @*game-scene* "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" [mode px py & more]
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys []} (:game @*xcfg*)
          adon? (get-in @*xcfg* [:AD :on?])
          B (if adon? (cx/ebox4) (cx/vbox4))
          zmks [:ptype :pvalue :pcolor :pid :pname]
          state (atom {:ebus (ebus/createEvBus)
                       :ecs (ecs/createECS)
                       :arena B
                       :running? false
                       :gmode mode
                       :evQ (array)
                       :whoAmI nil
                       :scores {(_1 px) 0 (_1 py) 0}
                       (_1 px) (zipmap zmks (rest px))
                       (_1 py) (zipmap zmks (rest py))})
          bl (bgLayer)
          gl (arenaLayer state)
          hud (hud/hudLayer (_1 px) (_1 py) state 0 0)]
      (reset! *game-scene* scene)
      (reset! *game-arena* gl)
      (cx/addItem scene bl "bg" -1)
      (cx/addItem scene gl "arena" 1)
      (cx/addItem scene hud "hud" 2)
      (attr* scene
             #js{:gstate state
                 :update impl/updateECS})
      (when adon?
        (let [a (cx/enableAD! scene)]
          (cx/addItem hud a)))
      (ocall! scene "scheduleOnce" (initOnce state) 0.1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



