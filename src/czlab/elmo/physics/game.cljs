;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.physics.game

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [_1 _2 f#* do-with]]
    [czlab.elmo.cc.ccsx :as cx :refer [sprite* attr*]])
  (:require [czlab.elmo.cc.ccsx
             :as cx :refer [*xcfg* *game-scene* *game-arena*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.physics.impl :as impl]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- arenaLayer "" [state]
  (do-with [layer (new js/cc.Layer)] ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initOnce "" [state]
  (f#* (impl/init state)
       (swap! state #(assoc % :running? true))
       (ocall! @*game-scene* "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys []} (:game @*xcfg*)
          adon? (get-in @*xcfg* [:AD :on?])
          B (if adon? (cx/ebox4) (cx/vbox4))
          state (atom {:arena B
                       :running? false})
          gl (arenaLayer state)]
      (reset! *game-scene* scene)
      (reset! *game-arena* gl)
      (cx/addItem scene gl "arena" 1)
      (attr* scene
             #js{:gstate state
                 :update impl/updateECS})
      (ocall! scene "scheduleOnce" (initOnce state) 0.1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



