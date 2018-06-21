;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.pong.app

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#*]]
                   [czlab.elmo.cc.ccsx :as cx :refer []])
  (:require [czlab.elmo.cc.ccsx :as cx :refer [*xcfg*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.pong.splash :as splash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;per second (cal'ced by reference to original pong game)
(def ^:private paddlespeed 120)
(def ^:private ballspeed 204)
(def ^:private court-length 512)
(def ^:private paddle-size {:height 168 :width 36})
(def ^:private ball-size {:width 36 :height 36 :radius 18})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onceOnly "" []
  (let [{:keys [width height]} (cx/vbox)
        ratio (if (> width height)
                (/ width court-length)
                (/ height court-length))]
    (swap! *xcfg*
           #(update-in %
                       [:game]
                       merge
                       {:BALL-SPEED (* ratio ballspeed)
                        :PADDLE-SPEED (* ratio paddlespeed)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:appKey "fa0860f9-76dc-4135-8bc7-bd5af3147d55",
          :appid "pong"
          :game { ;:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                 :landscape? true
                 :size {:width 2048 :height 1536}
                 ;:landscape? false
                 ;:size {:width 1536 :height 2048}
                 :NUM-POINTS 5
                 :P1-ICON "X"
                 :BEGIN-WITH "X"
                 :BALL-SPEED 0
                 :PADDLE-SPEED 0}
          :images {:lang-pics "pong/l10n/images.png"
                   :game-pics "pong/imgs/images.png"
                   :gui-edit-orange "core/orange_edit.png"
                   :game-bg "pong/imgs/bg.png" }
          :sheets {:lang-pics "pong/l10n/images.plist"
                   :game-pics "pong/imgs/images.plist"}
          :sounds {:game-end "snds/MineExplosion"
                   :1 "snds/ElevatorBeep"
                   :2 "snds/MineBeep"
                   :game-tie "snds/Death" }
          :fonts {:label "fnts/SmallTypeWriting.fnt"
                  :title "fnts/AutoMission.fnt"
                  :c "fnts/Subito.fnt"
                  :text "fnts/CoffeeBuzzed.fnt" }
          :runOnce onceOnly
          :startScene (f#* (splash/splashScene))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.configElmo
      (f#* (cx/info* "cc.game.configElmo called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



