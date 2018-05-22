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
                   [czlab.elmo.afx.ccsx :as cx :refer []])
  (:require [czlab.elmo.afx.ccsx :as cx]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.pong.splash :as splash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:appKey "fa0860f9-76dc-4135-8bc7-bd5af3147d55",
          :appid "pong"
          :game { ;:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                 :landscape? false
                 :size {:width 1536 :height 2048}
                 :BALL-SPEED 150 ;; 25 incremental
                 :PADDLE-SPEED 200 ;; 300
                 :NUM-POINTS 4
                 :GRID-W 40
                 :GRID-H 60}
          :images {:lang-pics "l10n/images.png"
                   :game-pics "imgs/images.png"
                   :gui-edit-orange "core/orange_edit.png"
                   :game-bg "imgs/bg.png" }
          :sheets {:lang-pics "l10n/images.plist"
                   :game-pics "imgs/images.plist"}
          :sounds {:game-end "snds/MineExplosion"
                   :1 "snds/ElevatorBeep"
                   :2 "snds/MineBeep"
                   :game-tie "snds/Death" }
          :fonts {:label "fnts/SmallTypeWriting.fnt"
                  :title "fnts/AutoMission.fnt"
                  :c "fnts/Subito.fnt"
                  :text "fnts/CoffeeBuzzed.fnt" }
          :startScene (f#* (splash/splashScene))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.configElmo
      (f#* (cx/info* "cc.game.configElmo called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



