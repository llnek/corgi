;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.app

  (:require [czlab.elmo.afx.ccsx :as cx]
            [czlab.elmo.afx.core :as ec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:appKey "bd5f79bb-eb41-4ed5-bb44-2529dc27ed3c"
          :appid "tictactoe"
          :game {:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                 :landscape? false
                 :size {:width 480 :height 640}}
          :csts {:PLAYER-THINK-TIME 7
                 :GRID-SIZE 3 :CELLS 9 :CV-Z 0}
          :images {:lang-pics "l10n/images.png"
                   :game-pics "imgs/images.png"
                   :gui-edit-orange "core/orange_edit.png"
                   :game-bg "imgs/bg.png" }
          :sprites {:lang-pics "l10n/images.plist"
                    :game-pics "imgs/images.plist"}
          :sounds {:game-end "snds/MineExplosion"
                   :x-pick "snds/ElevatorBeep"
                   :o-pick "snds/MineBeep"
                   :game-quit "snds/Death" }
          :fonts {:a "fnts/SmallTypeWriting.fnt"
                  :b "fnts/AutoMission.fnt"
                  :c "fnts/Subito.fnt"
                  :d "fnts/CoffeeBuzzed.fnt" }})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.configElmo
      (fn []
        (cx/info* "cc.game.configElmo called")
        cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



