;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.mcfud.tictactoe.app

  (:require-macros [czlab.mcfud.afx.core :as ec :refer [f#*]]
                   [czlab.mcfud.cc.ccsx :as cx :refer []])

  (:require [czlab.mcfud.cc.ccsx :as cx]
            [czlab.mcfud.afx.core :as ec]
            [czlab.mcfud.tictactoe.splash :as splash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private cfg {:appKey "bd5f79bb-eb41-4ed5-bb44-2529dc27ed3c"
                    :appid "tictactoe"
                    :game {;:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                           :landscape? false
                           :CV-Z 0
                           :size {:wide 1536 :tall 2048}
                           :P1-ICON "X"
                           :BEGIN-WITH "X"
                           :PLAYER-THINK-TIME 7
                           :BOT-THINK-TIME 0.5
                           :GRID-SIZE 3}
                    :images {:lang-pics "tictactoe/l10n/images.png"
                             :game-pics "tictactoe/imgs/images.png"
                             :gui-edit-orange "core/orange_edit.png"
                             :game-bg "tictactoe/imgs/bg.png"}
                    :sheets {:lang-pics "tictactoe/l10n/images.plist"
                             :game-pics "tictactoe/imgs/images.plist"}
                    :sounds {:game-end "snds/MineExplosion"
                             :1 "snds/ElevatorBeep"
                             :2 "snds/MineBeep"
                             :game-tie "snds/Death"}
                    :fonts {:label "fnts/SmallTypeWriting.fnt"
                            :title "fnts/AutoMission.fnt"
                            :c "fnts/Subito.fnt"
                            :text "fnts/CoffeeBuzzed.fnt"}
                    :startScene (f#* (splash/splashScene))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.____configurator
      (f#* (cx/debug* "cc.game.____configurator called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



