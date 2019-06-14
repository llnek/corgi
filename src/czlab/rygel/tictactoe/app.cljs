;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.rygel.tictactoe.app

  (:require [czlab.mcfud.cc.ccsx :as x :refer [CV-X CV-O CV-Z]]
            [czlab.mcfud.afx.core :as c :refer [fn_0]]
            [czlab.rygel.tictactoe.gui :as g]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:appkey "bd5f79bb-eb41-4ed5-bb44-2529dc27ed3c"
          :appid "tictactoe"
          :start-scene g/splash-scene
          :game {;:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                 :pmap {CV-X :player CV-O :pother}
                 :player {:pvalue CV-X}
                 :pother {:pvalue CV-O}
                 :size (js/cc.rect 0 0 1536 2048)
                 :landscape? false
                 :begin-with CV-X
                 :grid-size 3
                 :bot-time 0.5
                 :player-time 7}
          :assets {:images {:lang-pics "tictactoe/l10n/images.png"
                            :game-pics "tictactoe/imgs/images.png"
                            :gui-edit-orange "core/orange_edit.png"
                            :game-bg "tictactoe/imgs/bg.png"}
                   :sheets {:lang-pics "tictactoe/l10n/images.plist"
                            :game-pics "tictactoe/imgs/images.plist"}
                   :sounds {:game-end "snds/MineExplosion.mp3"
                            :player "snds/ElevatorBeep.mp3"
                            :pother "snds/MineBeep.mp3"
                            :game-tie "snds/Death.mp3"}
                   :fonts {:label "fnts/SmallTypeWriting.fnt"
                           :title "fnts/AutoMission.fnt"
                           :c "fnts/Subito.fnt"
                           :text "fnts/CoffeeBuzzed.fnt"}}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.____configurator
      (fn_0 (x/debug* "cc.game.____configurator called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

