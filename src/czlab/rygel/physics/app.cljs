;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.physics.app

  (:require [czlab.rygel.physics.gui :as g]
            [czlab.mcfud.cc.ccsx :as x]
            [czlab.mcfud.afx.core :as c :refer [fn_0]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:appKey "ff0860f9-76dc-4135-8bc7-bd5af3147d55"
          :appid "physics"
          :start-scene g/splash-scene
          :game {:policy js/cc.ResolutionPolicy.FIXED_WIDTH
                 :size (x/ccr* 0 0 2048 1536)}
          :assets {:images {:lang-pics "physics/l10n/images.png"
                            :game-pics "physics/imgs/images.png"
                            :game-bg "physics/imgs/bg.png" }
                   :sheets {:lang-pics "physics/l10n/images.plist"
                            :game-pics "physics/imgs/images.plist"}
                   :fonts {:label "fnts/SmallTypeWriting.fnt"
                           :title "fnts/AutoMission.fnt"
                           :c "fnts/Subito.fnt"
                           :text "fnts/CoffeeBuzzed.fnt" }}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.____configurator
      (fn_0 (x/debug* "cc.game.____configurator called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



