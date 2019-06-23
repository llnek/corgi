;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.pong.app

  (:require [czlab.rygel.pong.gui :as g]
            [czlab.mcfud.afx.core :as c :refer [fn_0 fn_1]]
            [czlab.mcfud.cc.ccsx :as x :refer [CV-X CV-O xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;per second (cal'ced by reference to original pong game)
(def ^:private PADDLE-SPEED 120)
(def ^:private BALL-SPEED 204)
(def ^:private COURT-LENGTH 512)
(def ^:private PADDLE-SIZE {:height 168 :width 36})
(def ^:private BALL-SIZE {:width 36 :height 36 :radius 18})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- once-only []
  (let [[width height] (x/vrect*)
        ratio (if (> width height)
                (/ width COURT-LENGTH)
                (/ height COURT-LENGTH))
        [bv pv] (c/mapfv *
                         ratio
                         BALL-SPEED PADDLE-SPEED)]
    (swap! xcfg
           (fn_1 (update-in ____1
                            [:game]
                            #(assoc %
                                    :b-vel (x/ccp* bv bv)
                                    :p-vel (x/ccp* pv pv)
                                    :fps (js/cc.director.getAnimationInterval)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cfg {:app-key "fa0860f9-76dc-4135-8bc7-bd5af3147d55"
          :app-id "pong"
          :run-once once-only
          :start-scene g/splash-scene
          :game {;:policy js/cc.ResolutionPolicy.FIXED_HEIGHT
                 :size (x/ccr* 0 0 2048 1536)
                 :landscape? true
                 ;:landscape? false
                 ;:size (js/cc.rect 0 0 1536 2048)
                 :imap {CV-X "#red_paddle.png"
                        CV-O "#green_paddle.png"}
                 :pmap {CV-X :player CV-O :pother}
                 :kmap {CV-X [js/cc.KEY.down js/cc.KEY.up]
                        CV-O [js/cc.KEY.s js/cc.KEY.w]}
                 :kmapXXX {CV-X [js/cc.KEY.left js/cc.KEY.right]
                           CV-O [js/cc.KEY.a js/cc.KEY.d]}
                 :sync-millis 3000
                 :num-points 5
                 :player {:pvalue CV-X}
                 :pother {:pvalue CV-O}
                 :scores {CV-X 0 CV-O 0}}
          :assets {:images {:lang-pics "pong/l10n/images.png"
                            :game-pics "pong/imgs/images.png"
                            :gui-edit-orange "core/orange_edit.png"
                            :game-bg "pong/imgs/bg.png" }
                   :sheets {:lang-pics "pong/l10n/images.plist"
                            :game-pics "pong/imgs/images.plist"}
                   :sounds {:game-end "snds/MineExplosion.mp3"
                            :player "snds/ElevatorBeep.mp3"
                            :pother "snds/MineBeep.mp3"
                            :game-tie "snds/Death.mp3" }
                   :fonts {:label "fnts/SmallTypeWriting.fnt"
                           :title "fnts/AutoMission.fnt"
                           :c "fnts/Subito.fnt"
                           :text "fnts/CoffeeBuzzed.fnt"}} })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.____configurator
      (fn_0 (x/debug* "cc.game.____configurator called") cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



