;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.game

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#*]]
                   [czlab.elmo.afx.ccsx :as cx :refer []])
  (:require [czlab.elmo.afx.ccsx :as cx]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.splash :as splash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [y0 (new js/cc.Layer)
          _ (cx/addItem scene y0 -1)
          bg (sprite* (cx/getImage :game-bg))
          _ (setXXX! bg {:pos cp})
          _ (cx/addItem y0 bg)]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gameLayer "" []
  (do-with [y (new js/cc.Layer)]
    (let []
      (->>
        #js{:ebus ebus
            :init
            #(do ()
                 (cx/onTouchOne ebus)
                 (cx/onMouse ebus)) }
        (attr* y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- replay "" [gl]
  ;;request server to restart a new game?
  (if-some [w (oget gl "?____wss")]
    (odn/odinSend w {:etype *msg-session*
                     :ecode *evt-replay*})
    (play false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- play "" []

  (initPlayers)
  (initEngine)
  (initHUD csts.P1_COLOR p1ids
           csts.P2_COLOR p2ids)
  (oset! gl "!lastWinner" nil)
  (oset! gl "!runningQ" true)
  (oset! gl "!msgBuf" []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateHUD "" []
  (if (oget gl "?runningQ")
    (drawStatus hud this.actor)
    (drawResult hud this.lastWinner)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- playTimeExpired "" [gl msg]
  (if-some [buf (oget gl "?msgBuf")]
    (.push buf "forfeit")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initPlayers "" []
  (let [mode (oget gl "?game_mode")
        [p1cat p2cat] (case mode
                        3 [3 3]
                        2 [1 1]
                        1 [1 2]
                        (raise! "crap!"))
        p1 {:category p1cat
            :value csts.CV_X
            :pnum 1
            :color csts.P1_COLOR
            :offset 0}
        p2 {:category p2cat
            :value csts.CV_O
            :pnum 2
            :color csts.P2_COLOR
            :offset 1}]
    (oset! gl "!players" [nil p1 p2])
    (oset! gl "!colors"
           {:X p1 :O p2})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overAndDone "" [winner]
  (endGame hud winner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xxx "" []
  (ebus/sub+ ebus
             "hud.showmenu"
             (f#* (showMenu)))
  (ebus/sub+ ebus
             "hud.replay"
             (f#* (replay)))

  (ebus/sub+ ebus
             "hud.timer.show"
             (f#* (showTimer hud)))

  (ebus/sub+ ebus
             "net.restart"
             (f#* (killTimer hud) (play)))

  (ebus/sub+ ebus
             "net.stop"
             #(overAndDone (:status %)))

  (ebus/sub+ ebus "hud.timer.hide"
             (f#* (killTimer)))

  (ebus/sub+ ebus
             "hud.score.update"
             (fn [msg]
               (updateScore hud
                            (:color msg)
                            (:score msg))))

  (ebus/sub+ ebus "hud.end"
      #(overAndDone (:winner %)))

  (ebus/sub+ ebus
             "hud.update"
             (fn [msg]
               (update hud
                       (:runningQ msg)
                       (:pnum msg))))

  (ebus/sub+ ebus
             "player.timer.expired"
             #(playTimeExpired %))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



