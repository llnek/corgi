;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.pong.impl

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [do->true half*
                    nneg? f#* n# _1 _2 do-with]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-bottom oget-right gcbyn
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [*game-arena* *game-scene* *xcfg* csize]]
    [czlab.elmo.afx.dialog :as dlg]
    [czlab.elmo.pong.hud :as hud]
    [czlab.elmo.afx.ecs :as ecs]
    [czlab.elmo.afx.ebus :as ebus]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare processCell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAI "" [state]

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onEnd "" [state]
  (let [{:keys [startScene]} @*xcfg*
        scene @*game-scene*
        arena @*game-arena*
        hud (gcbyn scene "hud")]
    (js/cc.eventManager.pauseTarget arena true)
    (js/cc.eventManager.pauseTarget hud true)
    (->> {:msg "Play Again?"
          :yes #(cx/run* (startScene))
          :cleanup #(do (js/cc.eventManager.resumeTarget arena true)
                        (js/cc.eventManager.resumeTarget hud true))}
         (dlg/popDlg scene ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- tieGame "" [state]
  (hud/writeStatus "It's a draw!")
  (cx/sfxPlayEffect :game-tie)
  (onEnd state)
  (swap! state #(assoc % :running? false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- wonGame "" [state who player]
  (let [s (get-in @state [:scores who])]
    (cx/sfxPlayEffect :game-end)
    (hud/writeScore who (inc s))
    (hud/writeStatus (str (get player :pid) " wins!"))
    (onEnd state)
    (swap! state #(-> (assoc % :running? false)
                      (update-in [:scores who] inc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkGameState "" [state player]

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateArena "" [state player cell]

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- processCell "" [state cell]
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onClick "" [state topic msgTopic & msgs]
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onTouch "" [state topic msgTopic & msgs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [state]
  (let [{:keys [gmode ebus ecs evQ]} @state
        {:keys [CV-Z CV-X CV-O CX CO CC-X CC-O CC-Z]} (:game @*xcfg*)
        cb (fn [& xs] (.push evQ xs))]
    (cx/info* "impl.init called")
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus
                 "mouse.up"
                 (fn [& xs] (apply onClick (concat [state] xs)))))
    (if (cx/onTouchOne ebus)
      (ebus/sub+ ebus
                 "touch.one.end"
                 (fn [& xs] (apply onTouch (concat [state] xs)))))

    ;always player 1 for mode 1, and create the bot
    (when (= 1 gmode)
      (swap! state #(assoc %
                           :whoAmI CX
                           :bot nil)))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(defn- xxx "" [gmode B]
  (let [{:keys [PADDLE-SPEED BALL-SPEED]} (:game @*xcfg*)
        {:keys [height width]} (cx/box4->box B)
        {:keys [top bottom right left]} B
        ps (csize "#red_paddle.png")
        bs (csize "#pongball.png")
        cp (cx/vbox4MID B)
        ;;position of paddles
        ;;portrait
        p1y (js/Math.floor (+ (* 0.1 top) (half* (:height ps))))
        p2y (js/Math.floor (- (* 0.9 top) (half* (:height ps))))
        ;;landscape
        p1x (js/Math.floor (+ left (half* (:width ps))))
        p2x (js/Math.floor (- right (half* (:width ps))))
        state {:FPS (js/cc.director.getAnimationInterval)
               :syncMillis 3000
               :paddle (assoc ps :speed PADDLE-SPEED)
               :ball (merge bs cp {:speed BALL-SPEED})
               :p1 (if (cx/isPortrait?)
                     (assoc cp :y p1y)
                     (assoc cp :x p1x))
               :p2 (if (cx/isPortrait?)
                     (assoc cp :y p2y)
                     (assoc cp :x p2x))}]
    (createPaddles state)
    (createBall state)
    ;; mouse only for 1p or netplay
    (if (not= gmode 2) (onMouse state))
    (cx/onTouchOne state)
    (cx/onKeys state)))


(defn- createPaddles "" [layer state]
  (let [{:keys [P1-ICON CC-X CC-O]} (:game @*xcfg*)
        {:keys [p1 p2]} @state
        [r1 r2] (if (= P1-ICON CC-X)
                  ["#red_paddle.png" "#green_paddle.png"]
                  ["#green_paddle.png" "#red_paddle.png"])
        sp1 (cx/setXXX! (sprite* r1) {:pos p1})
        sp2 (cx/setXXX! (sprite* r2) {:pos p2})]
    (cx/addItem layer sp1)
    (cx/addItem layer sp2)))

(defn- createBall "" [layer state]
  (let [{:keys [BALL-SPEED]} (:game @*xcfg*)
        [vx vy] (if (= gmode 3)
                  [0 0]
                  [(* BALL-SPEED (ec/randSign))
                   (* BALL-SPEED (ec/randSign))])
        sp (cx/setXXX! (sprite* "#pongball.png") {:pos ball})]
    (cx/addItem layer sp)))

