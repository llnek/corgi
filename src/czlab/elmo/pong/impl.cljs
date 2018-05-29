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
                    sprite* attr*
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [half-size* *game-arena* *game-scene* *xcfg* bsize csize]]
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
(defn- addBody "" [world x y width height dyna? img type]
  (let [body (if dyna?
               (new js/cp.Body
                    1 (js/cp.momentForBox 1 width height))
               (new js/cp.Body js/Infinity js/Infinity))
        _ (ocall! body "setPos" (js/cp.v x y))
        _ (if dyna? (ocall! world "addBody" body))
        shape (new js/cp.BoxShape body width height)]
    (ocall! shape "setElasticity" 0)
    (ocall! shape "setFriction" 1)
    (ocall! shape "name" type)
    (ocall! world "addShape" shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createBall "" [state]
  (let [{:keys [BALL-SPEED]} (:game @*xcfg*)
        {:keys [ball gmode]} @state
        layer @*game-arena*
        [vx vy] (if (= gmode 3)
                  [0 0]
                  [(* BALL-SPEED (ec/randSign))
                   (* BALL-SPEED (ec/randSign))])
        sp (cx/setXXX! (sprite* "#pongball.png") {:pos ball})]
    (attr* sp #js{:vel_x vx :vel_y vy})
    (cx/addItem layer sp "ball")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createPaddles "" [state]
  (let [{:keys [P1-ICON CC-X CC-O]} (:game @*xcfg*)
        {:keys [p1 p2]} @state
        layer @*game-arena*
        [r1 r2] (if (= P1-ICON CC-X)
                  ["#red_paddle.png" "#green_paddle.png"]
                  ["#green_paddle.png" "#red_paddle.png"])
        sp1 (cx/setXXX! (sprite* r1) {:pos p1})
        sp2 (cx/setXXX! (sprite* r2) {:pos p2})]
    (cx/addItem layer sp1 "pad1")
    (cx/addItem layer sp2 "pad2")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init "" [state]
  (let [{:keys [arena gmode ebus ecs evQ]} @state
        {:keys [PADDLE-SPEED BALL-SPEED
                CV-Z CV-X CV-O
                CX CO CC-X CC-O CC-Z]} (:game @*xcfg*)
        {:keys [height width]} (cx/bbox4->bbox arena)
        {:keys [top bottom right left]} arena
        ps (bsize "#red_paddle.png")
        bs (bsize "#pongball.png")
        cp (cx/vbox4MID arena)
        kb (array)
        ;;position of paddles
        ;;portrait
        p1y (js/Math.floor (+ (* 0.1 top) (half* (:height ps))))
        p2y (js/Math.floor (- (* 0.9 top) (half* (:height ps))))
        ;;landscape
        p1x (js/Math.floor (+ left (half* (:width ps))))
        p2x (js/Math.floor (- right (half* (:width ps))))
        cb (fn [& xs] (.push evQ xs))]
    (swap! state
           #(merge % {:FPS (js/cc.director.getAnimationInterval)
                      :syncMillis 3000
                      :keyboard kb
                      :paddle (assoc ps :speed PADDLE-SPEED)
                      :ball (merge bs cp {:speed BALL-SPEED})
                      :p1Keys (if (cx/isPortrait?) [js/cc.KEY.left js/cc.KEY.right] [js/cc.KEY.down js/cc.KEY.up])
                      :p2Keys (if (cx/isPortrait?) [js/cc.KEY.a js/cc.KEY.d] [js/cc.KEY.s js/cc.KEY.w])
                      :p1 (if (cx/isPortrait?) (assoc cp :y p1y) (assoc cp :x p1x))
                      :p2 (if (cx/isPortrait?) (assoc cp :y p2y) (assoc cp :x p2x))}))
    (cx/info* "impl.init called")
    (cx/onKeyPolls kb)
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

    (createBall state)
    (createPaddles state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motionPads "" [state dt]
  (let [{:keys [keyboard p1Keys p2Keys]} @state]
    (if (aget keyword (aget p1Keys 0)

    (aget p1Keys 1)

    (aget p2Keys 0)
    (aget p2Keys 1)

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motionBall "" [state dt]
  (let [{:keys [BALL-SPEED arena]} @state
        {:keys [top right bottom left]} arena
        layer @*game-arena*
        sp (gcbyn layer "ball")
        sz (bsize sp)
        [hw hh] (half-size* sz)
        pt (ocall sp "getPosition")
        vx (oget sp "?vel_x")
        vy (oget sp "?vel_y")
        px (+ (* dt vx) (oget pt "x"))
        py (+ (* dt vy) (oget pt "y"))
        [x' vx']
        (cond
          (<= (- px hw) left) [(+ left hw) (- vx)]
          (>= (+ px hw) right) [(- right hw) (- vx)]
          :else [px vx])
        [y' vy']
        (cond
          (>= (+ py hh) top) [(- top hh) (- vy)]
          (<= (- py hh) bottom) [(+ bottom hh) (- vy)]
          :else [py vy])]
    (oset! sp "!vel_x" vx')
    (oset! sp "!vel_y" vy')
    (cx/setXXX! sp {:pos {:x x' :y y'}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motion "" [state dt]
  (motionBall state dt)
  (motionPads state dt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [dt]
  (let [state (oget @*game-scene* "gstate")]

    (motion state dt)

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
;; PreUpdate:  100, NetPlay:    200, Select:     300, Motion:     400, Move:       500, Logic:   600, Collide:  700, Resolve:    800, Render:     900


