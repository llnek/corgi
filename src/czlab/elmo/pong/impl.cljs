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
    [czlab.elmo.pong.misc :as mc]
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
(defn- onClick "" [state topic msgTopic evt]
  (let [{:keys [p2Grabbed? p1Grabbed? paddle ball]} @state
        {:keys []} (:game @*xcfg*)
        loc (ocall evt "getLocation")
        dt (ocall evt "getDelta")
        p? (cx/isPortrait?)
        layer @*game-arena*
        sp2 (gcbyn layer "pad2")
        pt2 (ocall sp2 "getPosition")
        sp1 (gcbyn layer "pad1")
        pt1 (ocall sp1 "getPosition")
        r2 (ocall sp2 "getBoundingBox")
        r1 (ocall sp1 "getBoundingBox")]
    (cond
      (= msgTopic "mouse.down")
      (do (if (js/cc.rectContainsPoint r2 loc)
            (swap! state #(assoc % :p2Grabbed? true)))
          (if (js/cc.rectContainsPoint r1 loc)
            (swap! state #(assoc % :p1Grabbed? true))))
      (= msgTopic "mouse.up")
      (swap! state #(assoc % :p1Grabbed? false :p2Grabbed? false))
      (= msgTopic "mouse.move")
      (do (when p2Grabbed?
            (cx/setXXX! sp2 {:pos {:x (if p? (+ (oget-x pt2) (oget-x dt)) (oget-x pt2))
                                   :y (if p? (oget-y pt2) (+ (oget-y pt2) (oget-y dt)))}}))
          (when p1Grabbed?
            (cx/setXXX! sp1 {:pos {:x (if p? (+ (oget-x pt1) (oget-x dt)) (oget-x pt1))
                                   :y (if p? (oget-y pt1) (+ (oget-y pt1) (oget-y dt)))}}))))
    (mc/clampPaddle sp2)
    (mc/clampPaddle sp1)))

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
        {:keys [arena gmode]} @state
        cp (cx/vbox4MID arena)
        sp (sprite* "#pongball.png")
        bs (bsize sp)
        layer @*game-arena*
        [vx vy] (if (= gmode 3)
                  [0 0]
                  [(* BALL-SPEED (ec/randSign))
                   (* BALL-SPEED (ec/randSign))])]
    (attr* sp #js{:vel_x vx :vel_y vy})
    (cx/addItem layer
                (cx/setXXX! sp {:pos cp}) "ball")
    (swap! state
           #(merge % {:ball (merge bs cp {:speed BALL-SPEED})}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createPaddles "" [state]
  (let [{:keys [PADDLE-SPEED P1-ICON CC-X CC-O]} (:game @*xcfg*)
        {:keys [arena]} @state
        {:keys [top left bottom right]} arena
        cp (cx/vbox4MID arena)
        layer @*game-arena*
        [r1 r2] (if (= P1-ICON CC-X)
                  ["#red_paddle.png" "#green_paddle.png"]
                  ["#green_paddle.png" "#red_paddle.png"])
        sp1 (mc/rotFlat?? (sprite* r1))
        sp2 (mc/rotFlat?? (sprite* r2))
        ps (bsize sp1)
        ;;position of paddles, portrait
        p1y (js/Math.floor (+ bottom (half* (:height ps))))
        p2y (js/Math.floor (- top (half* (:height ps))))
        ;;landscape
        p1x (js/Math.floor (+ left (half* (:width ps))))
        p2x (js/Math.floor (- right (half* (:width ps))))
        p1 (if (cx/isPortrait?)
             (assoc cp :y p1y) (assoc cp :x p1x))
        p2 (if (cx/isPortrait?)
             (assoc cp :y p2y) (assoc cp :x p2x))]
    (cx/addItem layer (cx/setXXX! sp1 {:pos p1}) "pad1")
    (cx/addItem layer (cx/setXXX! sp2 {:pos p2}) "pad2")
    (swap! state
           #(merge % {:p1 p1 :p2 p2
                      :paddle (assoc ps :speed PADDLE-SPEED)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init "" [state]
  (let [{:keys [arena gmode ebus ecs evQ]} @state
        {:keys [CX]} (:game @*xcfg*)
        kb (array)
        cb (fn [& xs] (.push evQ xs))]
    (swap! state
           #(merge % {:FPS (js/cc.director.getAnimationInterval)
                      :syncMillis 3000
                      :keyboard kb
                      :p1Keys (if (cx/isPortrait?) [js/cc.KEY.left js/cc.KEY.right] [js/cc.KEY.down js/cc.KEY.up])
                      :p2Keys (if (cx/isPortrait?) [js/cc.KEY.a js/cc.KEY.d] [js/cc.KEY.s js/cc.KEY.w])}))
    (cx/info* "impl.init called")
    (cx/onKeyPolls kb)
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus
                 "mouse.>"
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
(defn- movePad "" [arena pad func p?]
  (let [{:keys [top right bottom left]} arena
        pt (ocall pad "getPosition")
        sz (bsize pad)
        [hw hh] (half-size* sz)
        [x y] (func (oget-x pt) (oget-y pt))
        [x' y']
        (if p?
          (cond (< (- x hw) left) [(+ left hw) y]
                (> (+ x hw) right) [(- right hw) y] :else [x y])
          (cond (< (- y hh) bottom) [x (+ bottom hh)]
                (> (+ y hh) top) [x (- top hh)] :else [x y]))]
    (cx/setXXX! pad {:pos {:x x' :y y'}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motionPads "" [state dt]
  (let [{:keys [arena keyboard p1Keys p2Keys]} @state
        {:keys [PADDLE-SPEED]} (:game @*xcfg*)
        dv (* dt PADDLE-SPEED)
        layer @*game-arena*
        p2 (gcbyn layer "pad2")
        p1 (gcbyn layer "pad1")
        p? (cx/isPortrait?)
        deltaRight #(if p? [(+ %1 dv) %2] [%1 (+ %2 dv)])
        deltaLeft #(if p? [(- %1 dv) %2] [%1 (- %2 dv)])]
    (if (aget keyboard (nth p1Keys 1)) (movePad arena p1 deltaRight p?))
    (if (aget keyboard (nth p1Keys 0)) (movePad arena p1 deltaLeft p?))

    (if (aget keyboard (nth p2Keys 1)) (movePad arena p2 deltaRight p?))
    (if (aget keyboard (nth p2Keys 0)) (movePad arena p2 deltaLeft p?))))

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


