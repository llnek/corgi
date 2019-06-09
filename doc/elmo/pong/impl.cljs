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
    [czlab.elmo.cc.ccsx
     :as cx :refer [oget-bottom oget-right gcbyn
                    sprite* attr* pos* pos! posX! posY!
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [half-size* *game-arena*
                    cpos bbox4
                    *game-scene* *xcfg* bsize csize]]
    [czlab.elmo.cc.dialog :as dlg]
    [czlab.elmo.pong.hud :as hud]
    [czlab.elmo.pong.misc :as mc]
    [czlab.elmo.afx.ecs :as ecs]
    [czlab.elmo.afx.ebus :as ebus]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

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
(defn- nextPoint "" [state]
  (let [{:keys [BALL-SPEED CX CO vert?]} (:game @*xcfg*)
        {:keys [arena gmode]} @state
        cp (cx/vbox4MID arena)
        layer @*game-arena*
        sp (gcbyn layer "ball")
        [vx vy] (if (= gmode 3)
                  [0 0]
                  [(* BALL-SPEED (ec/randSign))
                   (* BALL-SPEED (ec/randSign))])]
    (attr* sp #js{:vel_x vx :vel_y vy})
    (cx/setXXX! sp {:show? true :pos cp})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- wonGame "" [state who]
  (let [{:keys [pid] :as player} (get @state who)]
    (cx/sfxPlayEffect :game-end)
    (hud/writeStatus (str pid " wins!"))
    (onEnd state)
    (swap! state #(assoc % :running? false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkGame "" [state]
  (let [{:keys [NUM-POINTS CX CO vert?]} (:game @*xcfg*)
        {:keys [scores walls]} @state
        {:keys [n w e s]} walls
        layer @*game-arena*
        spb (gcbyn layer "ball")
        bb (bbox4 spb)
        winner (if vert?
                 (cond (cx/isIntersect? bb n) CX
                       (cx/isIntersect? bb s) CO)
                 (cond (cx/isIntersect? bb e) CX
                       (cx/isIntersect? bb w) CO))]
    (when (some? winner)
      (swap! state #(update-in % [:scores winner] inc))
      (cx/setXXX! spb {:show? false})
      (let [s (get-in @state [:scores winner])]
        (hud/writeScore winner s)
        (if (>= s NUM-POINTS) (wonGame state winner) (nextPoint state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onClick "" [state topic msgTopic evt]
  (let [{:keys [p2Grabbed? p1Grabbed? paddle ball]} @state
        {:keys [vert?]} (:game @*xcfg*)
        loc (ocall evt "getLocation")
        dt (ocall evt "getDelta")
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
            (cx/setXXX! sp2 {:pos {:x (if vert? (+ (oget-x pt2) (oget-x dt)) (oget-x pt2))
                                   :y (if vert? (oget-y pt2) (+ (oget-y pt2) (oget-y dt)))}}))
          (when p1Grabbed?
            (cx/setXXX! sp1 {:pos {:x (if vert? (+ (oget-x pt1) (oget-x dt)) (oget-x pt1))
                                   :y (if vert? (oget-y pt1) (+ (oget-y pt1) (oget-y dt)))}}))))))
    ;(mc/clampPaddle sp2) (mc/clampPaddle sp1)))

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
        {:keys [CX vert?]} (:game @*xcfg*)
        kb (array)
        cb (fn [& xs] (.push evQ xs))]
    (swap! state
           #(merge % {:FPS (js/cc.director.getAnimationInterval)
                      :syncMillis 3000
                      :keyboard kb
                      :p1Keys (if vert?
                                [js/cc.KEY.left js/cc.KEY.right]
                                [js/cc.KEY.down js/cc.KEY.up])
                      :p2Keys (if vert?
                                [js/cc.KEY.a js/cc.KEY.d]
                                [js/cc.KEY.s js/cc.KEY.w])}))
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
    (cx/setXXX! pad {:pos {:x x :y y}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motionPads "" [state dt]
  (let [{:keys [arena keyboard p1Keys p2Keys]} @state
        {:keys [vert? PADDLE-SPEED]} (:game @*xcfg*)
        dv (* dt PADDLE-SPEED)
        layer @*game-arena*
        p2 (gcbyn layer "pad2")
        p1 (gcbyn layer "pad1")
        deltaRight #(if vert? [(+ %1 dv) %2] [%1 (+ %2 dv)])
        deltaLeft #(if vert? [(- %1 dv) %2] [%1 (- %2 dv)])]
    (if (aget keyboard (nth p1Keys 1)) (movePad arena p1 deltaRight vert?))
    (if (aget keyboard (nth p1Keys 0)) (movePad arena p1 deltaLeft vert?))

    (if (aget keyboard (nth p2Keys 1)) (movePad arena p2 deltaRight vert?))
    (if (aget keyboard (nth p2Keys 0)) (movePad arena p2 deltaLeft vert?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motionBall "" [state dt]
  (let [sp (gcbyn @*game-arena* "ball")
        vx (oget sp "?vel_x")
        vy (oget sp "?vel_y")
        pt (pos* sp)]
    (pos! sp
          (+ (* dt vx) (oget-x pt))
          (+ (* dt vy) (oget-y pt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motion "" [state dt]
  (motionBall state dt)
  (motionPads state dt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolvePaddleHit "" [vert? sp bb bp [vx vy] ball pnum]
  (let [[hw hh] (half-size* ball)]
    (if vert?
      (do (oset! sp "!vel_y" (- vy))
          (cond (= 1 pnum) (ocall! sp "setPositionY" (+ (:top bp) hh))
                (= 2 pnum) (ocall! sp "setPositionY" (- (:bottom bp) hh))))
      (do (oset! sp "!vel_x" (- vx))
          (cond (= 1 pnum) (ocall! sp "setPositionX" (+ (:right bp) hw))
                (= 2 pnum) (ocall! sp "setPositionX" (- (:left bp) hw)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveWallHit "" [vert? sp bb bw [vx vy] ball w]
  (let [[hw hh] (half-size* ball)]
    (case  w
      (:n :s) (do (oset! sp "!vel_y" (- vy))
                  (cond (= :s w) (ocall! sp "setPositionY" (+ (:top bw) hh))
                        (= :n w) (ocall! sp "setPositionY" (- (:bottom bw) hh))))
      (:e :w) (do (oset! sp "!vel_x" (- vx))
                  (cond (= :w w) (ocall! sp "setPositionX" (+ (:right bw) hw))
                        (= :e w) (ocall! sp "setPositionX" (- (:left bw) hw))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collide "" [state]
  (let [{:keys [paddle ball walls]} @state
        {:keys [n w e s]} walls
        {:keys [vert?]} (:game @*xcfg*)
        [hw hh] (half-size* paddle)
        layer @*game-arena*
        sp1 (gcbyn layer "pad1")
        b1 (bbox4 sp1)
        sp2 (gcbyn layer "pad2")
        b2 (bbox4 sp2)
        spb (gcbyn layer "ball")
        vx (oget spb "?vel_x")
        vy (oget spb "?vel_y")
        bb (bbox4 spb)]
    ;;clamp pads
    (doseq [[sp bx] [[sp1 b1] [sp2 b2]]]
      (if vert?
        (do (if (cx/isIntersect? w bx)
              (ocall sp "setPositionX" (+ hw (:left w))))
            (if (cx/isIntersect? e bx)
              (ocall sp "setPositionX" (- (:right e) hw))))
        (do (if (cx/isIntersect? n bx)
              (ocall sp "setPositionY" (- (:bottom n) hh)))
            (if (cx/isIntersect? s bx)
              (ocall sp "setPositionY" (+ hh (:top s)))))))
    ;ball & pads
    (cond (cx/isIntersect? bb b1)
          (resolvePaddleHit vert? spb bb b1 [vx vy] ball 1)
          (cx/isIntersect? bb b2)
          (resolvePaddleHit vert? spb bb b2 [vx vy] ball 2))
    ;ball & walls
    (cond (cx/isIntersect? bb n)
          (resolveWallHit vert? spb bb n [vx vy] ball :n)
          (cx/isIntersect? bb s)
          (resolveWallHit vert? spb bb s [vx vy] ball :s))
    (cond (cx/isIntersect? bb e)
          (resolveWallHit vert? spb bb e [vx vy] ball :e)
          (cx/isIntersect? bb w)
          (resolveWallHit vert? spb bb w [vx vy] ball :w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [dt]
  (let [state (oget @*game-scene* "gstate")
        {:keys [running?]} @state]
    (when-not (false? running?)
      (motion state dt)
      (collide state)
      (checkGame state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



