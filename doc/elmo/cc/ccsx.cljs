;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.cc.ccsx

  (:refer-clojure :exclude [contains?])

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [each-indexed applyScalarOp
                    half* f#* n# _1 _2 do-with numStr]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [oget-x oget-y oget-piccy
                    oget-bottom oget-right
                    sprite* attr* ccmenu?
                    not-native? native?
                    gcbyn gcbyt zeropt
                    newBBox newBBox4
                    oget-width oget-height
                    oget-left oget-top oget-id
                    snode? bbox? bbox4? ccnode? sprite?]])
  (:require [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
            [czlab.elmo.afx.ebus :as ebus]
            [clojure.string :as cs]
            [goog.object :as go]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *xcfg* (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn radToDeg "" [rad] (* 180 (/ rad js/Math.PI)))
(defn degToRad "" [deg] (* deg (/ js/Math.PI 180)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x1 y1 x2 y2] {:x (- x2 x1) :y (- y2 y1)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn multVec2
  "Scalar multiplication."
  [v2 n]
  (vec2 0 0 (* n (:x v2)) (* n (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotateVec2
  "Transpose and rotate." [v2 cx cy deg]
  (let [{:keys [x y]} v2
        rad (degToRad deg)]
    (vec2 0 0
          (+ cx (- (* (js/Math.cos rad) (- x cx))
                   (* (js/Math.sin rad) (- y cy))))
          (+ cy (+ (* (js/Math.sin rad) (- x cx))
                   (* (js/Math.cos rad) (- y cy)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lengthVec2
  "Calculate the length of this vector."
  [v2] (let [{:keys [x y]} v2] (js/Math.sqrt (+ (* x x) (* y y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn plusVec2
  "Add 2 vectors together." [v1 v2]
  (vec2 0 0
        (+ (:x v1)(:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn minusVec2
  "Subtract another vector." [v1 v2]
  (vec2 0 0
        (- (:x v1)(:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare bbox bbox4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn debug* "" [& msgs] (js/cc.log (apply str msgs)))
(defn info* "" [& msgs] (js/cc.log (apply str msgs)))
(defn warn* "" [& msgs] (js/cc.log (apply str msgs)))
(defn error* "" [& msgs] (js/cc.log (apply str msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isIntersect? "" [a1 a2]
  (not (or (> (:left a1)(:right a2))
           (> (:left a2)(:right a1))
           (< (:top a1)(:bottom a2))
           (< (:top a2)(:bottom a1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn contains? "" [B x & [y]]
  (let [box4 (bbox4 B)
        [px py] (if (number? y) [x y] [(:x x) (:y x)])]
    (and (>= px (:left box4))
         (<= px (:right box4))
         (>= py (:bottom box4))
         (<= py (:top box4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox->bbox4 "" [b]
  {:right (+ (:x b)(:width b))
   :left (:x b)
   :bottom (:y b)
   :top (+ (:y b)(:height b))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox4->bbox "" [b4]
  {:x (:left b4)
   :y (:bottom b4)
   :width (- (:right b4)(:left b4))
   :height (- (:top b4)(:bottom b4))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox4
  "Create a 4 point rectangle."
  [obj]
  (cond (ccnode? obj)
        (bbox4 (bbox obj))
        (snode? obj)
        (bbox4 (oget-piccy obj))
        (bbox? obj)
        (bbox->bbox4 obj)
        (bbox4? obj)
        obj
        :else (raise! "bad call to bbox4")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- p->point
  "" [p] {:x (oget-x p) :y (oget-y p)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- point->p
  "" [pt] (js/cc.p (:x pt) (:y pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sz->size "" [sz]
  {:width (oget-width sz) :height (oget-height sz)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect->box "" [rc]
  {:x (oget-x rc)
   :y (oget-y rc)
   :width (oget-width rc)
   :height (oget-height rc)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- box->rect "" [box]
  (js/cc.rect (:x box)
              (:y box) (:width box) (:height box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox
  "Create a rectangle."
  [obj]
  (cond
    (ccnode? obj)
    (rect->box (ocall obj "getBoundingBox"))
    (snode? obj)
    (bbox (oget-piccy obj))
    (bbox4? obj)
    (bbox4->bbox obj)
    (bbox? obj)
    obj
    :else (raise! "bad bbox call")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities using cc-rects."
  [a b]
  (cond (and (ccnode? a) (ccnode? b))
        (collide? (bbox a) (bbox b))

        (and (snode? a) (snode? b))
        (collide? (oget-piccy a)(oget-piccy b))

        (and (bbox4? a) (bbox4? b))
        (collide? (bbox4->bbox a)(bbox4->bbox b))

        (and (bbox? a) (bbox? b))
        (js/cc.rectIntersectsRect (box->rect a) (box->rect b)) :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setDevRes!
  "Set device resolution, policy and orientation."
  [w h pcy] (js/cc.view.setDesignResolutionSize w h pcy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox
  "Get the visible screen rectangle."
  []
  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    (newBBox (oget-x vo)
             (oget-y vo)
             (oget-width wz)
             (oget-height wz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox4
  "Get the visible screen box." [] (bbox->bbox4 (vbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ebox "" []
  (let [{:keys [htPerc anchor]} (:AD @*xcfg*)
        vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)
        h (* (oget-height wz) htPerc)]
    (newBBox (oget-x vo)
             (+ h (oget-y vo))
             (oget-width wz)
             (- (oget-height wz) h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ebox4
  "Get the visible screen box minus ad-space." [] (bbox->bbox4 (ebox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vboxMID "" [box]
  {:x (+ (:x box) (half* (:width box)))
   :y (+ (:y box) (half* (:height box)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox4MID "" [box4] (vboxMID (bbox4->bbox box4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eboxCPoint "" [] (vboxMID (ebox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eboxCX "" [] (:x (eboxCPoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eboxCY "" [] (:y (eboxCPoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerPos "" [] (vboxMID (vbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerX
  "Get x pos of the center of the visible screen." [] (:x (centerPos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerY
  "Get y pos of the center of the visible screen." [] (:y (centerPos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenBox
  "Get the actual window/frame size."
  []
  (let [z (if (native?)
            (js/cc.view.getFrameSize)(js/cc.director.getWinSize))]
    (newBBox 0 0 (oget-width z)(oget-height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenHeight "" [] (:height (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenWidth "" [] (:width (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenCenterPos "" [] (vboxMID (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isPortrait?
  "Test if the screen is oriented vertically." []
  (let [s (screenBox)] (> (:height s)(:width s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn outOfBound?
  "Test if this entity is out of bound."
  [ent B]
  (let [b (bbox4 B)
        a (bbox4 ent)]
    (or (> (:left a)(:right b))
        (< (:top a)(:bottom b))
        (< (:right a)(:left b))
        (> (:bottom a)(:top b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn undoTimer!
  "Maybe release this timer."
  [p t]
  (if (and (native?)
           (some? t)) (ocall! t "release")) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createTimer
  "Create a timer action."
  [p t]
  (do-with [rc (ocall! p
                       "runAction"
                       (new js/cc.DelayTime t))] (if (native?)
                                                   (ocall! rc "retain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timerDone?
  "Test if this timer is done."
  [t] (and (some? t) (ocall t "isDone")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runScene
  "" [nx & [delay]]
  (js/cc.director.runScene
    (new js/cc.TransitionCrossFade (or delay 0.6) nx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isTransitioning? "" []
  (instance? js/cc.TransitionScene (js/cc.director.getRunningScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize
  "Find content-size of this thing."
  [obj]
  (if-some [z (ocall obj "getContentSize")]
    {:width (oget-width z) :height (oget-height z)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bsize
  "Find size of this thing."
  [obj]
  (cond (string? obj)
        (bsize (sprite* obj))
        (ccnode? obj)
        (bsize (bbox obj))
        (snode? obj)
        (bsize (oget-piccy obj))
        (bbox? obj)
        (dissoc obj :x :y)
        (bbox4? obj)
        (bsize (bbox4->bbox obj))
        :else (raise! "bad call bsize")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-size*
  "Calculate halves of width and height." [obj]
  (let [z (bsize obj)]
    (applyScalarOp *
                   0.5
                   (:width z) (:height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getHeight "" [sprite] (:height (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledHeight
  "Get the scaled height."
  [sprite] (* (ocall sprite "getScaleY") (getHeight sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWidth "" [sprite] (:width (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledWidth
  "Get the scaled width."
  [sprite] (* (ocall sprite "getScaleX") (getWidth sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPos
  "" [sprite] (p->point (ocall sprite "getPosition")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spriteBBox4 "" [obj]
  (if (or (ccnode? obj)
          (snode? obj))
    (bbox4 obj)
    (raise! "bad call spriteBBox4")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spriteBBox "" [obj]
  (if (or (ccnode? obj)
          (snode? obj))
    (bbox obj)
    (raise! "bad call spriteBBox")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getBottom "" [obj] (:bottom (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getLeft "" [obj] (:left (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getRight "" [obj] (:right (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTop "" [obj] (:top (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn traceEnclosure
  "Test if this box is hitting boundaries.
  If hit, the new position and velocities are returned."
  [dt bbox4 rect vel]
  (let [[hw hh] (half-size* rect)
        pt (vboxMID rect)
        vx (:x vel)
        vy (:y vel)
        y (+ (:y pt) (* dt vy))
        x (+ (:x pt) (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y hh) (:top bbox4)) ;;hitting top wall
          [x (- (:top bbox4) hh) vx (- vy) true]
          (< (- y hh) (:bottom bbox4)) ;;hitting bottom wall
          [x (+ (:bottom bbox4) hh) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x hw) (:right bbox4)) ;;hitting right wall
          [(- (:right bbox4) hw) y1 (- vx1) vy1 true]
          (< (- x hw) (:left bbox4)) ;;hitting left wall
          [(+ (:left bbox4) hw) y1 (- vx1) vy1 true]
          :else
          [x1 y1 vx1 vy1 t?])]
    [x2 y2 vx2 vy2 t2?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cc.Node stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *ws-uri* "/network/odin/websocket")
(def *game-scene* (atom nil))
(def *game-arena* (atom nil))
(def *online-game* 3)
(def *game-2* 2)
(def *game-1* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setMIFont "" [size & [name]]
  (js/cc.MenuItemFont.setFontName (or name "Arial"))
  (js/cc.MenuItemFont.setFontSize size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miFontLabel* "" [name size & [font]]
  (setMIFont size font)
  (doto (new js/cc.MenuItemFont name) (ocall! "setEnabled" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miFontItem* "" [name size & [font]]
  (setMIFont size font)
  (new js/cc.MenuItemFont name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getCachedSprite
  "Get the sprite from the frame cache using
  its id (e.g. #ship)."
  [frameid]
  (js/cc.spriteFrameCache.getSpriteFrame frameid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subEvent
  "" [e obj]
  (js/cc.eventManager.addListener (oset! obj "!event" e)
                                  (deref *game-arena*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasKeyPad? "" []
  (and (not-native?)
       (some? (oget js/cc.sys.capabilities "?keyboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeyPolls "" [kb]
  (when (hasKeyPad?)
    (info* "about to listen to key events")
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [key e] (aset kb key true))
                  :onKeyReleased (fn [key e] (aset kb key false))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeys "" [bus]
  (when (hasKeyPad?)
    (info* "about to listen to key events")
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [key e]
                                  (ebus/pub bus "key.down" key e))
                  :onKeyReleased (fn [key e]
                                   (ebus/pub bus "key.up" key e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasMouse? "" [] (some? (oget js/cc.sys.capabilities "?mouse")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onMouse "" [bus]
  (when (hasMouse?)
    (info* "about to listen to mouse events")
    (subEvent js/cc.EventListener.MOUSE
              #js{:onMouseMove
                  (fn [e]
                    (if (= (ocall e "getButton")
                           js/cc.EventMouse.BUTTON_LEFT)
                      (ebus/pub bus "mouse.move" e)))
                  :onMouseDown
                  (fn [e] (ebus/pub bus "mouse.down" e))
                  :onMouseUp
                  (fn [e] (ebus/pub bus "mouse.up" e))}) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasTouch? "" [] (some? (oget js/cc.sys.capabilities "?touches")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchAll "" [bus]
  (when (hasTouch?)
    (info* "about to listen to touch-all events")
    (let [obj #js{:onTouchesBegan (fn [ts e] true)
                  :prevTouchId -1
                  :onTouchesEnded
                  (fn [ts e]
                    (ebus/pub bus "touch.all.end" ts e))}]
      (oset! obj
             "!onTouchesMoved"
             (fn [ts e]
               (let [id (oget-id (aget ts 0))]
                 (if (not= (oget obj "?prevTouchId") id)
                   (oset! obj "!prevTouchId" id)
                   (ebus/pub bus "touch.all.move" ts e)))))
      (subEvent js/cc.EventListener.TOUCH_ALL_AT_ONCE obj)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchOne "" [bus]
  (when (hasTouch?)
    (info* "about to listen to touch-one events")
    (subEvent js/cc.EventListener.TOUCH_ONE_BY_ONE
              #js{:onTouchBegan (fn [t e] true)
                  :swallowTouches true
                  :onTouchMoved
                  (fn [t e]
                    (ebus/pub bus "touch.one.move" t e))
                  :onTouchEnded
                  (fn [t e]
                    (ebus/pub bus "touch.one.end" t e))}) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *anchor-center* 1)
(def *anchor-top* 2)
(def *anchor-top-right* 3)
(def *anchor-right* 4)
(def *anchor-bottom-right* 5)
(def *anchor-bottom* 6)
(def *anchor-bottom-left* 7)
(def *anchor-left* 8)
(def *anchor-top-left* 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getAnchorPoint "" [a]
  (condp = a
    *anchor-center* (js/cc.p 0.5 0.5)
    *anchor-top* (js/cc.p 0.5 1)
    *anchor-top-right* (js/cc.p 1 1)
    *anchor-right* (js/cc.p 1 0.5)
    *anchor-bottom-right* (js/cc.p 1 0)
    *anchor-bottom* (js/cc.p 0.5 0)
    *anchor-bottom-left* (js/cc.p 0 0)
    *anchor-left* (js/cc.p 0 0.5)
    *anchor-top-left* (js/cc.p 0 1)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run* "" [s] (js/cc.director.runScene s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeAll! "" [node] (ocall! node "removeAllChildren"))
(defn remove! "" [child] (ocall! child "removeFromParent"))
;(defn gcbyn "" [p n] (ocall p "getChildByName" n))
;(defn gcbyt "" [p t] (ocall p "getChildByTag" t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cpos "" [node]
  (let [pt (ocall node "getPosition")]
    {:x (oget-x pt) :y (oget-y pt)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn colorLayer*
  "" [r b g & [a]] (new js/cc.LayerColor (js/cc.color r b g a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addItem "" [node child & [tag zOrder]]
  (if (and (instance? js/cc.SpriteBatchNode node)
           (sprite? child))
      (ocall! child "setBatchNode" node))
  (ocall! node
          "addChild"
          child
          (if (number? zOrder) zOrder js/undefined)
          (if (or (string? tag)(number? tag)) tag js/undefined))
  child)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn batchNode* "" [img]
  (new js/cc.SpriteBatchNode
       (js/cc.textureCache.addImage img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addSpriteFrames* "" [plist]
  (js/cc.spriteFrameCache.addSpriteFrames plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfText* "" [text font] (new js/cc.LabelBMFont text font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setXXX! "" [node & [options]]
  (let [{:keys [scale color pos anchor show?] :or {show? true}} options]
    (if (some? anchor) (ocall! node "setAnchorPoint" (getAnchorPoint anchor)))
    (if (some? color) (ocall! node "setColor" color))
    (if (some? pos)
      (ocall! node "setPosition" (point->p pos)))
    (ocall! node "setVisible" show?)
    (if (number? scale) (ocall! node "setScale" scale)) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn misprite* "" [nnn cb & [sss ddd ctx]]
  (new js/cc.MenuItemSprite (sprite* nnn)
                            (sprite* (or sss nnn))
                            (sprite* (or ddd nnn)) (or cb noopy) ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mitoggle* "" [on off cb] (new js/cc.MenuItemToggle on off cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pegMenu?? "" [menu tag region anchor total padding flat?]
  (if (some? anchor)
    (let [{:keys [top left bottom right] :as B} (or region (vbox4))
          {:keys [width height]}
          (bsize (gcbyt menu tag))
          t (+ (* padding (dec total))
               (* total (if flat? width height)))
          [w h] (if flat? [t height] [width t])
          cp (vbox4MID B)
          cx (:x cp)
          cy (:y cp)
          hw (half* w)
          hh (half* h)
          pt
          (condp = anchor
            *anchor-top-left* {:x (+ left hw) :y (- top hh)}
            *anchor-top* {:x cx :y (- top hh)}
            *anchor-top-right* {:x (- right hw) :y (- top hh)}

            *anchor-left*  {:x (+ left hw) :y cy}
            *anchor-center* cp
            *anchor-right* {:x (- right hw) :y cy}

            *anchor-bottom-left* {:x (+ left hw) :y (+ bottom hh)}
            *anchor-bottom* {:x cx :y (+ bottom hh)}
            *anchor-bottom-right* {:x (- right hw) :y (+ bottom hh)}
            nil)]
      (setXXX! menu {:pos pt}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu
  "Create a text menu containing this set of items."
  [items & [options]]

  (let [{:keys [scale region anchor flat?
                align? color padding]
         :or {align? true padding 10}}
        options
        tag 911]
    (do-with
      [menu (new js/cc.Menu)]
      (each-indexed
        (fn [obj pos]
          (let [{:keys [text font cb ctx]} obj
                mi (new js/cc.MenuItemLabel
                        (bmfText* text font) cb ctx)]
            (setXXX! mi {:scale scale :color color})
            (addItem menu mi (+ pos tag))))
        items)
      (setXXX! menu (dissoc options :scale :color))
      (if align?
        (if flat?
          (ocall menu "alignItemsHorizontallyWithPadding" padding)
          (ocall menu "alignItemsVerticallyWithPadding" padding)))
      (pegMenu?? menu tag region anchor (n# items) padding flat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu
  "Create a menu with graphic buttons."
  [items & [options]]

  (let [{:keys [scale align? region anchor padding flat?]
         :or {align? true padding 10}}
        options
        tag 911]
    (do-with
      [menu (new js/cc.Menu)]
      (each-indexed
        (fn [obj pos]
          (let [{:keys [cb ctx nnn sss ddd]} obj
                mi (misprite* nnn cb sss ddd ctx)]
            (setXXX! mi {:scale scale})
            (addItem menu mi (+ pos tag))))
        items)
      (setXXX! menu (dissoc options :scale))
      (if align?
        (if flat?
          (ocall menu "alignItemsHorizontallyWithPadding" padding)
          (ocall menu "alignItemsVerticallyWithPadding" padding)))
      (pegMenu?? menu tag region anchor (n# items) padding flat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfLabel
  "" [text font & [options]]
  (setXXX! (bmfText* text font) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getCfgXXX "" [& path] (get-in @*xcfg* path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10nInit
  "Initialize the l10n module with the string table."
  []
  (let [lang (keyword js/cc.sys.language)]
    (info* "Loaded l10n strings. locale = " (name lang))
    (info* (js/JSON.stringify
             (clj->js (getCfgXXX :l10nTable lang))))
    (swap! *xcfg* #(assoc % :lang lang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10n
  "Localize the string." [key & pms]
  (let [table (getCfgXXX :l10nTable
                         (:lang @*xcfg*))
        msg (get table key)]
    (if (not-empty msg)
      (if (not-empty pms)
        (let [arr (cs/split msg #"\{\}")
              sz (n# arr)
              end (dec sz)
              plen (n# pms)
              out (transient [])]
          (dotimes [i sz]
            (conj! out (nth arr i))
            (if (and (< i end)
                     (< i plen))
              (conj! out (nth pms i))))
          (cs/join "" (persistent! out))) msg) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fire! "" [topic & args]
  (if-some [g (deref *game-arena*)]
    (if-some [bus (oget g "?ebus")]
      (apply ebus/pub bus (concat [topic] args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcXY
  "Find the corresponding x, y lengths based on the
  provided angle and length of the hypotenuse.
  quadrants =  4 | 1
               -----
               3 | 2"
  [angle hypot]
  (let [[t fx fy q]
        (cond
          (and (>= angle 0)(<= angle 90))
          [(degToRad (- 90 angle))
           js/Math.cos js/Math.sin 1]
          (and (>= angle 90)(<= angle 180))
          [(degToRad (- angle 90))
           js/Math.cos #(* -1 (js/Math.sin %)) 2]
          (and (>= angle 180)(<= angle 270))
          [(degToRad (- 270 angle))
           #(* -1 (js/Math.cos %))
           #(* -1 (js/Math.sin %)) 3]
          (and (>= angle 270)(<= angle 360))
          [(degToRad (- angle 270))
           #(* -1 (js/Math.cos %)) js/Math.sin 4]
          :else (raise! "bad calcXY call"))]
    [(* hypot (fx t)) (* hypot (fy t)) q]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn normalizeDeg
  "Normalize the degree - modulo 360." [deg]
    (if (> deg 360)
      (xmod deg 360)
      (if (< deg 0)
        (- 360 (xmod (- deg) 360)) deg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitizeUrlForDevice "" [url] url)
(defn sanitizeUrlForWeb "" [url] url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fixUrl
  "Sanitize this url differently for web and for devices." [url]
  (if (not-native?)
    (sanitizeUrlForWeb url) (sanitizeUrlForDevice url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->logo "" []
  (str "res/" (getCfgXXX :loader ::czlab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->pbar "" []
  (str "res/" (getCfgXXX :loader ::preloader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->imgs "" [] [(gldr->logo) (gldr->pbar)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gatlas "" [key] (str "res/" (getCfgXXX :sheets key)))
(defn gatlas->img "" [key] (cs/replace (gatlas key) #"\.plist$" ".png"))

(defn gimg "" [key] (str "res/" (getCfgXXX :images key)))
(defn gtile "" [key] (str "res/" (getCfgXXX :tiles key)))

(defn gfnt "" [key] (str "res/" (getCfgXXX :fonts key)))
(defn gfnt->img "" [key] (cs/replace (gfnt key) #"\.fnt$" ".png"))

(defn gsfx "" [key]
  (->> (getCfgXXX :game :sfx)
       (name)
       (str "res/" (getCfgXXX :sounds key) ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setSfx! "" [v]
  (swap! *xcfg* #(assoc-in % [:audio :open?] v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggleSfx! "" []
  (swap! *xcfg*
         #(update-in %
                     [:audio :open?] (fn [b] (not b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxOn? "" [] (getCfgXXX :audio :open?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxMusicVol "" [vol]
  (if (number? vol) (js/cc.audioEngine.setMusicVolume vol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayMusic "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playMusic (gsfx key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayEffect "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playEffect (gsfx key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxCancel! "" []
  (js/cc.audioEngine.stopMusic)
  (js/cc.audioEngine.stopAllEffects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxInit "" []
  (sfxMusicVol (getCfgXXX :audio :volume)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preCacheAtlases "" []
  (doseq [[k _] (:sheets @*xcfg*)]
    (->> (gatlas  k)
         (js/cc.spriteFrameCache.addSpriteFrames ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xliveIcon "" [img & [options]]
  (do-with [s (sprite* img)] (setXXX! s options)))

;create() { const dummy = new XLive(0,0,this.options); this.lifeSize = { width: ccsx.getScaledWidth(dummy), height: ccsx.getScaledHeight(dummy) } ; this.drawLives(); }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xliveGroup "" [hud img total x y direction]
  (atom {::topLeft {:x x :y y}
         ::image img
         ::totalLives total
         ::icons [] ::parent hud ::curLives total ::dir direction}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reduceOneLive "" [g]
  (swap! g
         (fn [{:keys [::parent ::icons ::curLives] :as root}]
           (let [e (peek icons)
                 c (pop icons)
                 n (dec curLives)]
             (remove! e)
             (merge root {::icons c ::curLives n})))) g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countLiveIcons "" [g] (::curLives @g))
(defn noLiveIcons?
  "" [g] (pos? (countLiveIcons g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resetLiveGroup "" [g]
  (swap! g
         (fn [{:keys [::parent ::curLives ::totalLives ::icons] :as root}]
           (doseq [x icons] (remove! x))
           (merge root {::curLives totalLives ::icons []}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawLiveIcons "" [g]
  (swap!
    g
    (fn [{:keys [::parent ::topLeft
                 ::curLives ::dir ::image] :as root}]
      (let
        [sz (bsize image)
         h (:height sz)
         w (:width sz)
         [hw hh] (half-size* sz)
         icons
         (loop [n 0 arr []
                y (- (:y topLeft) hh)
                x (+ (:x topLeft) hw)]
           (if-not (< n curLives)
             arr
             (let [v (xliveIcon image
                                {:pos {:x x :y y}})]
               (addItem parent v)
               (recur (inc n) (conj arr v)
                      y (if (pos? dir) (+ x w) (- x w))))))]
        (merge root {::icons icons})))) g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn audioIcon "" [yes no & [options]]
  (let
    [cb #(setSfx!
           (zero? (ocall % "getSelectedIndex")))
     off (misprite* no nil)
     on (misprite* yes nil)
     audio (mitoggle* on off cb)]
    (ocall! audio
            "setSelectedIndex" (if (sfxOn?) 0 1))
    (setXXX! audio options)
    (new js/cc.Menu audio)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn quit! "" [ctor]
  (js/cc.director.pushScene
    (ctor {:no #(js/cc.director.popScene)
           :yes (fn [start]
                  (js/cc.director.popToRootScene)
                  (runScene start))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerImage "" [node frame]
  (do-with [bg (sprite* frame)]
           (setXXX! bg {:pos (centerPos)}) (addItem node bg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateScore "" [node score]
  (ocall! node "setString" (numStr score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pegToAnchor "" [node where B]
  (let [{:keys [top right bottom left]} B
        {:keys [x y] :as cp} (vbox4MID B)
        pt
        (condp = where
          *anchor-top-left* {:x left :y top}
          *anchor-top* {:x x  :y top}
          *anchor-top-right* {:x right :y top}
          *anchor-left* {:x left :y y}
          *anchor-center* cp
          *anchor-right* {:x right :y y}
          *anchor-bottom-left* {:x left :y bottom}
          *anchor-bottom* {:x x :y bottom}
          *anchor-bottom-right* {:x right :y bottom}
          (raise! "pegToAnchor bad where = " where))]
    (setXXX! node {:pos pt})))

;import Cookies from 'Cookies';
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkScore
  "" [n v] {::value (js/Number (cs/trim v)) ::name (cs/trim n) })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn readHighScores "" [hs]
  (let [{:keys [KEY]} @hs
        s (cs/trim (or (js/Cookies.get KEY) ""))]
    (swap! hs
           #(assoc %
                   ::scores
                   (reduce
                     (fn [acc z]
                       (let [a (cs/split (or z "") ":")]
                         (if (= 2 (n# a))
                           (conj acc (mkScore (_1 a) (_2 a))) acc)))
                     []
                     (cs/split s "|")))) hs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resetHighScores! "" [hg]
  (swap! hg #(assoc % ::scores [])) hg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn writeHighScores! "" [hg]
  (let [{:keys [::KEY ::scores ::duration]} @hg]
    (js/Cookies.set KEY
                    (->> (map #(str (::name %)
                                    ":"
                                    (::value %)) scores)
                         (cs/join "|"))
                    duration)
    hg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hasSlotsHighScores?
   "Test if there is more room to store a new high score." [hg]
   (let [{:keys [::scores ::size]} @hg] (< (n# scores) size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn canAddHighScores?
  "Test if we can add this score to the list of highscores."
  [hg score]

  (or (hasSlotsHighScores? hg)
      (some #(< (::value %) score) (::scores @hg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insertHighScores! "" [hg name score]
  (let [s (mkScore (or name "???") score)
        {:keys [::scores]} @hg
        len (n# scores)]
    (when-not (hasSlotsHighScores? hg)
      (loop [i (dec len) arr nil]
        (if (some? arr)
          (swap! hg #(assoc % ::scores (js->clj arr)))
          (if-not (neg? i)
            (recur (dec i)
                   (if (< (::value (nth scores i)) score)
                     (.splice (clj->js scores) i 1)))))))
    (when (hasSlotsHighScores? hg)
      (swap! hg
             (fn [{:keys [::scores] :as root}]
               (assoc root
                      ::scores
                      (sort (fn [a b]
                              (cond (< (::value a) (::value b)) -1
                                    (> (::value a) (::value b)) 1 :else 0))
                            (conj scores s)))))
      (writeHighScores! hg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmtHighScores "" [key size & [duration]]
  {::duration (or duration (* 60 60 24 1000)) ::size size ::scores [] ::KEY key })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *CHUNK* 36)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadImages "" []
  (reduce #(conj %1 (gimg %2)) [] (keys (:images @*xcfg*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadTiles "" []
  (reduce #(conj %1 (gtile %2)) [] (keys (:tiles @*xcfg*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadSounds "" []
  (reduce #(conj %1 (gsfx %2)) [] (keys (:sounds @*xcfg*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadFonts "" []
  (reduce (fn [acc k]
            (conj acc
                  (gfnt->img k)
                  (gfnt k))) [] (keys (:fonts @*xcfg*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadSheets "" []
  (reduce (fn [acc k]
            (conj acc
                  (gatlas->img k)
                  (gatlas k)))
          []
          (keys (:sheets @*xcfg*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gatherPreloads "" []
  (concat (preloadImages)
          (preloadTiles)
          (preloadFonts)
          (preloadSounds)
          (preloadSheets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn niceFadeOut
  "After loading resources, runs the first scene." [scene]
  (let [{:keys [runOnce startScene]} @*xcfg*
        bar (gcbyn scene "progress")
        logo (gcbyn scene "logo")
        target nil
        selector #(do (preCacheAtlases)
                      (runOnce)
                      (run* (startScene)))]
    (info* "fade out! run next scene!!!!!")
    (ocall! scene "unscheduleUpdate")
    (remove! bar)
    (ocall! logo
           "runAction"
           (js/cc.Sequence.create
             (js/cc.FadeOut.create 1.2)
             (js/cc.CallFunc.create selector target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn loadChunk
  "We have to load chunk by chunk because
  the array of resources can't be too big, else jsb complains"
  [scene assets state]
  (let [s (aget state 1)
        e (aget state 2)
        arr (.slice assets s e)]
    ;(info* "start s = " s ", e = " e)
    ;(info* (js/JSON.stringify #js{:arr assets}))
    (if (pos? (n# arr))
      (js/cc.loader.load arr
                         (fn [result total cnt]
                           ;(info* "total = " total ", cnt = " cnt)
                           (aset state 0 (+ 1 (aget state 0))))
                         (fn []
                           ;(info* "done = " (aget state 0))
                           nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pkLoadMore "" [scene assets state]
  (f#*
    (let [pg (gcbyn scene "progress")
          cnt (aget state 0)
          len (n# assets)
          ratio (/ cnt len)
          perc (min (* ratio 100) 100)]
      (ocall! pg "setPercentage" perc)
      (if (< cnt len)
        (let [head (aget state 2) ;get last tail
              tail (+ head (min *CHUNK* (- len head)))]
          (aset state 1 head)
          (aset state 2 tail)
          (loadChunk scene assets state))
        (do (info* "all resources loaded!") (niceFadeOut scene))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pkLoad "" [scene assets state]
  (let [logo (sprite* (gldr->logo))
        sz (bsize logo)
        cp (centerPos)
        pg (new js/cc.ProgressTimer
                (sprite* (gldr->pbar)))]
    (setXXX! logo {:pos cp})
    (addItem scene logo "logo")
    (ocall! pg "setType" js/cc.ProgressTimer.TYPE_BAR)
    (ocall! pg "setScaleX" 0.8)
    (ocall! pg "setScaleY" 0.3)
    (setXXX! pg {:pos {:x (:x cp)
                       :y (- (:y cp)
                             (* 0.6 (:height sz)))}})
    (addItem scene pg "progress")
    (attr* scene #js{:update
                     (pkLoadMore scene (clj->js assets) state)})
    (ocall! scene "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn loaderScene* "" []
  (do-with [scene (new js/cc.Scene)]
    (let [assets (gatherPreloads)
          y (colorLayer* 0 0 0)
          ;;[count, head, tail] snapshot info used by
          ;;each iteration as we chunk up the unput
          state (array 0 0 0)
          func (f#* (pkLoad scene assets state))]
      (setXXX! y {:pos (zeropt)})
      (addItem scene y "bgLayer" -1)
      (attr* scene
             #js{:onEnter
                 #(do (.call js/cc.Node.prototype.onEnter scene)
                      (ocall! scene "scheduleOnce" func 0.3))
                 :onExit
                 #(.call js/cc.Node.prototype.onExit scene)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn preloader "" []
  (f#* (info* "start to run asset preloader.") (run* (loaderScene*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn enableAD! "" [scene]
  (let [{:keys [htPerc anchor]} (:AD @*xcfg*)
        {:keys [height width]} (vbox)
        {:keys [right left bottom]} (vbox4)
        h (* height htPerc)
        n (new js/cc.DrawNode)]
    (ocall! n
            "drawRect"
            (js/cc.p left bottom)
            (js/cc.p right (+ bottom h))
            (js/cc.color 123 222 187)
            nil
            (js/cc.color 255 255 255))
    n))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patch the config object!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! *xcfg*
        {:urlPrefix "/public/elmo/"
         :trackingID ""
         :version ""
         :appid ""
         :color (js/cc.color 0 0 0)
         :levels {}
         :loader {::czlab "core/ZotohLab.png"
                  ::preloader "core/preloader_bar.png"}
         :images {}
         :sheets {}
         :tiles {}
         :sounds {}
         :fonts {}
         :AD {:anchor *anchor-bottom*
              :on? true
              :htPerc 0.09 }
         :game {:policy js/cc.ResolutionPolicy.SHOW_ALL
                :preloadLevels? true
                :size {:width 0 :height 0}
                :resDir ""
                :landscape? true
                :CV-X 88 ;(.charCodeAt "X" 0)
                :CV-O 79 ;(.charCodeAt "0" 0)
                :CC-O "O"
                :CC-X "X"
                :CC-Z "?"
                :CX :1
                :CO :2
                :NETP 3
                :HUMAN 1
                :BOT 2
                :TILE 8
                :S-OFF 4
                :GAME-ID ""
                :scale 1
                :sfx :mp3
                :gravity 0 }
         :l10nTable {:en {"%mobileStart" "Press Anywhere To Start!"
                          "%webStart" "Press Spacebar To Start!"
                          "%passwd" "Password"
                          "%userid" "UserId"
                          "%player2" "Player 2"
                          "%player1" "Player 1"
                          "%computer" "Computer"
                          "%cpu" "P2"
                          "%p2" "P2"
                          "%p1" "P1"
                          "%2players" "2 Player"
                          "%1player" "1 Player"
                          "%online" "Online"
                          "%gameover" "Game Over"
                          "%quit!" "Quit"
                          "%back" "Back"
                          "%ok" "OK"
                          "%mmenu" "Main Menu"
                          "%options" "Options"
                          "%replay" "REPLAY"
                          "%play" "PLAY"
                          "%waitothers" "Waiting...\nfor other players."
                          "%waitother" "Waiting...\nfor another player."
                          "%signinplay" "Please sign in to play."
                          "%quit?" "Continue and quit game?" }}
         :audio {:volume 0.5
                 :open? true
                 :track nil }
         :runOnce noopy
         :startScene noopy})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

