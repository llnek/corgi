;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.ccsx

  (:refer-clojure :exclude [contains?])

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-x oget-y oget-piccy
                                   oget-bottom oget-right
                                   not-native? native?
                                   sprite* half*
                                   newBBox newBBox4
                                   oget-left oget-top oget-id
                                   oget-width oget-height
                                   snode? bbox? bbox4? sprite?]])
  (:require [czlab.elmo.afx.core :as ec :refer [raise!]]
            [czlab.elmo.afx.ebus :as ebus]
            [goog.object :as go]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare bbox bbox4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn contains? "" [B x & [y]]
  (let [box4 (bbox4 B)
        [px py] (if (number? y) [x y] [(oget-x x) (oget-y x)])]
    (and (>= px (oget-left box4))
         (<= px (oget-right box4))
         (>= py (oget-bottom box4))
         (<= py (oget-top box4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox->bbox4 "" [b]
  #js{:right (+ (oget-x b)(oget-width b))
      :left (oget-x b)
      :bottom (oget-y b)
      :top (+ (oget-y b)(oget-height b))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox4->bbox "" [b4]
  (js/cc.rect (oget-left b4)
              (oget-bottom b4)
              (- (oget-right b4)(oget-left b4))
              (- (oget-top b4)(oget-bottom b4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox4
  "Create a 4 point rectangle."
  [obj]
  (cond (sprite? obj)
        (bbox4 (bbox obj))
        (snode? obj)
        (bbox4 (oget-piccy obj))
        (bbox? obj)
        (bbox->bbox4 obj)
        (bbox4? obj)
        obj
        :else (raise! "bad call to bbox4")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox
  "Create a rectangle."
  [obj]
  (cond
    (sprite? obj)
    (ocall obj "getBoundingBox")
    (snode? obj)
    (bbox (oget-piccy obj))
    (bbox4? obj)
    (bbox4->bbox obj)
    (bbox? obj)
    obj
    :else (raise! "bad bbox call")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities using cc-rects.  Each entity
  wraps a sprite object."
  [a b]
  (cond (and (sprite? a) (sprite? b))
        (collide? (bbox a) (bbox b))

        (and (snode? a) (snode? b))
        (collide? (oget-piccy a)(oget-piccy b))

        (and (bbox4? a) (bbox4? b))
        (collide? (bbox4->bbox a)(bbox4->bbox b))

        (and (bbox? a) (bbox? b))
        (js/cc.rectIntersectsRect a b) :else false))

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
    (newBBox (oget-x vo) (oget-y vo) (oget-width wz) (oget-height wz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox4 "Get the visible screen box." [] (bbox->bbox4 (vbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vboxMID "" [box]
  (js/cc.p (+ (oget-x box)
              (* 0.5 (oget-width box)))
           (+ (oget-y box)
              (* 0.5 (oget-height box)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox4MID "" [box4] (vboxMID (bbox4->bbox box4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerPos "" [] (vboxMID (vbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerX
  "Get x pos of the center of the visible screen." [] (oget-x (centerPos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerY
  "Get y pos of the center of the visible screen." [] (oget-y (centerPos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenBox
  "Get the actual window/frame size."
  []
  (let [z (if (native?)
            (js/cc.view.getFrameSize)(js/cc.director.getWinSize))]
    (newBBox 0 0 (oget-width z)(oget-height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenHeight "" [] (oget-height (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenWidth "" [] (oget-width (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenCenterPos "" [] (vboxMID (screenBox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isPortrait?
  "Test if the screen is oriented vertically." []
  (let [s (screenBox)] (> (oget-height s)(oget-width s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn outOfBound?
  "Test if this entity is out of bound."
  [ent B]
  (let [a (bbox4 ent)
        b (or B (vbox))]
    (or (> (oget-left a)(oget-right b))
        (< (oget-top a)(oget-bottom b))
        (< (oget-right a)(oget-left b))
        (> (oget-bottom a)(oget-top b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn undoTimer!
  "Maybe release this timer."
  [p t]
  (if (and (native?)
           (some? t)) (ocall t "release")) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createTimer
  "Create a timer action."
  [p t]
  (do-with [rc (ocall p
                      "runAction"
                      (new js/cc.DelayTime t))] (if (native?)
                                                  (ocall rc "retain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timerDone
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
  "Find size of this thing."
  [obj]
  (cond (sprite? obj)
        (csize (bbox obj))
        (snode? obj)
        (csize (oget-piccy obj))
        (string? obj)
        (csize (sprite* obj))
        (bbox? obj)
        (js/cc.size (oget-width obj)
                    (oget-height obj))
        (bbox4? obj)
        (csize (bbox4->bbox obj))
        :else (raise! "bad call csize")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-size*
  "Calculate halves of width and height." [obj]
  (let [z (csize obj)]
    [(half* (oget-width z)) (half* (oget-height z))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getHeight "" [sprite] (oget-height (csize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledHeight
  "Get the scaled height."
  [sprite] (* (ocall sprite "getScaleY") (getHeight sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWidth "" [sprite] (oget-width (csize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledWidth
  "Get the scaled width."
  [sprite] (* (ocall sprite "getScaleX") (getWidth sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPos
  "" [sprite] (ocall sprite "getPosition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spriteBBox4 "" [obj]
  (if (or (sprite? obj)
          (snode? obj))
    (bbox4 obj)
    (raise! "bad call spriteBBox4")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spriteBBox "" [obj]
  (if (or (sprite? obj)
          (snode? obj))
    (bbox obj)
    (raise! "bad call spriteBBox")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getBottom "" [obj] (oget-y (spriteBBox obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getLeft "" [obj] (oget-x (spriteBBox obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getRight "" [obj]
  (let [b (spriteBBox obj)] (+ (oget-x b) (oget-width b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTop "" [obj]
  (let [b (spriteBBox obj)] (+ (oget-y b) (oget-height b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn traceEnclosure
  "Test if this box is hitting boundaries.
  rect.x & y are center positioned.
  If hit, the new position and velocities are returned."
  [dt bbox4 rect vel]
  (let [[sw sz] (half-size* rect)
        vx (oget-x vel)
        vy (oget-y vel)
        y (+ (oget-y rect) (* dt vy))
        x (+ (oget-x rect) (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y sz) (oget-top bbox4)) ;;hitting top wall
          [x (- (oget-top bbox4) sz) vx (- vy) true]
          (< (- y sz) (oget-bottom bbox4)) ;;hitting bottom wall
          [x (+ (oget-bottom bbox4) sz) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x sw) (oget-right bbox4)) ;;hitting right wall
          [(- (oget-right bbox4) sw) y1 (- vx1) vy1 true]
          (< (- x sw) (oget-left bbox4)) ;;hitting left wall
          [(+ (oget-left bbox4) sw) y1 (- vx1) vy1 true]
          :else
          [x1 y1 vx1 vy1 t?])]
    [x2 y2 vx2 vy2 t2?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getCachedSprite
  "Get the sprite from the frame cache using
  its id (e.g. #ship)."
  [frameid]
  (js/cc.spriteFrameCache.getSpriteFrame frameid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subEvent
  "" [e obj] (js/cc.eventManager.addListener (oset! obj "event" e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasKeyPad? "" []
  (and (not-native?)
       (some? (oget js/cc.sys.capabilities "keyboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeyPolls "" [kb]
  (if (hasKeyPad?)
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [key e] (aset kb key true))
                  :onKeyReleased (fn [key e] (aset kb key false))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeys "" [bus]
  (if (hasKeyPad?)
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [key e]
                                  (ebus/pub bus "key.down" [key e]))
                  :onKeyReleased (fn [key e]
                                   (ebus/pub bus "key.up" [key e]))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasMouse? "" [] (some? (oget js/cc.sys.capabilities "mouse")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onMouse "" [bus]
  (if (hasMouse?)
    (subEvent js/cc.EventListener.MOUSE
              #js{:onMouseMove
                  (fn [e]
                    (if (= (ocall e "getButton")
                           js/cc.EventMouse.BUTTON_LEFT)
                      (ebus/pub bus "mouse.move" e)))
                  :onMouseDown
                  (fn [e] (ebus/pub bus "mouse.down" e))
                  :onMouseUp
                  (fn [e] (ebus/pub bus "mouse.up" e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasTouch? "" [] (some? (oget js/cc.sys.capabilities "touches")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchAll "" [bus]
  (if (hasTouch?)
    (subEvent js/cc.EventListener.TOUCH_ALL_AT_ONCE
              #js{:prevTouchId -1
                  :onTouchesBegan (fn [ts e] true)
                  :onTouchesEnded
                  (fn [ts e]
                    (ebus/pub bus "touch.all.end" [ts e]))
                  :onTouchesMoved
                  (fn [ts e]
                    (this-as
                      self
                      (let [id (oget-id (aget ts 0))]
                        (if (not= (oget self "prevTouchId") id)
                          (oset! self "prevTouchId" id)
                          (ebus/pub bus "touch.all.move" [ts e])))))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchOne "" [bus]
  (if (hasTouch?)
    (subEvent js/cc.EventListener.TOUCH_ONE_BY_ONE
              #js{:swallowTouches true
                  :onTouchBegan (fn [t e] true)
                  :onTouchMoved
                  (fn [t e]
                    (ebus/pub bus "touch.one.move" [t e]))
                  :onTouchEnded
                  (fn [t e]
                    (ebus/pub bus "touch.one.end" [t e]))})))

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
(defn bmfText* "" [text font] (new js/cc.LabelBMFont text font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setXXX! "" [node & [options]]
  (let [{:keys [scale color pos anchor show?]} options]
    (if (some? anchor) (ocall node "setAnchorPoint" anchor))
    (if (some? color) (ocall node "setColor" color))
    (if (some? pos) (ocall node "setPosition" pos))
    (if-not show? (ocall node "setVisible" false))
    (if (number? scale) (ocall node "setScale" scale)) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu
  "Create a text menu containing this set of items."
  [items & [options]]

  (let [{:keys [flat? color scale anchor pos show?]} options]
    (do-with
      [menu (new js/cc.Menu)]
      (doseq [{:keys [text font cb ctx]} items
              :let [mi (new js/cc.MenuItemLabel (bmfText* text font) cb ctx)]]
        (if (some? color) (.setColor mi color))
        (.addChild menu mi))
      (setXXX! menu options)
      (if flat?
        (.alignItemsHorizontally menu)
        (.alignItemsVertically menu)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu
  "Create a menu with graphic buttons."
  [items & [options]]

  (let [{:keys [pad flat?
                pos scale anchor]} options
        padding (or pad 10)]
    (do-with
      [menu (new js/cc.Menu)]
      (doseq [{:keys [cb ctx
                      nnn sss ddd]} items
              :let [mi (new js/cc.MenuItemSprite
                            (sprite* nnn)
                            (sprite* (or sss nnn))
                            (sprite* (or ddd nnn)) cb ctx)]]
        (if (number? scale) (.setScale mi scale))
        (.addChild menu mi))
      (if flat?
        (.alignItemsHorizontallyWithPadding menu  padding)
        (.alignItemsVerticallyWithPadding menu padding)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfLabel
  "" [text font & [options]]
  (setXXX! (bmfText* text font) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


