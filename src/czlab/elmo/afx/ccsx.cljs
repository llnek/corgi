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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#* n# _1 _2 do-with numStr]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-x oget-y oget-piccy
                                   oget-bottom oget-right
                                   not-native? native?
                                   gcbyn gcbyt
                                   zeropt
                                   sprite* half* attr*
                                   newBBox newBBox4
                                   oget-left oget-top oget-id
                                   oget-width oget-height
                                   snode? bbox? bbox4? sprite?]])
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
  (not (or (> (oget-left a1)(oget-right a2))
           (> (oget-left a2)(oget-right a1))
           (< (oget-top a1)(oget-bottom a2))
           (< (oget-top a2)(oget-bottom a1)))))

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
  "Test collision of 2 entities using cc-rects."
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
  (let [b (bbox4 B)
        a (bbox4 ent)]
    (or (> (oget-left a)(oget-right b))
        (< (oget-top a)(oget-bottom b))
        (< (oget-right a)(oget-left b))
        (> (oget-bottom a)(oget-top b)))))

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
(defn getBottom "" [obj] (oget-bottom (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getLeft "" [obj] (oget-left (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getRight "" [obj] (oget-right (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTop "" [obj] (oget-top (spriteBBox4 obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn traceEnclosure
  "Test if this box is hitting boundaries.
  If hit, the new position and velocities are returned."
  [dt bbox4 rect vel]
  (let [[hw hh] (half-size* rect)
        pt (vboxMID rect)
        vx (oget-x vel)
        vy (oget-y vel)
        y (+ (oget-y pt) (* dt vy))
        x (+ (oget-x pt) (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y hh) (oget-top bbox4)) ;;hitting top wall
          [x (- (oget-top bbox4) hh) vx (- vy) true]
          (< (- y hh) (oget-bottom bbox4)) ;;hitting bottom wall
          [x (+ (oget-bottom bbox4) hh) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x hw) (oget-right bbox4)) ;;hitting right wall
          [(- (oget-right bbox4) hw) y1 (- vx1) vy1 true]
          (< (- x hw) (oget-left bbox4)) ;;hitting left wall
          [(+ (oget-left bbox4) hw) y1 (- vx1) vy1 true]
          :else
          [x1 y1 vx1 vy1 t?])]
    [x2 y2 vx2 vy2 t2?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cc.Node stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *ws-uri* "/network/odin/websocket")
(def *game-scene* (atom nil))
(def *online-game* 3)
(def *game-2* 2)
(def *game-1* 1)

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
                                  (deref *game-scene*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasKeyPad? "" []
  (and (not-native?)
       (some? (oget js/cc.sys.capabilities "?keyboard"))))

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
    (if (some? pos) (ocall! node "setPosition" pos))
    (if-not show? (ocall! node "setVisible" false))
    (if (number? scale) (ocall! node "setScale" scale)) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn misprite* "" [nnn cb & [sss ddd ctx]]
  (new js/cc.MenuItemSprite (sprite* nnn)
                            (sprite* (or sss nnn))
                            (sprite* (or ddd nnn)) (or cb noopy) ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mitoggle* "" [on off cb] (new js/cc.MenuItemToggle on off cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu
  "Create a text menu containing this set of items."
  [items & [options]]

  (let [{:keys [flat? color]} options]
    (do-with
      [menu (new js/cc.Menu)]
      (doseq [{:keys [text font cb ctx]} items
              :let [mi (new js/cc.MenuItemLabel
                            (bmfText* text font) cb ctx)]]
        (if (some? color) (ocall mi "setColor" color))
        (addItem menu mi))
      (setXXX! menu options)
      (if flat?
        (ocall menu "alignItemsHorizontally")
        (ocall menu "alignItemsVertically")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu
  "Create a menu with graphic buttons."
  [items & [options]]

  (let [{:keys [padding flat?] :or {padding 10}} options]
    (do-with
      [menu (new js/cc.Menu)]
      (doseq [{:keys [cb ctx
                      nnn sss ddd]} items
              :let [mi (misprite* nnn cb sss ddd ctx)]]
        (addItem menu mi))
      (setXXX! menu options)
      (if flat?
        (ocall menu "alignItemsHorizontallyWithPadding" padding)
        (ocall menu "alignItemsVerticallyWithPadding" padding)))))

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
  (if-some [g (deref *game-scene*)]
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
(defn setSfx! "" [v]
  (swap! *xcfg* #(assoc-in % [:sound :open?] v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggleSfx! "" []
  (swap! *xcfg*
         #(update-in %
                     [:sound :open?] (fn [b] (not b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxOn? "" [] (getCfgXXX :sound :open?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxMusicVol "" [vol]
  (if (number? vol) (js/cc.audioEngine.setMusicVolume vol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayMusic "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playMusic (getCfgXXX :sounds key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayEffect "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playEffect (getCfgXXX :sounds key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxCancel! "" []
  (js/cc.audioEngine.stopMusic)
  (js/cc.audioEngine.stopAllEffects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxInit "" []
  (sfxMusicVol (getCfgXXX :audio :volume)))

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
  (atom {::topLeft (js/cc.p x y)
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
        [sz (csize image)
         h (oget-height sz)
         w (oget-width sz)
         [hw hh] (half-size* sz)
         icons
         (loop [n 0 arr []
                y (- (oget-y topLeft) hh)
                x (+ (oget-x topLeft) hw)]
           (if-not (< n curLives)
             arr
             (let [v (xliveIcon image {:pos (js/cc.p x y)})]
               (addItem parent v)
               (recur (inc n) (conj arr v)
                      y (if (pos? dir) (+ x w) (- x w))))))]
        (merge root {::icons icons})))) g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addAudioIcon! "" [node yes no & [options]]
  (let
    [cb #(setSfx!
           (zero? (ocall % "getSelectedIndex")))
     off (misprite* no nil)
     on (misprite* yes nil)
     audio (mitoggle* on off cb)]
    (ocall! audio
            "setSelectedIndex" (if (sfxOn?) 0 1))
    (do-with
      [menu (new js/cc.Menu audio)]
      (setXXX! menu options) (addItem node menu))))

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
(defn pegToAnchor "" [node where]
  ;;TODO
  (let [sz (csize node)
        [hw hh] (half-size* sz)
        B (vbox4)]
    (setXXX! node {:pos (js/cc.p (- (oget-right B) hw) 0)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XgameLayer "" [options]
  (do-with [y (new js/cc.Layer)]
           (attr* y
                   #js{:ebus (ebus/createEvBus)
                       :keyboard []
                       :players []
                       :level 1
                       :actor nil})
           (set! *game-scene* y)))

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
        selector #(do (runOnce)
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
        sz (csize logo)
        cp (centerPos)
        pg (new js/cc.ProgressTimer
                (sprite* (gldr->pbar)))]
    (setXXX! logo {:pos cp})
    (addItem scene logo "logo")
    (ocall! pg "setType" js/cc.ProgressTimer.TYPE_BAR)
    (ocall! pg "setScaleX" 0.8)
    (ocall! pg "setScaleY" 0.3)
    (setXXX! pg {:pos (js/cc.p (oget-x cp)
                               (- (oget-y cp)
                                  (* 0.6 (oget-height sz))))})
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
         :game {:policy js/cc.ResolutionPolicy.SHOW_ALL
                :preloadLevels? true
                :size {:width 0 :height 0}
                :resDir ""
                :landscape? true
                :CV-X (.charCodeAt "X" 0)
                :CV-O (.charCodeAt "0" 0)
                :P2-COLOR "O"
                :P1-COLOR "X"
                :NETP 3
                :HUMAN 1
                :BOT 2
                :GAME-MODE 1
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
                          "%cpu" "CPU"
                          "%p2" "P2"
                          "%p1" "P1"
                          "%2players" "2 Players"
                          "%1player" "1 Player"
                          "%online" "Online"
                          "%gameover" "Game Over"
                          "%quit!" "Quit"
                          "%back" "Back"
                          "%ok" "OK"
                          "%mmenu" "Main Menu"
                          "%replay" "REPLAY"
                          "%play" "PLAY"
                          "%waitothers" "Waiting...\nfor other players."
                          "%waitother" "Waiting...\nfor another player."
                          "%signinplay" "Please sign in to play."
                          "%quit?" "Continue and quit game?" }}
         :audio {:volume 0.5
                 :open? false
                 :track nil }
         :runOnce (f#* (preCacheAtlases))
         :startScene (f#*  nil)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

