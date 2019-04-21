;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.cc.ccsx

  (:refer-clojure :exclude [contains?])

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid defvoid- each* do->true
                                   domonad
                                   js-prop? half* f#* n# _1 _2 do-with]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [visible! scale! color! anchor!
                                   run* pos* sprite* attr* ccmenu?
                                   pos! oget-x oget-y oget-piccy
                                   not-native? native? pushScene
                                   zero-rect new-rect popToRoot
                                   gcbyn gcbyt oget-id
                                   oget-width oget-height ccnode? sprite?]])

  (:require [czlab.mcfud.afx.core
             :as ec :refer [num?? nil-fn true-fn
                            monad-maybe false-fn raise! x->str]]
            [czlab.mcfud.afx.ebus :as ps]
            [czlab.mcfud.afx.math :refer [vec2 deg->rad]]
            [clojure.string :as cs]
            [goog.object :as go]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectEqualToRect?

  "True if rects are equal."
  [{x1 :x y1 :y w1 :wide t1 :tall :as rect1}
   {x2 :x y2 :y w2 :wide t2 :tall :as rect2}]

  (and (== x1 x2) (== y1 y2) (== w1 w2) (== t1 t2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectContainsRect?

  "True if R contains r."
  [{x1 :x y1 :y w1 :wide t1 :tall :as R}
   {x2 :x y2 :y w2 :wide t2 :tall :as r}]

  (not (or (>= x1 x2)
           (>= y1 y2)
           (<= (+ x1 w1) (+ x2 w2))
           (<= (+ y1 t1) (+ y2 t2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMaxX

  "Get right side of rect."
  [{:keys [x wide] :as r}]

  (+ x wide))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMidX

  "Get mid point of rect on the x-axis."
  [{:keys [x wide] :as r}]

  (+ x (* 0.5 wide)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMinX

  "Get left side of rect."
  [r]

  (:x r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMaxY

  "Get the top of the rect."
  [{:keys [y tall] :as r}]

  (+ y tall))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMidY

  "Get mid point of rect on the y-axis."
  [{:keys [y tall] :as r}]

  (+ y (* 0.5 tall)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectGetMinY

  "Get the bottom of rect."
  [r]

  (:y r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ptInRect?

  "True if point lies inside rect."
  [[px py] rect]

  (and (>= px (rectGetMinX rect))
       (<= px (rectGetMaxX rect))
       (>= py (rectGetMinY rect))
       (<= py (rectGetMaxY rect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectIntersectsRect?

  "True if the two rects intersect."
  [{x1 :x y1 :y w1 :wide t1 :tall :as rect1}
   {x2 :x y2 :y w2 :wide t2 :tall :as rect2}]

  (not (or (< (+ x1 w1) x2)
           (< (+ x2 w2) x1)
           (< (+ y1 t1) y2)
           (< (+ y2 t2) y1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectUnion

  "Find the union of two rects."
  [{x1 :x y1 :y w1 :wide t1 :tall :as rect1}
   {x2 :x y2 :y w2 :wide t2 :tall :as rect2}]

  (let [x (min x1 x2)
        y (min y1 y2)]
    (new-rect x
              y
              (- (max (+ x1 w1) (+ x2 w2)) x)
              (- (max (+ y1 t1) (+ y2 t2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectIntersection

  "Find the intersection of two rects."
  [{x1 :x y1 :y w1 :wide t1 :tall :as rect1}
   {x2 :x y2 :y w2 :wide t2 :tall :as rect2}]

  (let [x (max x1 x2)
        y (max y1 y2)]
    (new-rect x
              y
              (- (min (rectGetMaxX rect1) (rectGetMaxX rect2)) x)
              (- (min (rectGetMaxY rect1) (rectGetMaxY rect2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def xcfg (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn debug*

  "Debug log."
  [& msgs]

  (js/cc.log (apply str msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn outOfBound?

  "True if entity is outside of B."
  [ent B]

  (not (rectContainsRect? B ent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cp->vec2

  "Convert a cc-point to point."
  [p]

  (vec2 (oget-x p) (oget-y p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2->cp

  "Convert a point to cc-point."
  [[x y]]

  (js/cc.p x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ccrect->rect

  "Convert a cc-rect to rect."
  [rc]

  (new-rect (oget-x rc) (oget-y rc)
            (oget-width rc) (oget-height rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect->ccrect

  "Convert a rect to cc-rect."
  [{:keys [x y wide tall]}]

  (js/cc.rect x y wide tall))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getBBox

  "Call node.getBoundingBox."
  [n]

  (ocall n "getBoundingBox"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- snode?

  "True if node has a sprite as property."
  [obj]

  (and (object? obj) (js-prop? obj "piccy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ccrect?

  "True if object is a cc-rect."
  [obj]

  (and (object? obj)
       (js-prop? obj "width") (js-prop? obj "height")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect?

  "True if object is a rect."
  [obj]

  (and (map? obj)
       (clojure.core/contains? obj :wide)
       (clojure.core/contains? obj :tall)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->rect

  "Convert something to a rect."
  [obj]

  (cond (ccnode? obj)
        (ccrect->rect (getBBox obj))
        (snode? obj)
        (x->rect (oget-piccy obj))
        (ccrect? obj)
        (ccrect->rect obj)
        (rect? obj)
        obj
        :else (raise! "bad x->rect call")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect->box4

  "Get the 4 sides of a rect."
  [{:keys [x y wide tall] :as r}]

  {:peak (+ y tall) :rhs (+ x wide) :base y :lhs x})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?

  "Test collision of 2 entities."
  [a b]

  (cond (and (ccnode? a) (ccnode? b))
        (js/cc.rectIntersectsRect (getBBox a)
                                  (getBBox b))

        (and (snode? a) (snode? b))
        (collide? (oget-piccy a)
                  (oget-piccy b))

        (and (ccrect? a)(ccrect? b))
        (js/cc.rectIntersectsRect a b))

        (and (rect? a) (rect? b))
        (rectIntersectsRect? a b)

        :else
        (raise! "bad call collide?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setDevRes!

  "Set device resolution, policy and orientation."
  [{:keys [wide tall]} pcy]

  (js/cc.view.setDesignResolutionSize wide tall pcy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect

  "Get the visible screen rectangle."
  []

  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    (new-rect (oget-x vo)
              (oget-y vo)
              (oget-width wz)
              (oget-height wz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ebox

  "not-used"
  []

  (let [{:keys [htPerc anchor]} (:AD @xcfg)
        vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)
        h (* (oget-height wz) htPerc)]
    (new-rect (oget-x vo)
              (+ h (oget-y vo))
              (oget-width wz)
              (- (oget-height wz) h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mid-rect

  "Get the center point of a rect."
  [{:keys [x y wide tall] :as r}]

  (vec2 (+ x (* 0.5 wide))
        (+ y (* 0.5 tall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn frameSize

  "Get the actual window/frame size."
  []

  (let [z (if (native?)
            (js/cc.view.getFrameSize)
            (js/cc.director.getWinSize))]
    (new-rect 0 0 (oget-width z)(oget-height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isPortrait?

  "Test if the screen is oriented vertically."
  []

  (let [{:keys [wide tall]} (vrect)] (> tall wide)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid undoTimer!

  "Maybe release this timer."
  [p t]

  (if (and (native?)
           (some? t)) (ocall! t "release")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn newTimer

  "Create a timer action."
  [p t]

  (do-with [rc (->> (new js/cc.DelayTime t)
                    (ocall! p "runAction"))]
           (if (native?) (ocall! rc "retain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timerDone?

  "True if this timer is done."
  [t]

  (and (some? t) (ocall t "isDone")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- runNextScene

  "Run a scene."
  [nx & [delaySecs]]

  (run* (new js/cc.TransitionCrossFade (num?? delaySecs 0.6) nx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isTransitioning?

  "True if transitioning between scenes."
  []

  (instance? js/cc.TransitionScene
             (js/cc.director.getRunningScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize

  "Find content-size of this thing."
  [obj]

  (if-some [z (ocall obj "getContentSize")]
    (new-rect 0
              0
              (oget-width z)
              (oget-height z))
    (zero-rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bsize

  "Find size of object's bounding-box."
  [obj]

  (cond (string? obj)
        (bsize (sprite* obj))
        (ccnode? obj)
        (bsize (getBBox obj))
        (snode? obj)
        (bsize (oget-piccy obj))
        (ccrect? obj)
        (ccrect->rect obj)
        (rect? obj)
        obj
        :else (raise! "bad call bsize")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-size*

  "Calculate halves of width and height."
  [obj]

  (let [{:keys [wide tall]} (bsize obj)] [(* 0.5 wide) (* 0.5 tall)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getHeight

  "Get the height of a sprite."
  [sprite]

  (:tall (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledHeight

  "Get the scaled height."
  [sprite]

  (* (ocall sprite "getScaleY") (getHeight sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWidth

  "Get the width of a sprite."
  [sprite]

  (:wide (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledWidth

  "Get the scaled width."
  [sprite]

  (* (ocall sprite "getScaleX") (getWidth sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn traceEnclosure

  "Test if this box is hitting boundaries.
  If hit, the new position and velocities are returned."
  [B rect vel dt]

  (let [{:keys [peak base lhs rhs]} (rect->box4 B)
        [cx cy :as pt] (mid-rect rect)
        {:keys [wide tall]} rect
        hw (* 0.5 wide)
        hh (* 0.5 tall)
        [vx vy] vel
        y (+ cy (* dt vy))
        x (+ cx (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y hh) peak) ;;hitting top wall
          [x (- peak hh) vx (- vy) true]
          (< (- y hh) base) ;;hitting bottom wall
          [x (+ base hh) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x hw) rhs) ;;hitting right wall
          [(- rhs hw) y1 (- vx1) vy1 true]
          (< (- x hw) lhs) ;;hitting left wall
          [(+ lhs hw) y1 (- vx1) vy1 true]
          :else
          [x1 y1 vx1 vy1 t?])]
    [(vec2 x2 y2) (vec2 vx2 vy2) t2?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cc.Node stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def WS-URI "/network/odin/websocket")
(def game-scene (atom nil))
(def game-arena (atom nil))
(def GAME-NET 3)
(def GAME-TWO 2)
(def GAME-ONE 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setMIFont

  "Set (ttf) font details."
  [size & [name]]

  (js/cc.MenuItemFont.setFontName (or name "Arial"))
  (js/cc.MenuItemFont.setFontSize size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miFontLabel*

  "Create a menu-item label."
  [name size & [font]]

  (setMIFont size font)
  (doto (new js/cc.MenuItemFont name) (ocall! "setEnabled" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miFontItem*

  "Create a menu-item."
  [name size & [font]]

  (setMIFont size font)
  (new js/cc.MenuItemFont name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miSprite*

  "Create a menu-item sprite."
  [nnn cb & [sss ddd ctx]]

  (new js/cc.MenuItemSprite (sprite* nnn)
                            (sprite* (or sss nnn))
                            (sprite* (or ddd nnn)) (or cb nil-fn) ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miToggle*

  "Create a menu-item toggle."
  [on off cb & [ctx]]

  (new js/cc.MenuItemToggle on off cb ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn miLabel*

  "Create a menu-item label."
  [txt cb & [ctx]]

  (new js/cc.MenuItemLabel txt cb ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfText*

  "Create a bitmap-font label."
  [text font]

  (new js/cc.LabelBMFont text font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ttfLabel*

  "Create a ttf label."
  [text font size]

  (new js/cc.LabelTTF text font size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggleSelect!

  "Set which button is selected in a toggle."
  [t v]

  (ocall! t "setSelectedIndex" v) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getCachedSprite

  "Get the sprite from the frame cache using
  its id (e.g. #ship)."
  [frameid]

  (js/cc.spriteFrameCache.getSpriteFrame frameid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subEvent

  "Subscribe to an event."
  [e obj]

  (do->true (js/cc.eventManager.addListener (oset! obj "!event" e) @game-arena)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasKeyPad?

  "True if key-pad is available."
  []

  (and (not-native?)
       (some? (oget js/cc.sys.capabilities "?keyboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeyPolls

  "Subscribe to key polling."
  [kb]

  (when (hasKeyPad?)
    (debug* "about to listen to key events")
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [k _] (aset kb k true))
                  :onKeyReleased (fn [k _] (aset kb k false))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeys

  "Subscribe to key events."
  [bus]

  (when (hasKeyPad?)
    (debug* "about to listen to key events")
    (subEvent js/cc.EventListener.KEYBOARD
              #js{:onKeyPressed (fn [k e] (ps/pub bus "key.down" k e))
                  :onKeyReleased (fn [k e] (ps/pub bus "key.up" k e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasMouse?

  "True if mouse is available."
  []

  (some? (oget js/cc.sys.capabilities "?mouse")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onMouse

  "Subscribe to mouse events."
  [bus]

  (when (hasMouse?)
    (debug* "about to listen to mouse events")
    (subEvent js/cc.EventListener.MOUSE
              #js{:onMouseMove
                  (fn [e] (if (= (ocall e "getButton")
                                 js/cc.EventMouse.BUTTON_LEFT)
                            (ps/pub bus "mouse.move" e)))
                  :onMouseDown (fn [e] (ps/pub bus "mouse.down" e))
                  :onMouseUp (fn [e] (ps/pub bus "mouse.up" e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasTouch?

  "True if touch is available."
  []

  (some? (oget js/cc.sys.capabilities "?touches")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchAll

  "Subscribe to touch-all events."
  [bus]

  (when (hasTouch?)
    (debug* "about to listen to touch-all events")
    (let [obj #js{:onTouchesBegan true-fn
                  :prevTouchId -1
                  :onTouchesEnded
                  (fn [ts e] (ps/pub bus "touch.all.end" ts e))}]
      (oset! obj
             "!onTouchesMoved"
             (fn [ts e] (let [id (oget-id (_1 ts))]
                          (if (not= (oget obj "?prevTouchId") id)
                            (oset! obj "!prevTouchId" id)
                            (ps/pub bus "touch.all.move" ts e)))))
      (subEvent js/cc.EventListener.TOUCH_ALL_AT_ONCE obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchOne

  "Subscribe to touch-one events."
  [bus]

  (when (hasTouch?)
    (debug* "about to listen to touch-one events")
    (subEvent js/cc.EventListener.TOUCH_ONE_BY_ONE
              #js{:onTouchBegan true-fn
                  :swallowTouches true
                  :onTouchMoved (fn [t e] (ps/pub bus "touch.one.move" t e))
                  :onTouchEnded (fn [t e] (ps/pub bus "touch.one.end" t e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ANCHOR-CENTER 1)
(def ANCHOR-TOP 2)
(def ANCHOR-TOP-RIGHT 3)
(def ANCHOR-RIGHT 4)
(def ANCHOR-BOTTOM-RIGHT 5)
(def ANCHOR-BOTTOM 6)
(def ANCHOR-BOTTOM-LEFT 7)
(def ANCHOR-LEFT 8)
(def ANCHOR-TOP-LEFT 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn anchorValue

  "Map anchor to its point value."
  [a]

  (condp = a
    ANCHOR-CENTER (js/cc.p 0.5 0.5)
    ANCHOR-TOP (js/cc.p 0.5 1)
    ANCHOR-TOP-RIGHT (js/cc.p 1 1)
    ANCHOR-RIGHT (js/cc.p 1 0.5)
    ANCHOR-BOTTOM-RIGHT (js/cc.p 1 0)
    ANCHOR-BOTTOM (js/cc.p 0.5 0)
    ANCHOR-BOTTOM-LEFT (js/cc.p 0 0)
    ANCHOR-LEFT (js/cc.p 0 0.5)
    ANCHOR-TOP-LEFT (js/cc.p 0 1)
    (raise! "anchorValue - bad anchor enum")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn removeAll!

  "Call node.removeAllChildren."
  [node]

  (ocall! node "removeAllChildren"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove!

  "Call node.removeFromParent."
  [child]

  (ocall! child "removeFromParent"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cpos

  "Get the position of the node."
  [node]

  (cp->vec2 (pos* node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn colorLayer*

  "Create a color-layer."
  [r b g & [a]]

  (new js/cc.LayerColor (js/cc.color r b g a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add->

  "Add a child to the node."
  [node child & [tag zOrder]]

  (do-with [child]
           (if (and (instance? js/cc.SpriteBatchNode node)
                    (sprite? child))
             (ocall! child "setBatchNode" node))
           (ocall! node
                   "addChild"
                   child
                   (if (number? zOrder) zOrder js/undefined)
                   (if (or (string? tag)(number? tag)) tag js/undefined))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn batchNode*

  "Create a sprite-batch-node."
  [img]

  (new js/cc.SpriteBatchNode (js/cc.textureCache.addImage img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addSpriteFrames*

  "Add sprite frames to the cache."
  [plist]

  (js/cc.spriteFrameCache.addSpriteFrames plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setXXX!

  "Set node attributes."
  [node & [options]]

  (let [{:keys [scale
                color
                pos
                show?
                anchor]
         :or {show? true}} options]
    (do-with [node]
             (visible! node show?)
             (scale! node scale)
             (color! node color)
             (pos! node pos)
             (anchor! node anchor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pegMenu??

  "Place a menu by anchoring to its parent."
  [B anchor menu tag total padding flat?]

  (do-with [menu menu]
    (let [{:keys [peak base lhs rhs]}
          (rect->box4 B)
          {:keys [wide tall]}
          (bsize (gcbyt menu tag))
          t (+ (* padding (- total 1))
               (* total (if flat? wide tall)))
          [w h] (if flat? [t tall] [wide t])
          [cx cy :as cp] (mid-rect B)
          hw (* 0.5 w)
          hh (* 0.5 h)
          [x y]
          (condp = anchor
            ANCHOR-TOP-LEFT [(+ lhs hw) (- peak hh)]
            ANCHOR-TOP [cx (- peak hh)]
            ANCHOR-TOP-RIGHT [(- rhs hw) (- peak hh)]
            ANCHOR-LEFT  [(+ lhs hw) cy]
            ANCHOR-CENTER cp
            ANCHOR-RIGHT [(- rhs hw) cy]
            ANCHOR-BOTTOM-LEFT [(+ lhs hw) (+ base hh)]
            ANCHOR-BOTTOM [cx (+ base hh)]
          ANCHOR-BOTTOM-RIGHT [(- rhs hw) (+ base hh)]
          (raise! "pegMenu - bad anchor enum"))]
      (pos! menu (vec2 x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu

  "Create a text menu containing this set of items."
  [items & [options]]

  (let [{:keys [scale region anchor flat?
                align? color padding]
         :or {align? true padding 10}} options tag 911]
    (do-with [menu (new js/cc.Menu)]
             (each* #(let [{:keys [text font cb ctx]} %1
                           mi (-> (bmfText* text font)
                                  (miLabel* cb ctx))]
                       (scale! mi scale)
                       (color! mi color)
                       (add-> menu mi (+ %2 tag))) items)
             (when align?
               (if flat?
                 (ocall menu "alignItemsHorizontallyWithPadding" padding)
                 (ocall menu "alignItemsVerticallyWithPadding" padding)))
             (when anchor
              (anchor! menu anchor)
              (pegMenu?? (or region (vrect))
                         anchor menu tag (n# items) padding flat?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu

  "Create a menu with graphic buttons."
  [items & [options]]

  (let [{:keys [scale align? region anchor padding flat?]
         :or {align? true padding 10}}
        options
        tag 911]
    (do-with [menu (new js/cc.Menu)]
             (each* #(let [{:keys [cb ctx nnn sss ddd]} %1
                           mi (miSprite* nnn cb sss ddd ctx)]
                       (scale! mi scale)
                       (add-> menu mi (+ %2 tag))) items)
             (when align?
               (if flat?
                 (ocall menu "alignItemsHorizontallyWithPadding" padding)
                 (ocall menu "alignItemsVerticallyWithPadding" padding)))
             (when anchor
               (anchor! menu anchor)
               (pegMenu?? (or region (vrect))
                          anchor
                          menu tag (n# items) padding flat?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfLabel

  "Create a bitmapfont label."
  [text font & [options]]

  (setXXX! (bmfText* text font) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfgGet

  "Get a value from config."
  [& path]

  (get-in @xcfg path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid l10nInit

  "Initialize the l10n module with the string table."
  []

  (let [lang (keyword js/cc.sys.language)]
    (debug* "l10n locale = " (name lang))
    (debug* (-> (cfgGet :l10nTable lang)
                (clj->js)
                (js/JSON.stringify)))
    (swap! xcfg #(assoc % :lang lang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10n

  "Localize the string."
  [key & pms]

  (let [msg (get (cfgGet :l10nTable
                         (:lang @xcfg)) key)]
    (if (not-empty msg)
      (if (not-empty pms)
        (let [arr (cs/split msg #"\{\}")
              sz (n# arr)
              end (- sz 1)
              plen (n# pms)
              out (transient [])]
          (dotimes [i sz]
            (conj! out (nth arr i))
            (when (and (< i end)
                       (< i plen))
              (conj! out (nth pms i))))
          (cs/join "" (persistent! out))) msg) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fire!

  "Publish a message on this topic."
  [topic & args]

  (domonad monad-maybe
           [g @game-arena
            bus (oget g "?ebus")]
           (apply ps/pub bus (concat [topic] args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcXY

  "Find the corresponding x, y lengths based on the
  provided angle and length of the hypotenuse.
  quadrants =  3 | 4
               -----
               2 | 1"
  [angle magnitude]

  (let [r (deg->rad angle)]
    (vec2 (* (js/Math.cos r) magnitude)
          (* (js/Math.sin r) magnitude))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn normalizeDeg

  "Normalize the degree - modulo 360."
  [deg]

  (comment
    (if (> deg 360)
      (mod deg 360)
      (if (< deg 0)
        (- 360 (mod (- deg) 360)) deg)))
  (rem deg 360))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitizeUrlForDevice

  ""
  [url]

  url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitizeUrlForWeb

  ""
  [url]

  url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fixUrl

  "Sanitize this url differently for web and for devices."
  [url]

  (if (not-native?)
    (sanitizeUrlForWeb url) (sanitizeUrlForDevice url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->logo

  "Get the path for the logo image."
  []

  (str "res/" (cfgGet :loader ::czlab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->pbar

  "Get the path for loader-processing image."
  []

  (str "res/" (cfgGet :loader ::preloader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gldr->imgs

  "Get the path for loader images."
  []

  [(gldr->logo) (gldr->pbar)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gatlas

  "Get the path for the spritesheet plist."
  [key]

  (str "res/" (cfgGet :sheets key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gatlas->img

  "Get the path for the spritesheet image."
  [key]

  (cs/replace (gatlas key) #"\.plist$" ".png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gimg

  "Get the path for a image."
  [key]

  (str "res/" (cfgGet :images key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gtile

  "Get the path for a tile."
  [key]

  (str "res/" (cfgGet :tiles key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gfnt

  "Get the path for a font file."
  [key]

  (str "res/" (cfgGet :fonts key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gfnt->img

  "Get the path for a font image."
  [key]

  (cs/replace (gfnt key) #"\.fnt$" ".png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gsfx

  "Get the path for a sound file."
  [key]

  (->> (cfgGet :game :sfx)
       (name)
       (str "res/" (cfgGet :sounds key) ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid setSfx!

  "Turn sound on or off."
  [s?]

  (swap! xcfg #(assoc-in % [:audio :open?] s?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid toggleSfx!

  "Toggle the sound."
  []

  (swap! xcfg
         #(update-in %
                     [:audio :open?] (fn [b] (not b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxOn?

  "True if sound is on."
  []

  (cfgGet :audio :open?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxMusicVol

  "Set music volume."
  [vol]

  (if (number? vol) (js/cc.audioEngine.setMusicVolume vol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid sfxPlayMusic

  "Play music."
  [key & [options]]

  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playMusic (gsfx key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid sfxPlayEffect

  "Play sound effect."
  [key & [options]]

  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playEffect (gsfx key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid sfxCancel!

  "Stop all sound."
  []

  (js/cc.audioEngine.stopMusic)
  (js/cc.audioEngine.stopAllEffects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxInit

  "Initialize sound."
  []

  (sfxMusicVol (cfgGet :audio :volume)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- preCacheAtlases

  "Cache all spritesheets."
  []

  (doseq [[k _] (:sheets @xcfg)]
    (->> (gatlas k)
         (js/cc.spriteFrameCache.addSpriteFrames))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xliveIcon

  ""
  [img & [options]]

  (do-with [s (sprite* img)] (setXXX! s options)))

;create() { const dummy = new XLive(0,0,this.options); this.lifeSize = { width: ccsx.getScaledWidth(dummy), height: ccsx.getScaledHeight(dummy) } ; this.drawLives(); }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xliveGroup

  ""
  [hud img total pt direction]

  (atom {:topLeft pt
         :image img
         :totalLives total
         :icons [] :parent hud :curLives total :dir direction}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reduceOneLive

  "Reduce one live."
  [g]

  (swap! g (fn [{:keys [icons curLives] :as root}]
             (let [e (peek icons)
                   c (pop icons)
                   n (dec curLives)]
               (remove! e)
               (merge root {:icons c :curLives n}))))
  g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countLiveIcons

  "Get the number of lives left."
  [g]

  (:curLives @g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn noLiveIcons?

  "True if there are still more lives left."
  [g]

  (pos? (countLiveIcons g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid resetLiveGroup

  "Reset the lives-group."
  [g]

  (swap! g (fn [{:keys [totalLives icons] :as root}]
             (doseq [x icons] (remove! x))
             (merge root {:curLives totalLives :icons []}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawLiveIcons

  "Draw the lives-group."
  [g]

  (swap! g (fn [{:keys [parent topLeft
                        curLives dir image] :as root}]
             (let [[hw hh] (half-size* (bsize image))
                   w (* 2 hw (if (pos? dir) 1 (- 1)))
                   icons
                   (loop [n 0 arr []
                          y (- (_1 topLeft) hh)
                          x (+ (_2 topLeft) hw)]
                     (if (>= n curLives)
                       arr
                       (recur (+ 1 n)
                              (->> {:pos (vec2 x y)}
                                   (xliveIcon image)
                                   (add-> parent) (conj arr)) y (+ x w))))]
               (merge root {:icons icons}))))
  g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn audioIcon

  "Create the audio - sound control icon."
  [yes no & [options]]

  (let [cb #(setSfx!
              (zero? (ocall % "getSelectedIndex")))
        off (miSprite* no nil)
        on (miSprite* yes nil)]
    (new js/cc.Menu (do-with [audio (miToggle* on off cb)]
                             (toggleSelect! audio
                                            (if (sfxOn?) 0 1))
                             (setXXX! audio options)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid quit!

  "What happens when we quit."
  [ctor]

  (pushScene (ctor {:no #(js/cc.director.popScene)
                    :yes (fn [start]
                           (popToRoot)
                           (runNextScene start))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerImage

  "Add this image to the center."
  [node frame]

  (do-with [bg (sprite* frame)]
           (pos! bg (mid-rect (vrect))) (add-> node bg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateScore

  "Update score."
  [node score]

  (ocall! node "setString" (x->str score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pegToAnchor

  "Peg something to its parent based on the anchor-points."
  [B node where]

  (let [{:keys [peak base lhs rhs]} B
        [x y] (mid-rect B)
        [x' y']
        (condp = where
          ANCHOR-TOP-LEFT [lhs peak]
          ANCHOR-TOP [x peak]
          ANCHOR-TOP-RIGHT [rhs peak]
          ANCHOR-LEFT [lhs y]
          ANCHOR-CENTER [x y]
          ANCHOR-RIGHT [rhs y]
          ANCHOR-BOTTOM-LEFT [lhs base]
          ANCHOR-BOTTOM [x base]
          ANCHOR-BOTTOM-RIGHT [rhs base]
          (raise! "pegToAnchor bad where = " where))]
    (pos! node (vec2 x' y'))))

;import Cookies from 'Cookies';
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkScore

  "Create a score data object."
  [n v]

  {:value (js/Number (cs/trim v)) :name (cs/trim n)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn readHighScores

  "Grab high-scores from cookies."
  [hs]

  (let [{:keys [KEY]} @hs
        s (cs/trim (or (js/Cookies.get KEY) ""))]
    (swap! hs
           #(assoc %
                   :scores
                   (reduce
                     (fn [acc z]
                       (let [a (cs/split (or z "") ":")]
                         (if (= 2 (n# a))
                           (conj acc (mkScore (_1 a) (_2 a))) acc)))
                     []
                     (cs/split s "|"))))
    hs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resetHighScores!

  "Reset the high scores."
  [hg]

  (swap! hg #(assoc % :scores [])) hg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn writeHighScores!

  "Flush high scores to the cookie."
  [hg]

  (let [{:keys [KEY scores duration]} @hg]
    (js/Cookies.set KEY
                    (->> (map #(str (:name %)
                                    ":"
                                    (:value %)) scores)
                         (cs/join "|"))
                    duration)
    hg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hasSlotsHighScores?

  "Test if there is more room to store a new high score."
  [hg]

  (let [{:keys [scores size]} @hg] (< (n# scores) size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn canAddHighScores?

  "Test if we can add this score to the list of highscores."
  [hg score]

  (or (hasSlotsHighScores? hg)
      (some #(< (:value %) score) (:scores @hg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insertHighScores!

  "Insert a score into the high-scores."
  [hg name score]

  (let [s (mkScore (or name "???") score)
        {:keys [scores]} @hg
        len (n# scores)]
    (when-not (hasSlotsHighScores? hg)
      (loop [i (dec len) arr nil]
        (if (some? arr)
          (swap! hg #(assoc % :scores (js->clj arr)))
          (if-not (neg? i)
            (recur (dec i)
                   (if (< (:value (nth scores i)) score)
                     (.splice (clj->js scores) i 1)))))))
    (when (hasSlotsHighScores? hg)
      (swap! hg
             (fn [{:keys [scores] :as root}]
               (assoc root
                      :scores
                      (sort (fn [a b]
                              (cond (< (:value a) (:value b)) -1
                                    (> (:value a) (:value b)) 1 :else 0))
                            (conj scores s)))))
      (writeHighScores! hg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmtHighScores

  "Create a high-scores data object."
  [key size & [duration]]

  {:duration (or duration (* 60 60 24 1000)) :size size :scores [] :KEY key })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private CHUNK 36)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadImages

  "Load the paths to the images."
  []

  (reduce #(conj %1 (gimg %2)) [] (keys (:images @xcfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadTiles

  "Load the paths to the tiles."
  []

  (reduce #(conj %1 (gtile %2)) [] (keys (:tiles @xcfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadSounds

  "Load the paths to the sounds."
  []

  (reduce #(conj %1 (gsfx %2)) [] (keys (:sounds @xcfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadFonts

  "Load the paths to the fonts."
  []

  (reduce (fn [acc k]
            (conj acc
                  (gfnt->img k)
                  (gfnt k))) [] (keys (:fonts @xcfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadSheets

  "Load the paths to the spritesheets."
  []

  (reduce (fn [acc k]
            (conj acc
                  (gatlas->img k)
                  (gatlas k)))
          []
          (keys (:sheets @xcfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gatherPreloads

  "Get all paths to resources."
  []

  (concat (preloadImages)
          (preloadTiles)
          (preloadFonts)
          (preloadSounds)
          (preloadSheets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn niceFadeOut

  "After loading resources, runs the first scene."
  [scene]

  (let [{:keys [runOnce startScene]} @xcfg
        bar (gcbyn scene "progress")
        logo (gcbyn scene "logo")
        target nil
        selector #(do (preCacheAtlases)
                      (runOnce)
                      (run* (startScene)))]
    (debug* "fade out! run next scene!!!!!")
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
(defn- pkLoadMore

  "Incrementally load resouces."
  [scene assets state]

  (f#*
    (let [pg (gcbyn scene "progress")
          cnt (aget state 0)
          len (n# assets)
          ratio (/ cnt len)
          perc (min (* ratio 100) 100)]
      (ocall! pg "setPercentage" perc)
      (if (< cnt len)
        (let [head (aget state 2) ;get last tail
              tail (+ head (min CHUNK (- len head)))]
          (aset state 1 head)
          (aset state 2 tail)
          (loadChunk scene assets state))
        (do (debug* "all resources loaded!") (niceFadeOut scene))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pkLoad

  "Load resources."
  [scene assets state]

  (let [logo (sprite* (gldr->logo))
        {ht :tall} (bsize logo)
        [mx my :as cp] (mid-rect (vrect))
        pg (new js/cc.ProgressTimer (sprite* (gldr->pbar)))]
    (pos! logo cp)
    (add-> scene logo "logo")
    (ocall! pg "setType" js/cc.ProgressTimer.TYPE_BAR)
    (ocall! pg "setScaleX" 0.8)
    (ocall! pg "setScaleY" 0.3)
    (pos! pg (vec2 mx (- my (* 0.6 ht))))
    (add-> scene pg "progress")
    (attr* scene
           #js{:update (pkLoadMore scene
                                   (clj->js assets) state)})
    (ocall! scene "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn loaderScene*

  "Create the loader scene."
  []

  (do-with [scene (new js/cc.Scene)]
    (let [assets (gatherPreloads)
          y (colorLayer* 0 0 0)
          ;;[count, head, tail] snapshot info used by
          ;;each iteration as we chunk up the unput
          state (array 0 0 0)
          func (f#* (pkLoad scene assets state))]
      (pos! y (vec2 0 0))
      (add-> scene y "bgLayer" -1)
      (attr* scene
             #js{:onEnter
                 #(do (.call js/cc.Node.prototype.onEnter scene)
                      (ocall! scene "scheduleOnce" func 0.3))
                 :onExit
                 #(.call js/cc.Node.prototype.onExit scene)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn preloader

  "Get the function to preload resources."
  []

  (f#* (debug* "start to run asset preloader.") (run* (loaderScene*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patch the config object!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! xcfg
        {:urlPrefix "/public/mcfud/"
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
         :AD {:anchor ANCHOR-BOTTOM
              :on? true
              :htPerc 0.09}
         :game {:policy js/cc.ResolutionPolicy.SHOW_ALL
                :preloadLevels? true
                :size (zero-rect)
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
                 :track nil}
         :runOnce nil-fn
         :startScene nil-fn})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

