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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with numStr]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-x oget-y oget-piccy
                                   oget-bottom oget-right
                                   not-native? native?
                                   sprite* half*
                                   newBBox newBBox4
                                   oget-left oget-top oget-id
                                   oget-width oget-height
                                   snode? bbox? bbox4? sprite?]])
  (:require [czlab.elmo.afx.core :as ec :refer [xmod raise!]]
            [czlab.elmo.afx.ebus :as ebus]
            [goog.object :as go]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare bbox bbox4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn debug* "" [& msgs] (cc.log (apply str msgs)))
(defn info* "" [& msgs] (cc.log (apply str msgs)))

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
(defn removeAll! "" [node] (ocall node "removeAllChildren"))
(defn remove! "" [child] (ocall child "removeFromParent"))

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
(defn misprite* "" [nnn cb & [sss ddd ctx]]
  (new js/cc.MenuItemSprite (sprite* nnn)
                            (sprite* (or sss nnn))
                            (sprite* (or ddd nnn)) cb ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mitoggle* "" [on off cb]
  (new js/cc.MenuItemToggle on off cb))

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

  (let [{:keys [padding flat?] :or {padding 10}} options]
    (do-with
      [menu (new js/cc.Menu)]
      (doseq [{:keys [cb ctx
                      nnn sss ddd]} items
              :let [mi (misprite* nnn cb sss ddd ctx)]]
        (.addChild menu mi))
      (setXXX! menu options)
      (if flat?
        (.alignItemsHorizontallyWithPadding menu padding)
        (.alignItemsVerticallyWithPadding menu padding)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmfLabel
  "" [text font & [options]]
  (setXXX! (bmfText* text font) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *xcfg*
  (atom {:urlPrefix "/public/ig/"
         :appid ""
         :color ""
         :resolution {:web js/cc.ResolutionPolicy.SHOW_ALL
                      :resDir "sd" }
         :levels {}
         :images { }
         :plists { }
         :tiles { }
         :sounds {:start_game "res/cocos2d/sfx/PowerUp" }
         :fonts {
      :TinyBoxBB [ 'res/cocos2d/fon/{{lang}}', 'TinyBoxBlackBitA8.png', 'TinyBoxBlackBitA8.fnt' ]
      :OogieBoogie [ 'res/cocos2d/fon/{{lang}}', 'OogieBoogie.png', 'OogieBoogie.fnt' ]
      :JellyBelly [ 'res/cocos2d/fon/{{lang}}', 'JellyBelly.png', 'JellyBelly.fnt' ]
      :AgentOrange [ 'res/cocos2d/fon/{{lang}}', 'AgentOrange.png', 'AgentOrange.fnt' ]
      :Hiruko [ 'res/cocos2d/fon/{{lang}}', 'Hiruko.png', 'Hiruko.fnt' ]
      :OCR [ 'res/cocos2d/fon/{{lang}}', 'OCR.png', 'OCR.fnt' ] }
         :game {:borderTiles "cbox-borders_x8.png"
                :start nil
                :preloadLevels true
                :scale 1
                :sfx :mp3
                :landscape? false
                :gravity 0
                :version ""
                :trackingID "" }
         :l10nTable {:en {"%mobileStart" "Press Anywhere To Start!"
                          "%webStart" "Press Spacebar To Start!"
                          "%passwd" "Password"
                          "%userid" "UserId"
                          "%player2" "Player 2"
                          "%player1" "Player 1"
                          "%computer" "Computer"
                          "%cpu" "CPU"
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
                          "%quit?" "Continue and quit game?" } }
         :csts {:CV_X (ocall "X" "charCodeAt" 0)
                :CV_O (ocall "0" "charCodeAt" 0)
                :P2_COLOR "O"
                :P1_COLOR "X"
                :NETP 3
                :HUMAN 1
                :BOT 2
                :GAME_MODE 1
                :TILE 8
                :S_OFF 4
                :GAME_ID "" }
         :sound {:volume 0.5
                 :open? false}
         :music {:volume 0.5
                 :track nil }

        :handleResolution (fn [rs] nil)

        :runOnce (fn [] nil)
}))

;import Mustache from "mustache";
;import LZString from "eligrey/l10njs";
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10nInit
  "Initialize the l10n module with the string table."
  [table]

  (oset! js/LZString "locale" js/cc.sys.language)
  (oset! js/LZString "defaultLocale" "en")
  (js/LZString.toLocaleString table)
  (info* "Loaded l10n strings. locale = " (oget js/LZString "locale")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10n
  "Localize the string." [s & [pms]]
  (let [t (ocall s "toLocaleString")]
    (if (some? pms) (js/Mustache.render t pms) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *ws-uri* "/network/odin/websocket")
(def *main-game* (atom nil))
(def *online-game* 3)
(def *game-2* 2)
(def *game-1* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getCfgXXX "" [path key] (get (get-in @*xcfg* path) key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fire! "" [topic & args]
  (if-some [g (deref *main-game*)]
    (if-some [bus (oget g "?ebus")]
      (apply ebus/pub bus (concat [topic] args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn radToDeg "" [rad] (* 180 (/ rad js/Math.PI)))
(defn degToRad "" [deg] (* deg (/ js/Math.PI 180)))

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
  (swap! *xcfg*
         #(update-in %
                     [:sound :open?] (fn [_] v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggleSfx! "" []
  (swap! *xcfg*
         #(update-in %
                     [:sound :open?] (fn [b] (not b)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxOn? "" [] (get-in @*xcfg* [:sound :open?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxMusicVol "" [vol]
  (if (number? vol) (js/cc.audioEngine.setMusicVolume vol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayMusic "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playMusic (getCfgXXX [:sounds] key) repeat?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxPlayEffect "" [key & [options]]
  (if (sfxOn?)
    (let [{:keys [vol repeat?]} options]
      (sfxMusicVol vol)
      (js/cc.audioEngine.playEffect (getCfgXXX [:sounds] key) repeat?))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxCancel "" []
  (js/cc.audioEngine.stopMusic)
  (js/cc.audioEngine.stopAllEffects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfxInit "" []
  (sfxMusicVol (get-in @*xcfg* [:sound :volume])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitizeUrlForDevice "" [url] url)
(defn sanitizeUrlForWeb "" [url] url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fixUrl
  "Sanitize this url differently for web and for devices." [url]
  (if (not-native?)
    (sanitizeUrlForWeb url) (sanitizeUrlForDevice url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getImage "" [key]
  (fixUrl (get-in @*xcfg* [:assets :images key])))
(defn getPList "" [key]
  (fixUrl (get-in @*xcfg* [:assets :plists key])))
(defn getTile "" [key]
  (fixUrl (get-in @*xcfg* [:assets :tiles key])))
(defn getFont "" [key]
  (fixUrl (get-in @*xcfg* [:assets :fonts key])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addItem "" [node child]
  (if (and (instance? js/cc.SpriteBatchNode node)
           (sprite? child))
      (ocall child "setBatchNode" node))
  (ocall node "addChild" child)
  child)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xlive "" [img & [options]]
  (let [s (sprite* img)] (setXXX! s options)))

  ;create() { const dummy = new XLive(0,0,this.options); this.lifeSize = { width: ccsx.getScaledWidth(dummy), height: ccsx.getScaledHeight(dummy) } ; this.drawLives(); }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xliveGroup "" [hud img total x y direction]
  (atom {:topLeft (js/cc.p x y)
         :image img
         :totalLives total
         :icons [] :parent hud :curLives total :dir direction}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reduceOneLive "" [g]
  (swap! g
         (fn [{:keys [parent icons curLives] :as root}]
           (let [e (peek icons)
                 c (pop icons)
                 n (dec curLives)]
             (remove! e)
             (merge root {:icons c :curLives n})))) g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn countLives "" [g] (get-in @g [:curLives]))
(defn noLives? "" [g] (pos? (countLives g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn resetLiveGroup "" [g]
  (swap! g
         (fn [{:keys [parent curLives total icons] :as root}]
           (doseq [x icons] (remove! x))
           (merge root {:curLives total :icons []}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawLives "" [g]
  (swap!
    g
    (fn [{:keys [parent topLeft curLives dir image] :as root}]
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
             (let [v (xlive image {:pos (js/cc.p x y)})]
               (addItem parent v)
               (recur (inc n) (conj arr v)
                      y (if (pos? dir) (+ x w) (- x w))))))]
        (merge root {:icons icons})))) g)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn batchNode* "" [img]
  (new js/cc.SpriteBatchNode
       (js/cc.textureCache.addImage img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addAudioIcon "" [node yes no & [options]]
  (let
    [cb #(setSfx!
           (zero? (ocall % "getSelectedIndex")))
     off (misprite* no nil)
     on (misprite* yes nil)
     audio (mitoggle* on off cb)]
    (ocall audio "setSelectedIndex" (if (sfxOn?) 0 1))
    (let [menu (new js/cc.Menu audio)]
      (setXXX! menu options)
      (ocall node "addChild" menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn quit! "" [ctor]
  (js/cc.director.pushScene
    (ctor {:no #(js/cc.director.popScene)
           :yes (fn [start]
                  (js/cc.director.popToRootScene)
                  (runScene start))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerImage "" [node frame]
  (let [bg (sprite* frame)]
    (setXXX! bg {:pos (centerPos)})
    (ocall node "addChild" bg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateScore "" [node score]
  (ocall node "setString" (numStr score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pegToAnchor "" [node where]
  (let [sz (csize node)
        [hw hh] (half-size* sz)
        B (vbox4)]
    (setXXX! node {:pos (js/cc.p (- (oget-right B) hw) 0)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameLayer "" [options]
  (let [y (new js/cc.Layer)]
    (ocall y
           "attr"
           #js{:ebus (ebus/createEvBus)
               :keyboard []
               :players []
               :level 1
               :actor nil})
    (set! *main-game* y)
    y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


