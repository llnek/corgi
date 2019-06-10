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

  ;(:refer-clojure :exclude [contains?])

  (:require-macros [czlab.mcfud.cc.ccsx
                    :as x :refer [native? not-native? cnode?
                                  debug* gsidx gcbyt gcbyn
                                  oget-id oget-piccy
                                  oget-x oget-y oget-height oget-width]])

  (:require [czlab.mcfud.afx.core
             :as c :refer [defenum do->true do->nil
                           fn-nil is? fn_0 fn_* fn_1 fn_2 cc+ cc+1
                           if-string n# _1 _2 do-with raise! num??]]
            [czlab.mcfud.afx.ebus :as e]
            [czlab.mcfud.afx.math :as m]
            [clojure.string :as cs]
            [goog.object :as go]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def xcfg (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn r->
  "Destruct, get width & height."
  [r] [(oget-width r) (oget-height r)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn p->
  "Destruct, get x & y."
  [p] [(oget-x p) (oget-y p)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-bbox
  "Node.getBoundingBox."
  [n] (ocall n "getBoundingBox"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn crect?
  "If object is a cc-rect?"
  [obj]
  (and (object? obj)
       (c/js-prop? obj "width") (c/js-prop? obj "height")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- snode?
  "If node has a sprite property?"
  [obj] (and (object? obj) (c/js-prop? obj "piccy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn r->b4
  "Get the 4 sides of a rect."
  [r]
  (let [x (oget-x r) y (oget-y r)
        w (oget-width r) h (oget-height r)]
    {:top (+ y h) :right (+ x w) :bottom y :left x}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities."
  [a b]
  (cond (and (snode? a) (snode? b))
        (collide? (oget-piccy a)
                  (oget-piccy b))
        (and (cnode? a) (cnode? b))
        (collide? (get-bbox a)
                  (get-bbox b))
        (and (crect? a)(crect? b))
        (js/cc.rectIntersectsRect a b)
        :else
        (raise! "bad call collide?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-dev-res!
  "Set device resolution, policy and orientation."
  [width height policy]
  (js/view.setDesignResolutionSize width height policy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect
  "Get the visible screen rectangle."
  []
  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    (js/cc.rect (oget-x vo)
                (oget-y vo) (oget-width wz) (oget-height wz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ebox
  "not-used"
  []
  (let [{:keys [htPerc anchor]} (:AD @xcfg)
        [x y] (p-> (js/cc.view.getVisibleOrigin))
        [w h] (r-> (js/cc.view.getVisibleSize))]
    (js/cc.rect x y w (- h (* h htPerc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mid-rect
  "Center point of a rect."
  [r]
  (js/cc.p (js/cc.rectGetMidX r) (js/cc.rectGetMidY r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn frame-size
  "Actual window/frame size."
  []
  (let [z (if (native?)
            (js/cc.view.getFrameSize)
            (js/cc.director.getWinSize))]
    (js/cc.rect 0 0 (oget-width z)(oget-height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-portrait?
  "If screen is oriented vertically."
  []
  (let [[w h] (r-> (vrect))] (> h w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn undo-timer!
  "Maybe release this timer."
  [p t]
  (if (and (native?)
           (some? t)) (ocall! t "release")) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-timer
  "Create a timer action."
  [p t]
  (do-with
    [rc (->> (new js/cc.DelayTime t)
             (ocall! p "runAction"))] (if (native?) (ocall! rc "retain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timer-done?
  "If timer is done."
  [t] (and (some? t) (ocall t "isDone")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-next-scene
  "Run a scene."
  [nx & [delaySecs]]
  (x/run-scene (new js/cc.TransitionCrossFade (num?? delaySecs .6) nx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-transitioning?
  "If transitioning between scenes?"
  []
  (instance? js/cc.TransitionScene (js/cc.director.getRunningScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize
  "Content-size of this thing."
  [obj]
  (if-some [z (ocall obj "getContentSize")]
    (js/cc.rect 0 0
                (oget-width z)
                (oget-height z)) (js/cc.rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bsize
  "Find size of object's bounding-box."
  [obj]
  (cond (string? obj)
        (bsize (x/sprite* obj))
        (snode? obj)
        (bsize (oget-piccy obj))
        (cnode? obj)
        (bsize (get-bbox obj))
        (crect? obj)
        obj
        :else (raise! "bad call bsize")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-rect
  "Calculate halves of width and height."
  [r]
  (js/cc.rect 0 0
              (/ (oget-width r) 2)
              (/ (oget-height r) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-height
  "Get the height of a sprite."
  [sprite] (oget-height (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-scaled-height
  "Get the scaled height."
  [sprite]
  (* (ocall sprite "getScaleY") (get-height sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-width
  "Get the width of a sprite."
  [sprite] (oget-width (bsize sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-scaled-width
  "Get the scaled width."
  [sprite]
  (* (ocall sprite "getScaleX") (get-width sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trace-enclosure
  "Test if this box is hitting boundaries.
  If hit, the new position and velocities are returned."
  [R rect vel dt]
  (let [{:keys [top bottom left right]} (r->b4 R)
        [hw hh] (r-> (half-rect rect))
        [cx cy] (p-> (mid-rect rect))
        [vx vy] (p-> vel)
        y (+ cy (* dt vy))
        x (+ cx (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y hh) top) ;;hitting top wall
          [x (- top hh) vx (- vy) true]
          (< (- y hh) bottom) ;;hitting bottom wall
          [x (+ bottom hh) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x hw) right) ;;hitting right wall
          [(- right hw) y1 (- vx1) vy1 true]
          (< (- x hw) left) ;;hitting left wall
          [(+ left hw) y1 (- vx1) vy1 true]
          :else
          [x1 y1 vx1 vy1 t?])]
    [(js/cc.p x2 y2) (js/cc.p vx2 vy2) t2?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cc.Node stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def WS-URI "/network/odin/websocket")

(defenum G ONE 1 TWO NET)
(defenum P MAN 1 BOT)

(def CV-X 88) ;(.charCodeAt "X" 0)
(def CV-O 79) ;(.charCodeAt "0" 0)
(def CV-Z 0)
(def CC-O "O")
(def CC-X "X")
(def CC-Z "?")
(def CX :1)
(def CO :2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenum ANCHOR
  CENTER 1
  TOP
  BOTTOM
  LEFT
  RIGHT
  TOP-LEFT
  TOP-RIGHT
  BOTTOM-LEFT
  BOTTOM-RIGHT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-font!
  "Set (ttf) font details."
  [size & [name]]
  (js/cc.MenuItemFont.setFontName (or name "Arial"))
  (js/cc.MenuItemFont.setFontSize size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mifont-item*
  "Create a menu-item."
  [name size & [font]]
  (set-font! size font) (x/mifont* name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mifont-text*
  "Create a menu-item label."
  [name size & [font]]
  (-> (mifont-item* name size font) (ocall! "setEnabled" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggle-select!
  "Select button in a toggle."
  [t v]
  (ocall! t "setSelectedIndex" v) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subevent
  "Subscribe to an event."
  [e obj]
  (do->true
    (js/cc.eventManager.addListener
      (oset! obj "!event" e)
      (gcbyn (get-in @xcfg [:game :scene]) "arena"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-keys?
  "If key-pad is available?"
  []
  (and (not-native?)
       (some? (oget js/cc.sys.capabilities "?keyboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-keys
  "Subscribe to key polling."
  [s]
  (when (has-keys?)
    (debug* "about to listen to key events")
    (subevent js/cc.EventListener.KEYBOARD
              (if (array? s)
                #js{:onKeyPressed (fn [a b] (aset s a true))
                    :onKeyReleased (fn [a b] (aset s a false))}
                #js{:onKeyPressed (fn [a b] (e/pub s "key.down" a b))
                    :onKeyReleased (fn [a b] (e/pub s "key.up" a b))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-mouse?
  "If mouse is available?"
  []
  (some? (oget js/cc.sys.capabilities "?mouse")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-mouse
  "Subscribe to mouse events."
  [s]
  (when (has-mouse?)
    (debug* "about to listen to mouse events")
    (subevent js/cc.EventListener.MOUSE
              #js{:onMouseMove
                  (fn [a] (if (= (ocall a "getButton")
                               js/cc.EventMouse.BUTTON_LEFT)
                            (e/pub s "mouse.move" a)))
                  :onMouseDown (fn [a] (e/pub s "mouse.down" a))
                  :onMouseUp (fn [a] (e/pub s "mouse.up" a))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-touch?
  "If touch is available?"
  []
  (some? (oget js/cc.sys.capabilities "?touches")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-touch-all
  "Subscribe to touch-all events."
  [s]
  (when (has-touch?)
    (debug* "about to listen to touch-all events")
    (let [obj #js{:onTouchesBegan c/fn-true
                  :prevTouchId -1
                  :onTouchesEnded
                  (fn [a b] (e/pub s "touch.all.end" a b))}]
      (oset! obj
             "!onTouchesMoved"
             (fn [a b]
               (let [id (oget-id (_1 a))]
                 (if (not= (oget obj "?prevTouchId") id)
                   (oset! obj "!prevTouchId" id)
                   (e/pub s "touch.all.move" a b)))))
      (subevent js/cc.EventListener.TOUCH_ALL_AT_ONCE obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-touch-one
  "Subscribe to touch-one events."
  [s]
  (when (has-touch?)
    (debug* "about to listen to touch-one events")
    (subevent js/cc.EventListener.TOUCH_ONE_BY_ONE
              #js{:onTouchBegan c/fn-true
                  :swallowTouches true
                  :onTouchMoved (fn [a b] (e/pub s "touch.one.move" a b))
                  :onTouchEnded (fn [a b] (e/pub s "touch.one.end" a b))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn anchor-value
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
(defn add->
  "Add a child to the node."
  [node child & [tag zOrder]]
  (do-with [child]
    (if (and (is? js/cc.SpriteBatchNode node)
             (x/sprite? child)) (ocall! child "setBatchNode" node))
    (ocall! node
            "addChild"
            child
            (if (number? zOrder) zOrder js/undefined)
            (if (or (string? tag)(number? tag)) tag js/undefined))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set!!
  "Set node attributes."
  [node & [options]]
  (let [{:keys [scale color
                pos show? anchor]
         :or {show? true}} options]
    (doto node
      (x/visible! show?)
      (x/scale! scale)
      (x/color! color)
      (x/pos! pos)
      (x/anchor! anchor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- peg-menu??
  "Place a menu by anchoring to its parent."
  [R anchor menu tag total padding flat?]
  (do-with [menu]
    (let [{:keys [top bottom left right]}
          (r->b4 R)
          [width height]
          (r-> (bsize (gcbyt menu tag)))
          t (+ (* padding (- total 1))
               (* total (if flat? width height)))
          [w h] (if flat? [t height] [width t])
          [cx cy :as cp] (p-> (mid-rect R))
          [hw hh] (c/mapfv / 2 w h)
          [x y]
          (condp = anchor
            ANCHOR-TOP-LEFT [(+ left hw) (- top hh)]
            ANCHOR-TOP [cx (- top hh)]
            ANCHOR-TOP-RIGHT [(- right hw) (- top hh)]
            ANCHOR-LEFT  [(+ left hw) cy]
            ANCHOR-CENTER cp
            ANCHOR-RIGHT [(- right hw) cy]
            ANCHOR-BOTTOM-LEFT [(+ left hw) (+ bottom hh)]
            ANCHOR-BOTTOM [cx (+ bottom hh)]
            ANCHOR-BOTTOM-RIGHT [(- right hw) (+ bottom hh)]
            (raise! "pegMenu - bad anchor enum"))]
      (x/pos! menu x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu
  "Create a text menu containing this set of items."
  [items & [options]]
  (let [{:keys [scale region anchor
                flat? align? color padding]
         :or {align? true padding 10}} options tag 911]
    (do-with [menu (x/menu*)]
      (c/each* #(let [{:keys [text font cb ctx]} %1]
                  (add-> menu
                         (set!! (-> (x/bmf-text* text font)
                                    (x/milabel* cb ctx))
                                {:scale scale :color color})
                         (+ %2 tag))) items)
      (when align?
        (if flat?
          (x/halign-items menu padding)
          (x/valign-items menu padding)))
      (when anchor
        (x/anchor! menu anchor)
        (peg-menu?? (or region (vrect))
                    anchor menu tag (n# items) padding flat?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu
  "Create a menu with graphic buttons."
  [items & [options]]
  (let [tag 911
        {:keys [scale align? region
                anchor padding flat?]
         :or {align? true padding 10}} options]
    (do-with [menu (x/menu*)]
      (c/each* #(let [{:keys [cb ctx nnn sss ddd]} %1]
                  (add-> menu
                         (set!! (x/misprite* nnn cb
                                             sss ddd ctx)
                                {:scale scale}) (+ %2 tag))) items)
      (when align?
        (if flat?
          (x/halign-items menu padding)
          (x/valign-items menu padding)))
      (when anchor
        (x/anchor! menu anchor)
        (peg-menu?? (or region (vrect))
                    anchor
                    menu tag (n# items) padding flat?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmf-label*
  "New bitmapfont label."
  [text font & [options]]
  (set!! (x/bmf-text* text font) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gicfg
  "Get value from config."
  [& path] (get-in @xcfg path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l10n
  "Localize the string."
  [key & pms]
  (if-string [msg (get (gicfg :l10n
                               (:lang @xcfg)) key)]
    (let [arr (cs/split msg #"\{\}")
          SZ (n# arr)
          end (- SZ 1)
          plen (n# pms)]
      (loop [i SZ out (c/tvec*)]
        (if (>= i SZ)
          (cs/join "" (c/pert! out))
          (recur (+ 1 i)
                 (let [out' (conj! out
                                   (nth arr i))]
                   (if (and (< i end)
                            (< i plen))
                     (conj! out' (nth pms i)) out'))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fire!
  "Publish a message on this topic."
  [topic & args]
  (if-some [b (-> (get-in @xcfg [:game :scene])
                  (oget "?ebus"))] (apply e/pub b (cc+1 topic args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-xy
  "Find the corresponding x, y lengths based on the
  provided angle and length of the hypotenuse.
  quadrants =  3 | 4
               -----
               2 | 1"
  [angle magnitude]
  (let [r (m/deg->rad angle)]
    (js/cc.p (* (js/Math.cos r) magnitude)
             (* (js/Math.sin r) magnitude))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn normalize-deg
  "Normalize the degree - modulo 360."
  [deg]
  (comment
    (if (> deg 360)
      (mod deg 360)
      (if (< deg 0)
        (- 360 (mod (- deg) 360)) deg)))
  (rem deg 360))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitize-url-device "" [url] url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sanitize-url-web "" [url] url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fix-url
  "Sanitize this url differently for web and for devices."
  [url]
  (if (not-native?)
    (sanitize-url-web url) (sanitize-url-device url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gres*
  "Get resource path."
  [path]
  (if (not-native?) (str "res/" path) path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gres+
  "Get resource path."
  [& path] (gres* (apply gicfg path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gfnt
  "Get font file path." [k] (gres* (gicfg :assets :fonts k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gimg
  "Get image file path." [k] (gres* (gicfg :assets :images k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx!
  "Turn sound on or off."
  [s?]
  (do->nil
    (swap! xcfg #(assoc-in % [:audio :open?] s?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggle-sfx!
  "Toggle the sound."
  []
  (do->nil
    (swap! xcfg
           (fn_1 (update-in ____1
                            [:audio :open?] #(not %))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx? "If sound is on?" [] (get-in @xcfg [:audio :open?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx-music-vol!
  "Set music volume."
  [vol]
  (do->nil (if (number? vol) (js/cc.audioEngine.setMusicVolume vol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sfx-play?? [what key & [options]]
  (let [{:keys [vol repeat?]} options
        p (get-in @xcfg [:assets :sounds key])]
    (do->nil
      (when (and (sfx?)
                 (some? p))
        (sfx-music-vol! vol)
        (condp = what
          :music (js/cc.audioEngine.playMusic p repeat?)
          :effect (js/cc.audioEngine.playEffect p repeat?) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx-effect "Play effect." [m & [options]] (sfx-play?? :effect m options))
(defn sfx-music "Play music." [m & [options]] (sfx-play?? :music m options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx-cancel!
  "Stop all sound."
  []
  (do->nil (js/cc.audioEngine.stopMusic)
           (js/cc.audioEngine.stopAllEffects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xlive-icon
  "Create a live icon."
  [img & [options]]
  (set!! (x/sprite* img) options))
;create() { const dummy = new XLive(0,0,this.options); this.lifeSize = { width: ccsx.getScaledWidth(dummy), height: ccsx.getScaledHeight(dummy) } ; this.drawLives(); }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xlive-group
  "Create a new lives group."
  [hud img total pt direction]
  (atom {:topLeft pt
         :image img
         :totalLives total
         :icons [] :parent hud :curLives total :dir direction}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reduce-live!
  "Reduce one live."
  [g]
  (do-with [g]
    (swap! g (fn [{:keys [icons curLives] :as root}]
               (let [e (peek icons)]
                 (x/remove! e)
                 (assoc root
                        :icons (pop icons)
                        :curLives (dec curLives)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn count-live-icons "Get n# of lives left." [g] (:curLives @g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn no-live-icons?
  "If there are more lives?" [g] (pos? (count-live-icons g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-live-group!
  "Reset the lives-group."
  [g]
  (do-with [g]
    (swap! g (fn [{:keys [totalLives icons] :as root}]
               (doseq [x icons] (x/remove! x))
               (assoc root :curLives totalLives :icons [])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-live-icons
  "Draw the lives-group."
  [g]
  (let [{:keys [topLeft image
                dir parent curLives]} @g
        [width height] (r-> (bsize image))
        [hw hh] (c/mapfv / 2 width height)
        w (* width (if (pos? dir) 1 -1))]
    (do-with [g]
      (swap! g
             #(assoc %
                     :icons
                     (loop [n 0 arr []
                            y (- (_1 topLeft) hh)
                            x (+ (_2 topLeft) hw)]
                       (if (>= n curLives)
                         arr
                         (recur (+ 1 n)
                                (->> {:pos (js/cc.p x y)}
                                     (xlive-icon image)
                                     (add-> parent) (conj arr)) y (+ x w)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn audio-icon
  "Create the audio - sound control icon."
  [yes no & [options]]
  (x/menu* (-> (x/mitoggle* (x/misprite* yes nil)
                            (x/misprite* no nil)
                            #(sfx! (zero? (gsidx %))))
               (set!! options)
               (toggle-select! (if (sfx?) 0 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn quit!
  "What happens when we quit."
  [ctor]
  (do->nil
    (x/push-scene (ctor {:no #(x/pop-scene)
                         :yes (fn_1 (x/pop->root)
                                    (run-next-scene ____1))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn center!!
  "Add this to the center of R."
  [R node obj & [tag zOrder]]
  (add-> node (set!! obj
                     {:pos (mid-rect R)}) tag zOrder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn center-image
  "Add this image to the center of R."
  [R node frame & [tag zOrder]]
  (center!! R node (x/sprite* frame) tag zOrder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn peg-to-anchor
  "Peg something to its parent based on the anchor-points."
  [R node where]
  (let [[x y] (p-> (mid-rect R))
        {:keys [top bottom left right]} (r->b4 R)]
    (do-with [node]
      (x/pos! node
              (condp = where
                ANCHOR-TOP-LEFT (js/cc.p left top)
                ANCHOR-TOP (js/cc.p x top)
                ANCHOR-TOP-RIGHT (js/cc.p right top)
                ANCHOR-LEFT (js/cc.p left y)
                ANCHOR-CENTER (js/cc.p x y)
                ANCHOR-RIGHT (js/cc.p right y)
                ANCHOR-BOTTOM-LEFT (js/cc.p left bottom)
                ANCHOR-BOTTOM (js/cc.p x bottom)
                ANCHOR-BOTTOM-RIGHT (js/cc.p right bottom)
               (raise! "pegToAnchor bad where = " where))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hgsort
  "Sort scores in order."
  [scores]
  (sort (c/compare-asc* #(_2 %)) scores))

;import Cookies from 'Cookies';
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkscore
  "Create a score data object."
  [n v]
  [(cs/trim n) (js/Number (cs/trim v))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-high-scores
  "Grab high-scores from cookies."
  [hgObj]
  (let [{:keys [ckey]} @hgObj
        s (cs/trim (or (js/Cookies.get ckey) ""))]
    (do-with [hgObj]
      (swap! hgObj
             #(assoc %
                     :scores
                     (reduce
                       (fn [acc z]
                         (let [a (cs/split (or z "") ":")]
                           (if (= 2 (n# a))
                             (conj acc
                                   (mkscore (_1 a) (_2 a))) acc)))
                       []
                       (cs/split s "|")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reset-high-scores!
  "Reset the high scores."
  [hgObj]
  (do-with [hgObj]
    (swap! hgObj #(assoc % :scores []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn write-high-scores!
  "Flush high scores to the cookie."
  [hgObj]
  (let [{:keys [ckey scores duration]} @hgObj]
    (do-with [hgObj]
      (js/Cookies.set ckey
                      (cs/join "|"
                               (map #(str (_1 %)
                                          ":"
                                          (_2 %)) scores)) duration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insert-high-scores!
  "Insert a score into the high-scores."
  [hgObj name score]
  (let [{:keys [scores size]} @hgObj
        len (n# scores)]
    (do-with [hgObj]
      (when (or (and (>= len size)
                     (some #(< (_2 %) score) scores))
                (< len size))
        (let [s (mkscore name score)
              scores (conj scores s)
              scores (hgsort scores)
              len (n# scores)
              scores (if (< len size)
                       scores
                       (take size scores))]
          (swap! hgObj
                 #(assoc %
                         :scores
                         (into [] scores)))
          (write-high-scores! hgObj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-high-scores
  "Create a high-scores data object."
  [key size & [duration]]
  (atom {:size size :scores [] :ckey key
         :duration (or duration (* 60 60 24 1000))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn attr*
  "Set js-object properties."
  [node attrsObj]
  {:pre [(object? attrsObj)]}
  (ocall! node "attr" attrsObj) node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patch the config object!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! xcfg
        {:AD {:anchor ANCHOR-BOTTOM :on? true :htperc 0.09}
         :url-prefix "/public/mcfud/"
         :version ""
         :appid ""
         :assets {:images {} :sheets {}
                  :tiles {} :fonts {} :sounds {}
                  :loader {::czlab "core/ZotohLab.png"
                           ::preloader "core/preloader_bar.png"}}
         :audio {:volume 0.5 :open? true :track nil}
         :levels {}
         :start-scene fn-nil
         :run-once fn-nil
         :game {:pother {:ptype P-BOT :pid "" :pname ""}
                :player {:ptype P-MAN :pid "" :pname ""}
                :policy js/cc.ResolutionPolicy.SHOW_ALL
                :size (js/cc.rect)
                :resdir ""
                :scale 1
                :sfx :mp3
                :gravity 0
                :landscape? true
                :preload-levels? true}
         :l10n {:en {"%mobileStart" "Press Anywhere To Start!"
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
                     "%quit?" "Continue and quit game?" }} })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

