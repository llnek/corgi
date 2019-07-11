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
                                  oget-id oget-piccy
                                  oget-x oget-y oget-height oget-width]])

  (:require [clojure.string :as cs]
            [goog.object :as go]
            [oops.core :as oc]
            [czlab.mcfud.afx.ebus :as e]
            [czlab.mcfud.afx.math :as m]
            [czlab.mcfud.afx.core
             :as c :refer [defenum do#true do#nil let#nil
                           fn-nil is? fn_0 fn_* fn_1 fn_2 cc+ cc+1
                           if-string n# _1 _2 do-with raise! num??]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def xcfg (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn browser-size []
  (cond (and js/window
             (c/js-prop? js/window :innerWidth)
             (number? (c/get-js js/window :innerWidth)))
        [(c/get-js js/window :innerWidth)
         (c/get-js js/window :innerHeight)]
        (and js/document
             (c/js-prop? js/document :documentElement)
             (c/js-prop? js/document.documentElement :clientWidth))
        [(c/get-js js/document.documentElement :clientWidth)
         (c/get-js js/document.documentElement :clientHeight)]
        (and js/document
             (c/js-prop? js/document :body)
             (c/js-prop? js/document.body :clientWidth))
        [(c/get-js js/document.body :clientWidth)
         (c/get-js js/document.body :clientHeight)]
        :else
        (c/raise! "Failed to find browser size")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn r->
  "Destruct, get width & height."
  [r] [(oget-width r) (oget-height r)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn p->
  "Destruct, get x & y."
  [p] [(oget-x p) (oget-y p)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pos* [obj] (p-> (x/pos?? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox
  "Node.getBoundingBox."
  [n] (c/call-js! n :getBoundingBox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox*
  "Node.getBoundingBox for all."
  [& args] (mapv #(c/call-js! % :getBoundingBox) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gcbyn+
 "Get nested children under a node."
 [node & args] (reduce #(x/gcbyn %1 %2) node args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gcbyn*
 "Get children under a node."
 [node & args] (map #(x/gcbyn node %1) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn crect?
  "If object is a cc-rect?"
  [obj]
  (and (object? obj)
       (oget-width obj) (oget-height obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- snode?
  "If node has a sprite property?"
  [obj] (and (object? obj) (oget-piccy obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn r->b4
  "Get the 4 sides of a rect."
  [r]
  (let [x (oget-x r) y (oget-y r)
        w (oget-width r) h (oget-height r)]
    {:top (+ y h)
     :right (+ x w)
     :left x
     :bottom y
     :low y :lhs x :rhs (+ x w)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bbox?? [obj]
  (cond (snode? obj) (bbox?? (oget-piccy obj))
        (cnode? obj) (bbox?? (bbox obj))
        (crect? obj) obj
        :else (c/raise! "bad call bbox??")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pt?? [obj]
  (cond (snode? obj) (pt?? (oget-piccy obj))
        (cnode? obj) (pt?? (x/pos?? obj))
        (and (object? obj)
             (c/js-prop? obj :x)
             (c/js-prop? obj :y)) obj
        :else (c/raise! "bad call pt??")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn contains-pt?
  [obj pt]
  (js/cc.rectContainsPoint (bbox?? obj) (pt?? pt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn minX [obj] (js/cc.rectGetMinX (bbox?? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn minY [obj] (js/cc.rectGetMinY (bbox?? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn maxX [obj] (js/cc.rectGetMaxX (bbox?? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn maxY [obj] (js/cc.rectGetMaxY (bbox?? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities."
  [a b]
  (js/cc.rectIntersectsRect (bbox?? a) (bbox?? b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn grabbed?
  "If mouse or touch on object?" [obj evt]
  (contains-pt? (bbox obj) (c/call-js! obj :getLocation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move-pos!
  "Move an object." [obj evt]
  (do-with [obj]
    (let [[ox oy] (pos* obj)
          [dx dy] (c/call-js! evt :getDelta)]
      (x/pos! obj (+ ox dx) (+ oy dy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn design-res!
  "Set design resolution, display policy."
  [width height policy]
  (js/cc.view.setDesignResolutionSize width height policy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect
  "Get the visible view's rectangle."
  []
  ;no good in browser ;(js/cc.view.getViewPortRect)
  (comment
    (let [r js/cc.visibleRect
          bl (c/get-js r :bottomLeft)]
      (js/cc.rect (oget-x bl)
                  (oget-y bl) (oget-width r) (oget-height r))))
  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    (js/cc.rect (oget-x vo)
                (oget-y vo) (oget-width wz) (oget-height wz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect*
  "Get the visible view's width & height." [] (r-> (vrect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect-b4 "Get vrect as box4." [] (r->b4 (vrect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ebox
  "not-used"
  []
  (let [{:keys [htPerc anchor]} (:AD @xcfg)
        [x y] (p-> (js/cc.view.getVisibleOrigin))
        [w h] (r-> (js/cc.view.getVisibleSize))] (js/cc.rect x y
                                                             w (- h (* h htPerc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mid-rect*
  "Center point of a rect."
  ([] (mid-rect* (vrect)))
  ([r]
   [(js/cc.rectGetMidX r) (js/cc.rectGetMidY r)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mid-rect
  "Center point of a rect."
  ([] (mid-rect (vrect)))
  ([r]
   (js/cc.p (js/cc.rectGetMidX r) (js/cc.rectGetMidY r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn frame-size*
  "Actual window/frame width & height."
  []
  (comment (if (native?)
             (js/cc.view.getFrameSize)
             (js/cc.director.getWinSize)))
  (let [z (js/cc.view.getFrameSize)]
    [(oget-width z) (oget-height z)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn frame-size
  "Actual window/frame size."
  []
  (let [z (js/cc.view.getFrameSize)]
    (js/cc.rect 0 0 (oget-width z)(oget-height z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn undo-timer!
  "Maybe release this timer."
  [p t]
  (if (and (native?) t) (c/call-js! t :release)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-timer
  "Create a timer action."
  [p t]
  (do-with
    [rc (->> (new js/cc.DelayTime t)
             (c/call-js! p :runAction))]
    (if (native?) (c/call-js! rc :retain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timer-done?
  "If timer is done."
  [t] (and t (c/call-js! t :isDone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-next-scene
  "Run a scene."
  ([nx] (run-next-scene nx .6))
  ([nx delaySecs]
   (x/run-scene (new js/cc.TransitionCrossFade delaySecs nx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-transitioning?
  "If transitioning between scenes?"
  [] (instance? js/cc.TransitionScene
                (js/cc.director.getRunningScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize
  "Content-size of this thing."
  [obj]
  (if-some [z (c/call-js! obj :getContentSize)]
    (js/cc.rect 0 0
                (oget-width z)
                (oget-height z)) (js/cc.rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize* [obj] (r-> (csize obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bsize
  "Find size of object's bounding-box."
  [obj]
  (cond (string? obj) (bsize (x/sprite* obj))
        (snode? obj) (bsize (oget-piccy obj))
        (cnode? obj) (bsize (bbox obj))
        (crect? obj) obj
        :else (c/raise! "bad call bsize")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bsize* [obj] (r-> (bsize obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-rect
  "Calculate halves of width and height."
  [r] (js/cc.rect 0 0
                  (/ (oget-width r) 2)
                  (/ (oget-height r) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn half-rect* [rc] (r-> (half-rect rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-height
  "Get the height of a sprite." [sprite] (_2 (bsize* sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-scaled-height
  "Get the scaled height."
  [sprite]
  (* (c/call-js! sprite :getScaleY) (get-height sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-width
  "Get the width of a sprite." [sprite] (_1 (bsize* sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-scaled-width
  "Get the scaled width."
  [sprite]
  (* (c/call-js! sprite :getScaleX) (get-width sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trace-enclosure
  "Test if this box is hitting boundaries.
  If hit, the new position and velocities are returned."
  [W rect vel dt]
  (let [{:keys [top low lhs rhs]} (r->b4 W)
        [hw hh] (half-rect* rect)
        [cx cy] (mid-rect* rect)
        [vx vy] (p-> vel)
        y (+ cy (* dt vy))
        x (+ cx (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y hh) top) ;;hitting top wall
          [x (- top hh) vx (- vy) true]
          (< (- y hh) low) ;;hitting bottom wall
          [x (+ low hh) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x hw) rhs) ;;hitting right wall
          [(- rhs hw) y1 (- vx1) vy1 true]
          (< (- x hw) lhs) ;;hitting left wall
          [(+ lhs hw) y1 (- vx1) vy1 true]
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
  ([size] (set-font! size nil))
  ([size font]
   (js/cc.MenuItemFont.setFontSize size)
   (js/cc.MenuItemFont.setFontName (or font "Arial"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mifont-item*
  "Create a menu-item."
  ([item size] (mifont-item* item size nil))
  ([item size font]
   (set-font! size font) (x/mifont* item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mifont-text*
  "Create a menu-item label."
  ([item size] (mifont-text* item size nil))
  ([item size font]
   (do-with [f (mifont-item* item size font)]
            (c/call-js! f :setEnabled false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggle-select!
  "Select button in a toggle."
  [t v] (c/call-js! t :setSelectedIndex v) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pkeys [pmap] [(get pmap CV-X) (get pmap CV-O)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; event subscriptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenum MOUSE UP 200 DOWN MOVE)
(defenum KEY UP 100 DOWN)
(defenum TOUCH-ONE END 300 MOVE)
(defenum TOUCH-ALL END 400 MOVE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ecb-p2 [t] (fn [a b] (e/pub (:ebus @xcfg) t a b)))
(defn- ecb-p1 [t] (fn [a] (e/pub (:ebus @xcfg) t a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- keys-listener-obj []
  #js {:onKeyPressed
       (fn_2 (aset (get-in @xcfg [:game :keybd]) ____1 true))
       :onKeyReleased
       (fn_2 (aset (get-in @xcfg [:game :keybd]) ____1 false))
       :event js/cc.EventListener.KEYBOARD})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mouse-listener-obj []
  #js {:event js/cc.EventListener.MOUSE
       :onMouseDown (ecb-p1 MOUSE-DOWN)
       :onMouseUp (ecb-p1 MOUSE-UP)
       :onMouseMove (fn [e]
                      (if (= (c/call-js! e "getButton")
                             js/cc.EventMouse.BUTTON_LEFT)
                        (e/pub (:ebus @xcfg) MOUSE-MOVE e)))})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- touchall-listener-obj []
  (let [prev #js [-1]]
    #js{:onTouchesBegan c/fn-true
        :onTouchesEnded (ecb-p2 TOUCH-ALL-END)
        :event js/cc.EventListener.TOUCH_ALL_AT_ONCE
        :onTouchesMoved (fn [a b]
                          (let [id (c/call-js! (_1 a) :getID)]
                            (if (not= (_1 prev) id)
                              (aset prev 0 id)
                              (e/pub (:ebus @xcfg) TOUCH-ALL-MOVE a b))))}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- touchone-listener-obj []
  #js {:event js/cc.EventListener.TOUCH_ONE_BY_ONE
       :onTouchBegan c/fn-true
       :swallowTouches true
       :onTouchMoved (ecb-p2 TOUCH-ONE-MOVE)
       :onTouchEnded (ecb-p2 TOUCH-ONE-END)})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subevent
  "Add an event-listener, storing it for removal."
  [obj-ctor ctx]
  (let [obj (obj-ctor)
        h (js/cc.eventManager.addListener obj ctx)]
    (swap! xcfg
           (fn_1 (update-in ____1
                            [:listeners] #(conj % h))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-keys?
  "If key-pad is available?"
  [] (and (not-native?)
       (some? (c/get-js js/cc.sys.capabilities :keyboard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-mouse?
  "If mouse is available?"
  [] (some? (c/get-js js/cc.sys.capabilities :mouse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn has-touch?
  "If touch is available?"
  [] (some? (c/get-js js/cc.sys.capabilities :touches)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- accept-keys [node]
  (when (has-keys?)
    (x/debug* "Accept key events")
    (subevent keys-listener-obj node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- accept-mouse [node]
  (when (has-mouse?)
    (x/debug* "Accept mouse events")
    (subevent mouse-listener-obj node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- accept-touch [node multi-touch?]
  (when (has-touch?)
    (if multi-touch?
      (do (x/debug* "Accept touch-all events")
          (subevent touchall-listener-obj node))
      (do (x/debug* "Accept touch-one events")
          (subevent touchone-listener-obj node)))))

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
    (c/raise! "anchorValue - bad anchor enum")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add->
  "Add a child to the node."
  ([node child] (add-> node child nil))
  ([node child tag] (add-> node child tag nil))
  ([node child tag zOrder]
   (do-with [child]
     (if (and (is? js/cc.SpriteBatchNode node)
              (x/sprite? child)) (c/call-js! child :setBatchNode node))
     (c/call-js! node
                 :addChild
                 child
                 (if (number? zOrder) zOrder js/undefined)
                 (if (number? tag) tag (if tag (name tag) js/undefined))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set!!
  "Set node attributes."
  ([node] (set!! node nil))
  ([node options]
   (let [{:keys [scale color
                 pos show? anchor]
          :or {show? true}} options]
     (do-with [node]
       (if (some? color) (x/color! node color))
       (if (some? pos) (x/pos! node pos))
       (x/visible! node show?)
       (if (number? scale) (x/scale! node scale))
       (if (some? anchor) (x/anchor! node anchor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hide! "" [obj] (x/visible! obj false))
(defn show! "" [obj] (x/visible! obj true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- peg-menu??
  "Place a menu by anchoring to its parent."
  [W anchor menu tag total padding flat?]
  (do-with [menu]
    (let [{:keys [top low lhs rhs]}
          (r->b4 W)
          [width height]
          (bsize* (x/gcbyt menu tag))
          t (+ (* padding (- total 1))
               (* total (if flat? width height)))
          [w h] (if flat? [t height] [width t])
          [cx cy :as cp] (mid-rect* W)
          [hw hh] (c/mapfv / 2 w h)
          [x y]
          (condp = anchor
            ANCHOR-TOP-LEFT [(+ lhs hw) (- top hh)]
            ANCHOR-TOP [cx (- top hh)]
            ANCHOR-TOP-RIGHT [(- rhs hw) (- top hh)]
            ANCHOR-LEFT  [(+ lhs hw) cy]
            ANCHOR-CENTER cp
            ANCHOR-RIGHT [(- rhs hw) cy]
            ANCHOR-BOTTOM-LEFT [(+ lhs hw) (+ low hh)]
            ANCHOR-BOTTOM [cx (+ low hh)]
            ANCHOR-BOTTOM-RIGHT [(- rhs hw) (+ low hh)]
            (c/raise! "pegMenu - bad anchor enum"))]
      (x/pos! menu x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tmenu
  "Create a text menu containing this set of items."
  ([items] (tmenu items nil))
  ([items options]
   (let [items (if (map? items) [items] items)
         tag 911
         {:keys [scale region anchor
                 flat? align? color padding]
          :or {align? true padding 10}} options]
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
                     anchor menu tag (n# items) padding flat?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gmenu
  "Create a menu with graphic buttons."
  ([items] (gmenu items nil))
  ([items options]
   (let [items (if (map? items) [items] items)
         tag 911
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
                     menu tag (n# items) padding flat?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bmf-label*
  "New bitmapfont label."
  ([text font] (bmf-label* text font nil))
  ([text font options]
   (set!! (x/bmf-text* text font) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gicfg "Get value from config." [& path] (get-in @xcfg path))

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
      (loop [i 0 out (c/tvec*)]
        (if (>= i SZ)
          (cs/join "" (c/ps! out))
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
                  (c/get-js :ebus))] (apply e/pub b (cc+1 topic args))))

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
  (let [p (:url-prefix @xcfg)]
    (if (not-native?) (str p path) path)))

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
  (do#nil (swap! xcfg #(assoc-in % [:audio :open?] s?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toggle-sfx!
  "Toggle the sound."
  []
  (do#nil
    (swap! xcfg
           (fn_1 (update-in ____1
                            [:audio :open?] #(not %))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx? "If sound is on?" [] (get-in @xcfg [:audio :open?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sfx-music-vol!
  "Set music volume."
  [vol]
  (do#nil (if (number? vol) (js/cc.audioEngine.setMusicVolume vol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sfx-play?? [what key & [options]]
  (let [{:keys [vol repeat?]} options
        p (gres+ :assets :sounds key)]
    (do#nil
      (when (and (sfx?) p)
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
  (do#nil (js/cc.audioEngine.stopMusic)
          (js/cc.audioEngine.stopAllEffects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xlive-icon
  "Create a live icon."
  ([img] (xlive-icon nil))
  ([img options]
   (set!! (x/sprite* img) options)))
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
        [width height] (bsize* image)
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
  ([yes no] (audio-icon yes no nil))
  ([yes no options]
   (x/menu* (-> (x/mitoggle* (x/misprite* yes nil)
                             (x/misprite* no nil)
                             #(sfx! (zero? (x/gsidx %))))
                (set!! options)
                (toggle-select! (if (sfx?) 0 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn quit!
  "What happens when we quit."
  [ctor]
  (do#nil
    (x/push-scene (ctor {:no #(x/pop-scene)
                         :yes (fn_1 (x/pop->root)
                                    (run-next-scene ____1))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn center!!
  "Add this to the center of W."
  ([W node obj] (center!! W node obj nil))
  ([W node obj tag] (center!! W node obj tag nil))
  ([W node obj tag zOrder]
   (add-> node (set!! obj {:pos (mid-rect W)}) tag zOrder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn center-image
  "Add this image to the center of W."
  ([W node frame] (center-image W node frame nil))
  ([W node frame tag] (center-image W node frame tag nil))
  ([W node frame tag zOrder] (center!! W node (x/sprite* frame) tag zOrder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn peg-to-anchor
  "Peg something to its parent based on the anchor-points."
  [R node where]
  (let [[x y] (mid-rect* R)
        {:keys [top low lhs rhs]} (r->b4 R)]
    (do-with [node]
      (x/pos! node
              (condp = where
                ANCHOR-TOP-LEFT (js/cc.p lhs top)
                ANCHOR-TOP (js/cc.p x top)
                ANCHOR-TOP-RIGHT (js/cc.p rhs top)
                ANCHOR-LEFT (js/cc.p lhs y)
                ANCHOR-CENTER (js/cc.p x y)
                ANCHOR-RIGHT (js/cc.p rhs y)
                ANCHOR-BOTTOM-LEFT (js/cc.p lhs low)
                ANCHOR-BOTTOM (js/cc.p x low)
                ANCHOR-BOTTOM-RIGHT (js/cc.p rhs low)
               (c/raise! "pegToAnchor bad where = " where))))))

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
  ([key size] (new-high-scores key size nil))
  ([key size duration]
   (atom {:size size :scores [] :ckey key
          :duration (or duration (* 60 60 24 1000))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn attr*
  "Set js-object properties."
  [node attrsObj]
  {:pre [(object? attrsObj)]}
  (c/call-js! node :attr attrsObj) node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn enable-events
  "Enable all possible event listeners."
  ([node] (enable-events node false))
  ([node multi-touch?]
   (do#nil
     (if (native?)
       (accept-touch node multi-touch?)
       (doto node (accept-keys) (accept-mouse))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn disable-events
  "Disable all event listeners."
  []
  (do#nil
    (swap! xcfg
           (fn [root]
             (doseq [v (:listeners root)]
               ;(debug* "removing listener= " v)
               (js/cc.eventManager.removeListener v))
             (assoc root :listeners [])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-grid
  ""
  ([region size drawer] (draw-grid region size drawer false))
  ([region size drawer border?]
   (let#nil
     [{:keys [lhs rhs top low]} (r->b4 region)
      cw (/ (- rhs lhs) size)
      rh (/ (- top low) size)]
     (x/debug* "cw = " cw ", rh= " rh)
     (dotimes [n size]
       (cond
         (= n 0)
         (when border?
           (drawer (x/ccp* lhs top) (x/ccp* rhs top))
           (drawer (x/ccp* lhs top) (x/ccp* lhs low)))
         (= n size)
         (when border?
           (drawer (x/ccp* lhs low) (x/ccp* rhs low))
           (drawer (x/ccp* rhs top) (x/ccp* rhs low)))
         :else
         (let [x (+ lhs (* n cw))
               y (- top (* n rh))]
           (drawer (x/ccp* lhs y) (x/ccp* rhs y))
           (drawer (x/ccp* x top) (x/ccp* x low))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-scene [scene]
  (.call js/cc.Node.prototype.onEnter scene)
  (enable-events (x/gcbyn scene :arena)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-scene-enter [scene] (fn_0 (on-scene scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-scene-exit [scene]
  (fn_0 (disable-events)
        (.call js/cc.Node.prototype.onExit scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn reg-game-scene
  "Register the game scene and add the arena layer."
  ([scene] (reg-game-scene scene nil))
  ([scene zOrder]
   (swap! xcfg
          (fn_1 (update-in ____1
                           [:game]
                           #(assoc % :scene scene))))
   (add-> scene (x/layer*) :arena zOrder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-game-status! "" [running?]
  (swap! xcfg #(assoc-in % [:game :running?] running?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hook-update
  ([scene fstep] (hook-update scene fstep false))
  ([scene fstep run?]
   (let#nil []
     (attr* scene
            #js{:update fstep
                :onExit (on-scene-exit scene)
                :onEnter (on-scene-enter scene)})
     (c/call-js! scene :scheduleUpdate)
     (if run? (set-game-status! true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bootstrap
  "Bootstrap framework."
  [work]
  (let [lang (keyword js/cc.sys.language)
        f #(c/merge+ %
                     (js/cc.game.____configurator))]
    (x/debug* "bootstrap(), lang= " (name lang))
    (swap! xcfg #(assoc (f %) :lang lang))
    (work)
    (sfx-music-vol! (get-in @xcfg [:audio :volume]))
    (let [[w h] (r-> (js/cc.view.getDesignResolutionSize))]
      (x/debug* "design= [" w ", " h "], loaded & running.  OK"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patch the config object!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! xcfg
        {:AD {:anchor ANCHOR-BOTTOM :on? true :htperc 0.09}
         :url-prefix "res/"
         :version ""
         :appid ""
         :assets {:images {} :sheets {}
                  :tiles {} :fonts {} :sounds {}
                  :loader {:czlab "core/ZotohLab_x1024.png"
                           :preloader "core/preloader_bar.png"}}
         :audio {:volume 0.5 :open? true :track nil}
         :ebus (e/new-event-bus)
         :listeners []
         :levels {}
         :start-scene fn-nil
         :run-once fn-nil
         :game {:pother {:ptype P-BOT :pid "" :pname ""}
                :player {:ptype P-MAN :pid "" :pname ""}
                :policy js/cc.ResolutionPolicy.SHOW_ALL
                :size (js/cc.rect)
                :keybd #js[]
                :resdir ""
                :scale 1
                :sfx :mp3
                :gravity 0
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
                     "%1player" "Single Player"
                     "%online" "Online"
                     "%gameOver" "Game Over"
                     "%playMore?" "Play Again?"
                     "%quit!" "Quit"
                     "%back" "Go Back"
                     "%sound" "Sound"
                     "%off" "Off"
                     "%on" "On"
                     "%ok" "OK"
                     "%cancel" "Cancel"
                     "%mmenu" "Main Menu"
                     "%options" "Options"
                     "%replay" "REPLAY"
                     "%play" "PLAY"
                     "%tieGame" "It's a draw!"
                     "%winGame" "{} wins!"
                     "%waitOthers" "Waiting...\nfor other players."
                     "%waitOther" "Waiting...\nfor another player."
                     "%signinPlay" "Please sign in to play."
                     "%quit?" "Continue and quit game?" }} })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

