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

  (:require-macros [czlab.elmo.afx.core :as ec])
  (:require [goog.object :as go]
            [czlab.elmo.afx.core :as ec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointInBox? "" [box x & [y]]
  (let [[px py]
        (if (number? y)
          [x y] [(oget x "x") (oget x "y")])]
    (and (>= px (oget box "left"))
         (<= px (oget box "right"))
         (>= py (oget box "bottom"))
         (<= py (oget box "top")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities using cc-rects.  Each entity
  wraps a sprite object."
  [a b]
  (cond (and (instance? js/cc.Sprite a)
             (instance? js/cc.Sprite b))
        (collide? (bbox a) (bbox b))
        (and (go/containsKey a "piccy")
             (go/containsKey b "piccy"))
        (collide? (oget a "piccy")(oget b "piccy"))
        (and (go/containsKey a "width")
             (go/containsKey b "width"))
        (js/cc.rectIntersectsRect a b) :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setDevRes!
  "Set device resolution, policy and orientation."
  [landscape? w h pcy]
  (if landscape?
    (js/cc.view.setDesignResolutionSize w h pcy)
    (js/cc.view.setDesignResolutionSize h w pcy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isPortrait?
  "Test if the screen is oriented vertically." []
  (let [s (screen)] (> (oget s "height") (oget s "width"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn outOfBound?
  "Test if this entity is out of bound."
  [ent B]
  (let [a (bbox4 (oget ent "piccy"))
        b (or B (vbox))]
    (or (> (oget a "left")(oget b "right"))
        (< (oget a "top")(oget b "bottom"))
        (< (oget a "right")(oget b "left"))
        (> (oget a "bottom")(oget b "top")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn undoTimer
  "Maybe release this timer."
  [p t]
  (if (and js/cc.sys.isNative (some? t)) (ocall t "release")) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn createTimer
  "Create a timer action."
  [p t]
  (let [rc (ocall p
                  "runAction"
                  (new js/cc.DelayTime t))]
    (if js/cc.sys.isNative (ocall rc "retain"))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn timerDone
  "Test if this timer is done."
  [t] (and (some? t) (ocall t "isDone")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sprite* "" [arg] (new js/cc.Sprite arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox4
  "Create a 4 point rectangle from this sprite."
  [pic]
  #js{:bottom (getBottom pic)
      :top (getTop pic)
      :left (getLeft pic)
      :right (getRight pic)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runScene
  "" [ns delay]
  (js/cc.director.runScene
    (new js/cc.TransitionCrossFade (or delay 0.6) ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isTransitioning "" []
  (instance? js/cc.TransitionScene (js/cc.director.getRunningScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn csize
  "Find size of this thing."
  [arg]
  (cond
    (instance? js/cc.Sprite arg)
    (ocall arg "getContentSize")
    (string? arg)
    (csize (sprite* arg))
    (go/containsKey arg "width")
    arg
    (go/containsKey arg "bottom")
    (js/cc.size (- (oget arg "right")
                   (oget arg "left"))
                (- (oget arg "top")
                   (oget arg "bottom")))
    :else (js/cc.size 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn halfHW
  "Calculate halves of width and height of this sprite."
  [arg]
  (let [z (csize arg)]
    [(* 0.5 (oget z "width")) (* 0.5 (oget z "height"))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bbox
  "Create a rectangle from this sprite."
  [sprite]
  (js/cc.rect (getLeft sprite)
              (getBottom sprite)
              (getWidth sprite)
              (getHeight sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledHeight
  "Get the scaled height."
  [sprite] (* (ocall sprite "getScaleY") (getHeight sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getHeight
  "" [sprite] (oget (csize sprite) "height"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaledWidth
  "Get the scaled width."
  [sprite] (* (ocall sprite "getScaleX") (getWidth sprite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWidth
  "" [sprite] (oget (csize sprite) "width"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPos
  "" [sprite] (ocall sprite "getPosition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getLeft "" [sprite]
  (- (oget (getPos sprite) "x") (* 0.5 (getWidth sprite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getRight "" [sprite]
  (+ (oget (getPos sprite) "x") (* 0.5 (getWidth sprite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTop "" [sprite]
  (+ (oget (getPos sprite) "y") (* 0.5 (getHeight sprite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getBottom "" [sprite]
  (- (oget (getPos sprite) "y") (* 0.5 (getHeight sprite))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerX
  "Get the x pos of the center of the visible screen."
  [] (oget (centerPos) "x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerY
  "Get the y pos of the center of the visible screen."
  [] (oget (centerPos) "y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centerPos "" []
  (let [rc (vrect)]
    (js/cc.p (+ (oget rc "x")
                 (* 0.5 (oget rc "width")))
             (+ (oget rc "y")
                (* 0.5 (oget rc "height"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenHeight "" [] (oget (screenSize) "height"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenWidth "" [] (oget (screenSize) "width"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vrect
  "Get the visible screen rectangle."
  []
  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    (js/cc.rect (oget vo "x")
                (oget vo "y")
                (oget wz "width")
                (oget wz "height"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vbox
  "Get the visible screen box."
  []
  (let [vo (js/cc.view.getVisibleOrigin)
        wz (js/cc.view.getVisibleSize)]
    #js{:bottom (oget vo "y")
        :left (oget vo "x")
        :right (+ (oget vo "x") (oget wz "width"))
        :top (+ (oget vo "y") (oget wz "height"))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenSize
  "Get the actual window/frame size."
  []
  (if js/cc.sys.isNative
    (js/cc.view.getFrameSize)
    (js/cc.director.getWinSize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn screenCenterPos "" []
  (let [sz (screenSize)]
    (js/cc.p (* 0.5 (oget sz "width"))
             (* 0.5 (oget sz "height")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vboxMID "" [box4]
  (js/cc.p (+ (oget box4 "left")
              (* 0.5 (- (oget box4 "right")
                        (oget box4 "left"))))
           (+ (oget box4 "bottom")
              (* 0.5 (- (oget box4 "top")
                        (oget box4 "bottom"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn traceEnclosure
  "Test if this box is hitting boundaries.
  rect.x & y are center positioned.
  If hit, the new position and velocities are returned."
  [dt bbox4 rect vel]
  (let [sz (* 0.5 (oget rect "height"))
        sw (* 0.5 (oget rect "width"))
        vx (oget vel "x")
        vy (oget vel "y")
        y (+ (oget rect "y") (* dt vy))
        x (+ (oget rect "x") (* dt vx))
        [x1 y1 vx1 vy1 t?]
        (cond
          (> (+ y sz) (oget bbox4 "top")) ;;hitting top wall
          [x (- (oget bbox4 "top") sz) vx (- vy) true]
          (< (- y sz) (oget bbox4 "bottom")) ;;hitting bottom wall
          [x (+ (oget bbox4 "bottom") sz) vx (- vy) true]
          :else [x y vx vy false])
        [x2 y2 vx2 vy2 t2?]
        (cond
          (> (+ x sw) (oget bbox4 "right")) ;;hitting right wall
          [(- (get bbox4 "right") sw) y1 (- vx1) vy1 true]
          (< (- x sw) (oget bbox4 "left")) ;;hitting left wall
          [(+ (oget bbox4 "left") sw) y1 (- vx1) vy1 true]
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
(defn hasKeyPad? "" []
  (and (not js/cc.sys.isNative)
       (some? (oget js/cc.sys.capabilities "keyboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeyPolls "" [kb]
  (if (hasKeyPad?)
    (js/cc.eventManager.addListener
      #js{:onKeyPressed (fn [key e] (aset kb key true))
          :onKeyReleased (fn [key e] (aset kb key false))
          :event js/cc.EventListener.KEYBOARD})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onKeys "" [bus]
  (if (hasKeyPad?)
    (js/cc.eventManager.addListener
      #js{:onKeyPressed (fn [key e]
                          (ebus/pub bus "key.down" [key e]))
          :onKeyReleased (fn [key e]
                           (ebus/pub bus "key.up" [key e]))
          :event js/cc.EventListener.KEYBOARD})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasMouse? "" []
  (some? (oget js/cc.sys.capabilities "mouse")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onMouse "" [bus]
  (if (hasMouse?)
    (js/cc.eventManager.addListener
      #js{:onMouseMove
          (fn [e]
            (if (= (ocall e "getButton")
                   js/cc.EventMouse.BUTTON_LEFT)
              (ebus/pub bus "mouse.move" e)))
          :onMouseDown
          (fn [e] (ebus/pub bus "mouse.down" e))
          :onMouseUp
          (fn [e] (ebus/pub bus "mouse.up" e))
          :event js/cc.EventListener.MOUSE})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hasTouch? "" []
  (some? (oget js/cc.sys.capabilities "touches")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchAll "" [bus]
  (if (hasTouch?)
    (js/cc.eventManager.addListener
      #js{:event js/cc.EventListener.TOUCH_ALL_AT_ONCE
          :prevTouchId -1
          :onTouchesBegan (fn [ts e] true)
          :onTouchesEnded
          (fn [ts e]
            (ebus/pub bus "touch.all.end" [ts e]))
          :onTouchesMoved
          (fn [ts e]
            (this-as
              self
              (let [id (oget (aget ts 0) "id")]
                (if (not= (oget self "prevTouchId") id)
                  (oset! self "prevTouchId" id)
                  (ebus/pub bus "touch.all.move" [ts e])))))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onTouchOne "" [bus]
  (if (hasTouch?)
    (js/cc.eventManager.addListener
      #js{:event js/cc.EventListener.TOUCH_ONE_BY_ONE
          :swallowTouches true
          :onTouchBegan (fn [t e] true)
          :onTouchMoved
          (fn [t e]
            (ebus/pub bus "touch.one.move" [t e]))
          :onTouchEnded
          (fn [t e]
            (ebus/pub bus "touch.one.end" [t e]))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *anchors*
  {:Center (js/cc.p 0.5 0.5)
   :Top (js/cc.p 0.5 1)
   :TopRight (js/cc.p 1 1)
   :Right (js/cc.p 1 0.5)
   :BottomRight (js/cc.p 1 0)
   :Bottom (js/cc.p 0.5 0)
   :BottomLeft (js/cc.p 0 0)
   :Left (js/cc.p 0 0.5)
   :TopLeft (js/cc.p 0 1)})


  /**
   * Create a text menu containing this set of items.
   *
   * Each item has the form {:text
   * :fontPath
   * :cb
   * :target}
   * @method
   * @param {Array} items
   * @param {Number} scale
   * @return {cc.Menu}
   */
  tmenu(items,scale) {
    let menu= new cc.Menu(),
    mi,
    t=0,
    obj;

    for (let n=0; n < items.length; ++n) {
      obj= items[n];
      mi= new cc.MenuItemLabel(new cc.LabelBMFont(obj.text,
                                                  obj.fontPath),
                               obj.selector || obj.cb,
                               obj.target);
      mi.setOpacity(255 * 0.9);
      mi.setScale(scale || 1);
      mi.setTag(++t);
    }
    return menu;
  },

  /**
   * Make a text label menu containing one single button.
   * @method
   * @param {Object} options
   * @return {cc.Menu}
   */
  tmenu1(options) {
    let menu = this.tmenu(options);
    if (options.anchor) { menu.setAnchorPoint(options.anchor); }
    if (options.pos) { menu.setPosition(options.pos); }
    if (options.visible === false) { menu.setVisible(false); }
    menu.alignItemsVertically();
    return menu;
  },

  /**
   * Create a vertically aligned menu with graphic buttons.
   * @method
   * @param {Array} items
   * @param {Object} options
   * @return {cc.Menu}
   */
  vmenu(items, options) {
    const hint=options || {},
    m= this.pmenu(true,
                  items,
                  hint.scale,
                  hint.padding);
    if (!!hint.pos) {
      m.setPosition(hint.pos);
    }
    return m;
  },

  /**
   * Create a horizontally aligned menu with graphic buttons.
   * @method
   * @param {Array} items
   * @param {Object} options
   * @return {cc.Menu}
   */
  hmenu(items, options) {
    const hint= options || {},
    m= this.pmenu(false,
                  items,
                  hint.scale,
                  hint.padding);
    if (!!hint.pos) {
      m.setPosition(hint.pos);
    }
    return m;
  },

  /**
   * Create a menu with graphic buttons.
   * @method
   * @param {Boolean} vertical
   * @param {Array} items
   * @param {Number} scale
   * @param {Number} padding
   * @return {cc.Menu}
   */
  pmenu(vertical, items, scale, padding) {
    let menu = new cc.Menu(),
    obj,
    mi,
    t=0;

    for (let n=0; n < items.length; ++n) {
      obj=items[n];
      mi= new cc.MenuItemSprite(new cc.Sprite(obj.nnn),
                                new cc.Sprite(obj.sss || obj.nnn),
                                new cc.Sprite(obj.ddd || obj.nnn),
                                obj.selector || obj.cb,
                                obj.target);
      if (!!obj.color) { mi.setColor(obj.color); }
      if (!!scale) { mi.setScale(scale); }
      mi.setTag(++t);
      menu.addChild(mi);
    }

    padding = padding || 10;
    if (!vertical) {
      menu.alignItemsHorizontallyWithPadding(padding);
    } else {
      menu.alignItemsVerticallyWithPadding(padding);
    }

    return menu;
  },

  /**
   * Create a single button menu.
   * @method
   * @param {Object} options
   * @return {cc.Menu}
   */
  pmenu1(options) {
    const menu = this.pmenu(true, [options]);
    if (options.anchor) { menu.setAnchorPoint(options.anchor); }
    if (options.pos) { menu.setPosition(options.pos); }
    if (options.visible === false) { menu.setVisible(false); }
    return menu;
  },

  /**
   * Create a Label.
   * @method
   * @param {Object} options
   * @return {cc.LabelBMFont}
   */
  bmfLabel(options) {
    let f= new cc.LabelBMFont(options.text, options.fontPath);
    if (options.color) { f.setColor(options.color); }
    if (options.pos) { f.setPosition(options.pos); }
    if (options.anchor) { f.setAnchorPoint(options.anchor); }
    if (options.visible === false) { f.setVisible(false); }
    f.setScale( options.scale || 1);
    f.setOpacity(0.9*255);
    return f;
  }

};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/

//////////////////////////////////////////////////////////////////////////////
//EOF

