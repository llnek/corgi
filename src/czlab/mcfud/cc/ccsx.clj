;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.cc.ccsx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro not-native? [] `(not (czlab.mcfud.cc.ccsx/native?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro native? [] `~'js/cc.sys.isNative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug* [& msgs] `(js/cc.log (str ~@msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gebyid
  "Document.getElementById."
  [id]
  `(if-not (czlab.mcfud.cc.ccsx/native?) (js/document.getElementById ~id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite?
  "If object is a Sprite."
  [obj] `(instance? ~'js/cc.Sprite ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cnode?
  "If object is a Node."
  [obj] `(instance? ~'js/cc.Node ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cmenu?
  "If object is a Menu."
  [obj] `(instance? ~'js/cc.Menu ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro visible!
  "Node.setVisible."
  [node v?]
  `(oops.core/ocall! ~node "setVisible" ~v?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro anchor!
  "Node.setAnchorPoint."
  [node p]
  `(oops.core/ocall! ~node
                     "setAnchorPoint"
                     (czlab.mcfud.cc.ccsx/anchor-value ~p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro color*
  "Create a color object."
  [& args] `(js/cc.color ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro color!
  "Node.setColor."
  [node & args]
  `(oops.core/ocall! ~node "setColor" (czlab.mcfud.cc.ccsx/color* ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro scale!
  "Node.setScale."
  [node n]
  (let [X (gensym)]
    `(czlab.mcfud.afx.core/if-number [~X ~n]
                                     (oops.core/ocall! ~node "setScale" ~X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pos*
  "Node.getPosition."
  [node]
  `(oops.core/ocall ~node "getPosition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gcbyn
  "Node.getChildByName."
  [parent n]
  `(oops.core/ocall ~parent "getChildByName" ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gcbyt
  "Node.getChildByTag."
  [parent t]
  `(oops.core/ocall ~parent "getChildByTag" ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gsidx
  "Node.getSelectedIndex."
  [obj]
  `(oops.core/ocall ~obj "getSelectedIndex"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro halign-items
  "Node.alignItemsHorizontally(WithPadding)."
  [menu & [arg]]
  (if (nil? arg)
    `(oops.core/ocall ~menu "alignItemsHorizontally")
    `(oops.core/ocall ~menu "alignItemsHorizontallyWithPadding" ~arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro valign-items
  "Node.alignItemsVertically(WithPadding)."
  [menu & [arg]]
  (if (nil? arg)
    `(oops.core/ocall ~menu "alignItemsVertically")
    `(oops.core/ocall ~menu "alignItemsVerticallyWithPadding" ~arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro align-in-cols
  "Node.alignItemsInColumns."
  [menu & args]
  `(oops.core/ocall ~menu "alignItemsInColumns" ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro align-in-rows
  "Node.alignItemsInRows."
  [menu & args]
  `(oops.core/ocall ~menu "alignItemsInRows" ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pos!
  "Node.setPosition."
  [node & args]
  (if (not-empty args)
    `(oops.core/ocall! ~node "setPosition" ~@args)
    `(oops.core/ocall! ~node "setPosition" 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro posX!
  "Node.setPositionX."
  [node x]
  (let [N (gensym)]
    `(czlab.mcfud.afx.core/if-number [~N ~x]
                                     (oops.core/ocall! ~node "setPositionX" ~N))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro posY!
  "Node.setPositionY."
  [node y]
  (let [N (gensym)]
    `(czlab.mcfud.afx.core/if-number [~N ~y]
                                     (oops.core/ocall! ~node "setPositionY" ~N))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-x
  "Get x from object."
  [obj] `(oops.core/oget ~obj "?x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-y
  "Get y from object."
  [obj] `(oops.core/oget ~obj "?y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-id
  "Get id from object."
  [obj] `(oops.core/oget ~obj "?id"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-piccy
  "Get property piccy from object."
  [obj] `(oops.core/oget ~obj "?piccy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-height
  "Get height from object."
  [obj] `(oops.core/oget ~obj "?height"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-width
  "Get width from object."
  [obj] `(oops.core/oget ~obj "?width"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro scene*
  "New Scene."
  [& args] `(new ~'js/cc.Scene ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro layer*
  "New Layer."
  [& args] `(new ~'js/cc.Layer ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro clayer*
  "New Color-Layer."
  [r b g & [a]]
  `(new ~'js/cc.LayerColor (js/cc.color ~r ~b ~g ~a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro milabel*
  "New menu-item label."
  [& args]
  `(new ~'js/cc.MenuItemLabel ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bmf-text*
  "New bitmap-font label."
  [& args]
  `(new ~'js/cc.LabelBMFont ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ttf-text*
  "New TTF label."
  [& args]
  `(new ~'js/cc.LabelTTF ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite*
  "New Sprite object."
  [& args] `(new ~'js/cc.Sprite ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro menu*
  "New Menu object."
  [& args] `(new ~'js/cc.Menu ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro misprite*
  "Create a menu-item sprite."
  [nnn cb & [sss ddd ctx]]
  `(new ~'js/cc.MenuItemSprite
        (czlab.mcfud.cc.ccsx/sprite* ~nnn)
        (czlab.mcfud.cc.ccsx/sprite* (or ~sss ~nnn))
        (czlab.mcfud.cc.ccsx/sprite* (or ~ddd ~nnn)) ~cb ~ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mitoggle*
  "New menu-item toggle."
  [& args]
  `(new ~'js/cc.MenuItemToggle ~@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mifont*
  "New menu-item-font."
  [name]
  `(new ~'js/cc.MenuItemFont ~name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro add-sprite-frames*
  "Add sprite frames to the cache."
  [plist]
  `(js/cc.spriteFrameCache.addSpriteFrames ~plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro batch-node*
  "Create a sprite-batch-node."
  [img]
  `(new ~'js/cc.SpriteBatchNode (js/cc.textureCache.addImage ~img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-cached-sprite
  "Get the sprite from the frame cache using
  its id (e.g. #ship)."
  [frameid]
  `(js/cc.spriteFrameCache.getSpriteFrame ~frameid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro remove-all!
  "Node.removeAllChildren."
  [node]
  `(oops.core/ocall! ~node "removeAllChildren"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro remove!
  "Node.removeFromParent."
  [child]
  `(oops.core/ocall! ~child "removeFromParent"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro push-scene
  "Director.pushScene."
  [s] `(js/cc.director.pushScene ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run-scene
  "Director.runScene."
  [s] `(js/cc.director.runScene ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pop-scene
  "Director.popScene"
  [] `(js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pop->root
  "Director.popToRootScene."
  [] `(js/cc.director.popToRootScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


