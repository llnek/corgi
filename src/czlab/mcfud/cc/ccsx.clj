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
(defmacro not-native?

  "True if not native."
  []

  `(not ~'js/cc.sys.isNative))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro native?

  "True if native."
  []

  `~'js/cc.sys.isNative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gebyid

  "Calls document.getElementById."
  [id]

  `(if (czlab.mcfud.cc.ccsx/native?) nil (js/document.getElementById ~id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite?

  "True if object is a Sprite."
  [obj]

  `(instance? ~'js/cc.Sprite ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ccnode?

  "True if object is a Node."
  [obj]

  `(instance? ~'js/cc.Node ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ccmenu?

  "True if object is a Menu."
  [obj]

  `(instance? ~'js/cc.Menu ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro attr*

  "Call node.attr method."
  [node attrs]

  `(oops.core/ocall! ~node "attr" ~attrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro visible!

  "Call node.setVisible."
  [node v?]

  `(oops.core/ocall! ~node "setVisible" ~v?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro anchor!

  "Call node.setAnchorPoint."
  [node p]

  (let [x (gensym)]
    `(if-some [~x ~p]
       (oops.core/ocall! ~node
                         "setAnchorPoint"
                         (czlab.mcfud.cc.ccsx/anchorValue ~x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro color!

  "Call node.setColor."
  [node c]

  (let [x (gensym)]
    `(if-some [~x ~c]
       (oops.core/ocall! ~node "setColor" ~x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro scale!

  "Call node.setScale."
  [node n]

  (let [x (gensym)]
    `(czlab.mcfud.afx.core/if-number [~x ~n]
                                     (oops.core/ocall! ~node "setScale" ~x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pos*

  "Call node.getPosition."
  [node]

  `(oops.core/ocall ~node "getPosition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gcbyn

  "Call node.getChildByName."
  [parent n]

  `(oops.core/ocall ~parent "getChildByName" ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro gcbyt

  "Call node.getChildByTag."
  [parent t]

  `(oops.core/ocall ~parent "getChildByTag" ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pos!

  "Call node.setPosition."
  [node p]

  (let [x (gensym)]
    `(if-some [~x ~p]
       (oops.core/ocall! ~node "setPosition" (czlab.mcfud.cc.ccsx/vec2->cp ~x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro posX!

  "Call node.setPositionX."
  [node x]

  (let [n (gensym)]
    `(czlab.mcfud.afx.core/if-number [~n ~x]
                                     (oops.core/ocall! ~node "setPositionX" ~n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro posY!

  "Call node.setPositionY."
  [node y]

  (let [n (gensym)]
    `(czlab.mcfud.afx.core/if-number [~n ~y]
                                     (oops.core/ocall! ~node "setPositionY" ~n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-x

  "Get property x from object."
  [obj]

  `(oops.core/oget ~obj "?x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-y

  "Get property y from object."
  [obj]

  `(oops.core/oget ~obj "?y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-id

  "Get property id from object."
  [obj]

  `(oops.core/oget ~obj "?id"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-piccy

  "Get property piccy from object."
  [obj]

  `(oops.core/oget ~obj "?piccy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-height

  "Get property height from object."
  [obj]

  `(oops.core/oget ~obj "?height"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-width

  "Get property width from object."
  [obj]

  `(oops.core/oget ~obj "?width"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite*

  "Create a new Sprite object."
  [arg]

  `(new ~'js/cc.Sprite ~arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro new-rect

  "Create a new Rect object."
  [x y w h]

  `{:x ~x :y ~y :wide ~w :tall ~h})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro new-size

  "Create a new Area object."
  [x y w h]

  `{:x ~x :y ~y :wide ~w :tall ~h})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zero-rect

  "Create a new Rect object with zeroes."
  []

  `{:x 0 :y 0 :wide 0 :tall 0})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pushScene

  "Call director.pushScene."
  [s]

  `(js/cc.director.pushScene ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro run*

  "Call director.runScene."
  [s]

  `(js/cc.director.runScene ~s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro popScene

  "Call director.popScene"
  []

  `(js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro popToRoot

  "Call director.popToRootScene."
  []

  `(js/cc.director.popToRootScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


