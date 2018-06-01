;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.ccsx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite? "" [obj] `(instance? ~'js/cc.Sprite ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ccnode? "" [obj] `(instance? ~'js/cc.Node ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ccmenu? "" [obj] `(instance? ~'js/cc.Menu ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro snode? "" [obj] `(goog.object/containsKey ~obj "piccy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bbox? "" [obj] `(contains? ~obj :width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro bbox4? "" [obj] `(contains? ~obj :bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zerort "" [] `{:x 0 :y 0 :width 0 :height 0})
(defmacro zeropt "" [] `{:x 0 :y 0})
(defmacro zerosz "" [] `{:width 0 :height 0})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro attr* "" [node attrs] `(oops.core/ocall! ~node "attr" ~attrs))
(defmacro pos* "" [node] `(oops.core/ocall ~node "getPosition"))
(defmacro gcbyn "" [p n] `(oops.core/ocall ~p "getChildByName" ~n))
(defmacro gcbyt "" [p t] `(oops.core/ocall ~p "getChildByTag" ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pos! "" [node x y] `(oops.core/ocall! ~node "setPosition" ~x ~y))
(defmacro posX! "" [node x] `(oops.core/ocall! ~node "setPositionX" ~x))
(defmacro posY! "" [node y] `(oops.core/ocall! ~node "setPositionY" ~y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-x "" [obj] `(oops.core/oget ~obj "x"))
(defmacro oget-y "" [obj] `(oops.core/oget ~obj "y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-bottom "" [obj] `(oops.core/oget ~obj "bottom"))
(defmacro oget-right "" [obj] `(oops.core/oget ~obj "right"))
(defmacro oget-top "" [obj] `(oops.core/oget ~obj "top"))
(defmacro oget-left "" [obj] `(oops.core/oget ~obj "left"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-id "" [obj] `(oops.core/oget ~obj "?id"))
(defmacro oget-piccy "" [obj] `(oops.core/oget ~obj "?piccy"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro oget-height "" [obj] `(oops.core/oget ~obj "height"))
(defmacro oget-width "" [obj] `(oops.core/oget ~obj "width"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro not-native? "" [] `(not ~'js/cc.sys.isNative))
(defmacro native? "" [] `~'js/cc.sys.isNative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro sprite* "" [arg] `(new ~'js/cc.Sprite ~arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro newBBox4
  "" [t r b l] `{:top ~t :right ~r :bottom ~b :left ~l})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro newBBox
  "" [x y w h] `{:x ~x :y ~y :width ~w :height ~h})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


