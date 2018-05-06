;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.canvas

  (:require-macros [czlab.elmo.afx.core :as ec])
  (:require [czlab.elmo.afx.core :as ec]
            [oops.core :refer [oget oset! ocall oapply ocall!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cfgStyle! "" [ctx styleObj]
  (oset! ctx "lineWidth" (oget styleObj "?line" "?width"))
  (oset! ctx "strokeStyle" (oget styleObj "?stroke" "?style")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle "" [x y radius] {:x x :y y :radius radius})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawCircle "" [circle ctx styleObj]
  (let [{:keys [x y radius]} circle]
    (ocall! ctx "beginPath")
    (cfgStyle! ctx styleObj)
    (ocall! ctx "arc"
            x y radius
            0 (* 2 js/Math.PI) true)
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn line "" [x1 y1 x2 y2] {:x1 x1 :y1 y1 :x2 x2 :y2 y2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawLine "" [line ctx styleObj]
  (let [{:keys [x1 x2 y1 y2]} line]
    (ocall! ctx "beginPath")
    (ocall! ctx "moveTo" x1 y1)
    (ocall! ctx "lineTo" x2 y2)
    (cfgStyle! ctx styleObj)
    (if-some [x (oget styleObj "?line" "?cap")] (oset! ctx "!lineCap" x))
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn point2d "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn area2d "" [x y width height]
  {:x x :y y :width width :height height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn textStyle "" []
  {:font "14px 'Arial'" :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


