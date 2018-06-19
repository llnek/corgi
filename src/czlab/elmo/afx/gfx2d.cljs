;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.gfx2d

  (:require-macros [czlab.elmo.afx.core :as ec])
  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [oops.core :refer [oget oset! ocall oapply ocall!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? true)
(def _cocos2dx? false)
(def PI js/Math.PI)
(def TWO-PI (* 2 PI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def VEC_ZERO (vec2 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pythagSQ "" [x y] (+ (* x x) (* y y)))
(defn pythag "" [x y] (js/Math.sqrt (pythagSQ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-len "" [v] (pythag (:x v) (:y v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-add "" [v1 v2] (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sub "" [v1 v2] (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-scale "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-negate "" [v] (v2-scale v -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dot "" [v1 v2] (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-cross "" [v1 v2] (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-rot "rotate counter-clockwise" [v1 center angleRad]
  (let [angle (if _cocos2dx? (- angleRad) angleRad)
        {cx :x cy :y} center
        cos (js/Math.cos angle)
        sin (js/Math.sin angle)
        x (- (:x v1) cx) y (- (:y v1) cy)]
    (vec2 (+ cx (- (* x cos) (* y sin)))
          (+ cy (+ (* x sin) (* y cos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-norm "" [v1]
  (let [len (invert (v2-len v1))]
    (vec2 (* len (:x v1)) (* len (:y v1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dist "" [v1 v2]
  (pythag (- (:x v1) (:x v2))
          (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Size2D "" [width height] {:width width :height height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Point2D "" [x y] (vec2 x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Area2D "" [pt sz] (merge pt sz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cfgStyle! "" [ctx styleObj]
  (oset! ctx "!lineWidth" (oget styleObj "?line" "?width"))
  (oset! ctx "!strokeStyle" (oget styleObj "?stroke" "?style")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Shape "" [pt] (atom {:pos pt}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [c1 ctx & [styleObj]]
  (let [{:keys [pos radius startPt]} @c1
        {:keys [x y]} pos]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx "arc" x y radius 0 TWO-PI true)
    (when (some? startPt)
      (ocall! ctx
              "moveTo"
              (:x startPt)
              (:y startPt))
      (ocall! ctx "lineTo" x y))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius]
  (let [s (Shape pt)]
    (swap! s #(assoc %
                     :draw circleDraw
                     :type :circle
                     :radius radius)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "" [r1 ctx & [styleObj]]
  (let [{:keys [vertices width height angle]} @r1
        {:keys [x y]} (nth vertices 0)]
    (ocall! ctx "save")
    (ocall! ctx "translate" x y)
    (ocall! ctx "rotate" angle)
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx "strokeRect" 0 0 width height)
    (ocall! ctx "restore")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz]
  (let [s (Shape pt)
        {:keys [width height]} sz]
    (swap! s #(assoc %
                     :type :rectangle
                     :draw rectDraw
                     :angle 0
                     :width_2 (/ width 2)
                     :height_2 (/ height 2)
                     :width width
                     :height height)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lineDraw "" [line ctx & [styleObj]]
  (let [{:keys [pos endPt]} @line]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj)
      (if-some [x (oget styleObj
                        "?line" "?cap")] (oset! ctx "!lineCap" x)))
    (ocall! ctx "moveTo" (:x pos) (:y pos))
    (ocall! ctx "lineTo" (:x endPt) (:y endPt))
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line "" [ptA ptB]
  (let [s (Shape ptA)]
    (swap! s #(assoc %
                     :draw lineDraw
                     :type :line
                     :endPt ptB)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn textStyle
  "" [] {:font "14px 'Arial'" :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawShape
  "" [s ctx & more] (apply (:draw @s) (concat [s ctx] more)) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


