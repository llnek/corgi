;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.gfx2d

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid defvoid- half* _1 n# assoc!!]])

  (:require [oops.core :refer [oget oset! oapply!+ ocall!]]
            [czlab.mcfud.afx.core
             :as ec :refer [num?? numFlip sqrt* abs*]]
            [czlab.mcfud.afx.math
             :as ma :refer [TWO-PI wrap?? vec2 vec-rot vec-zero]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? false)
(def ^:private _cocos2dx? true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid canvasBatchOps!

  "Apply a sequence of operations to the html5 canvas,
  with each op being [method arg1 arg2 ...]"
  [ctx & callArgs]

  (doseq [a callArgs] (oapply!+ ctx (_1 a) (rest a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addPt??

  "Add a point to the object."
  [obj & [pt]]

  (if (array? pt) (assoc obj :pos pt) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Area

  "Represents the size of an area."
  [width height]

  {:wide width :tall height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rect

  "Defines a rectangle, origin is left+bottom, and area."
  [origin width height]
  {:pre [(array? origin)]}

  (let [[x y] origin] {:x x :y y :wide width :tall height}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn textStyle

  "Html5 Text Style object."
  []

  {:font "14px 'Arial'"
   :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid drawShape

  "Draw the shape onto the html5 canvas."
  [s canvas & args]

  (let [{:keys [draw]}
        (if (map? s) s @s)] (apply draw (concat [s canvas] args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyArea

  "Calculate the area of this polygon."
  [{:keys [vertices] :as P}]

  (loop [i 0
         SZ (n# vertices) area 0]
    (if (>= i SZ)
      (half* (abs* area))
      (let [i2 (wrap?? i SZ)
            [xn yn] (nth vertices i2)
            [xi yi] (nth vertices i)]
        (recur (+ 1 i)
               SZ
               (+ area (- (* xi yn) (* xn yi))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcPolyCenter

  "Find the center point of this polygon."
  [{:keys [vertices] :as P}]

  (loop [A (* 6 (polyArea P))
         i 0
         SZ (n# vertices) cx 0 cy 0]
    (if (>= i SZ)
      (vec2 (/ cx A) (/ cy A))
      (let [i2 (wrap?? i SZ)
            [xn yn] (nth vertices i2)
            [xi yi] (nth vertices i)]
        (recur A
               (+ 1 i)
               SZ
               (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
               (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid cfgStyle!

  "Apply styles to the canvas."
  [canvas styleObj]

  (when-some [line (:line styleObj)]
    (if-some [c (:cap line)] (oset! canvas "!lineCap" c))
    (if-some [w (:width line)] (oset! canvas "!lineWidth" w)))
  (when-some [k (:stroke styleObj)]
    (if-some [s (:style k)] (oset! canvas "!strokeStyle" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid polyDraw*

  "Draw and connect this set of points onto the canvas."
  [vs canvas]
  {:pre [(or (list? vs)(vector? vs))]}

  (ocall! canvas "beginPath")
  (loop [i 0
         SZ (n# vs)]
    (when (< i SZ)
      (let [i2 (wrap?? i SZ)
            [x1 y1] (nth vs i)
            [x2 y2] (nth vs i2)]
        (canvasBatchOps! canvas
                         ["moveTo" x1 y1]
                         ["lineTo" x2 y2])
        (recur (+ 1 i) SZ))))
  (ocall! canvas "stroke"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- polyDraw

  "Draw this polygon."
  [p canvas]

  (polyDraw* (:vertices p) canvas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon

  "Create a polygonal shape."
  [vertices & [center]]

  (addPt?? {:type :polygon
            :draw polyDraw
            :vertices (or vertices [])} center))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge

  "Define an edge of a shape."
  [v1 v2]

  {:v1 v1 :v2 v2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid circleDraw*

  "Draw a circle onto the canvas.  If a starting point
  is provided, draw a line to the center."
  [center radius angle canvas & [startPt?]]

  (let [[cx cy] center
        angle' (num?? angle 0)]
    (canvasBatchOps! canvas
                     ["beginPath"]
                     ["arc" cx cy radius 0 TWO-PI true])
    (when startPt?
      (let [sp (vec2 (+ cx radius) cy)
            [x y] (vec-rot sp angle' center)]
        (canvasBatchOps! canvas
                         ["moveTo" cx cy] ["lineTo" x y])))
    (canvasBatchOps! canvas ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- circleDraw

  "Draw a circle."
  [{:keys [radius pos] :as C} canvas & [center
                                        angle
                                        startPt?]]

  (-> (or pos center (vec-zero 2))
      (circleDraw* radius angle canvas startPt?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle

  "Create a circle shape."
  [radius & [center]]

  (addPt?? {:draw circleDraw
            :type :circle :radius radius} center))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- rectDraw

  "Draw a reactgle, not used."
  [{:keys [vertices wide tall] :as R} canvas & [angle]]

  (let [[x y] (nth vertices 0)]
    (canvasBatchOps! canvas
                     ["save"]
                     ["translate" x y]
                     ["rotate" (num?? angle 0)]
                     ["strokeRect" 0 0 wide tall] ["restore"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotRectVertices

  "Rotate a set of points."
  [vs pivot angle]

  (let [[v0 v1 v2 v3] vs]
    (vector (vec-rot v0 angle pivot) (vec-rot v1 angle pivot)
            (vec-rot v2 angle pivot) (vec-rot v3 angle pivot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcRectVertices

  "Find the vertices of a rectangle."
  [[x y :as center] width height]

  (let [hh (half* height)
        hw (half* width)
        base (if _cocos2dx? (- y hh) (+ y hh))
        peak (if _cocos2dx? (+ y hh) (- y hh))
        rhs (+ x hw)
        lhs (- x hw)]
    (vector (vec2 lhs peak) (vec2 rhs peak)
            (vec2 rhs base) (vec2 lhs base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle

  "Create a rectangle shape."
  [{:keys [wide tall] :as area} & [p0]]

  (let [p0 (or p0 (vec-zero 2))]
    (-> (calcRectVertices p0 wide tall)
        (Polygon p0)
        (assoc :type :rectangle :wide wide :tall tall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- lineDraw

  "Draw a line onto canvas."
  [line canvas]

  (let [{:keys [v1 v2]} line
        [ax ay] v1
        [ex ey] v2]
    (canvasBatchOps! canvas
                     ["beginPath"]
                     ["moveTo" ax ay] ["lineTo" ex ey] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line

  "Create a line shape."
  [ptA ptB]

  {:v1 ptA :v2 ptB :draw lineDraw :type :line})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


