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

  (:require [oops.core :refer [oget oset! oapply!+ ocall!]]
            [czlab.mcfud.afx.math :as m :refer [TWO-PI vec2]]
            [czlab.mcfud.afx.core :as c :refer [do->nil atom? cc+ _1 n# num??]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? false)
(def ^:private _cocos2dx? true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn canvas-batch!
  "Apply a sequence of operations to the html5 canvas,
  with each op being [method arg1 arg2 ...]"
  [ctx & callArgs]
  (doseq [a callArgs
          :let [[x & xs] a]] (oapply!+ ctx x xs)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addpt??
  "Add a point to the object."
  [obj & [pt]]
  (if (array? pt) (assoc obj :pos pt) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Area
  "Size of a rectangle."
  [width height]
  {:width width :height height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rect
  "Defines a rectangle, origin is left+bottom, and area."
  [origin width height]
  {:pre [(array? origin)]}
  (let [[x y] origin] {:x x :y y :width width :height height}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn text-style
  "Html5 Text Style object."
  []
  {:font "14px 'Arial'"
   :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-shape
  "Draw the shape onto the html5 canvas."
  [s canvas & args]
  (let [{:keys [draw]}
        (cond
          (atom? s) @s
          (map? s) s :else nil)] (apply draw (cc+ [s canvas] args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- poly-area
  "Calculate the area of this polygon."
  [{:keys [vertices] :as P}]
  (loop [i 0
         SZ (n# vertices) area 0]
    (if (>= i SZ)
      (* .5 (c/abs* area))
      (let [i2 (m/wrap?? i SZ)
            [xn yn] (nth vertices i2)
            [xi yi] (nth vertices i)]
        (recur (+ 1 i)
               SZ
               (+ area (- (* xi yn) (* xn yi))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-poly-center
  "Find the center point of this polygon."
  [{:keys [vertices] :as P}]
  (loop [A (* 6 (poly-area P))
         i 0
         SZ (n# vertices) cx 0 cy 0]
    (if (>= i SZ)
      (vec2 (/ cx A) (/ cy A))
      (let [i2 (m/wrap?? i SZ)
            [xn yn] (nth vertices i2)
            [xi yi] (nth vertices i)]
        (recur A
               (+ 1 i)
               SZ
               (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
               (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfg-style!
  "Apply styles to the canvas."
  [canvas styleObj]
  (do->nil
    (when-some [line (:line styleObj)]
      (if-some [c (:cap line)] (oset! canvas "!lineCap" c))
      (if-some [w (:width line)] (oset! canvas "!lineWidth" w)))
    (when-some [k (:stroke styleObj)]
      (if-some [s (:style k)] (oset! canvas "!strokeStyle" s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn poly-draw*
  "Draw and connect this set of points onto the canvas."
  [vs canvas]
  {:pre [(or (list? vs)(vector? vs))]}
  (do->nil
    (ocall! canvas "beginPath")
    (loop [i 0
           SZ (n# vs)]
      (when (< i SZ)
        (let [i2 (m/wrap?? i SZ)
              [x1 y1] (nth vs i)
              [x2 y2] (nth vs i2)]
          (c/jsto canvas
                  ["moveTo" x1 y1]
                  ["lineTo" x2 y2])
          (recur (+ 1 i) SZ))))
    (ocall! canvas "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- poly-draw
  "Draw this polygon."
  [p canvas]
  (poly-draw* (:vertices p) canvas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "Create a polygonal shape."
  [vertices & [center]]
  (addpt?? {:type :polygon
            :draw poly-draw
            :vertices (or vertices [])} center))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge
  "Define an edge of a shape."
  [v1 v2]
  {:v1 v1 :v2 v2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle-draw*
  "Draw a circle onto the canvas.  If a starting point
  is provided, draw a line to the center."
  [center radius angle canvas & [startPt?]]
  (let [[cx cy] center
        angle' (num?? angle 0)]
    (c/jsto canvas
            ["beginPath"]
            ["arc" cx cy radius 0 TWO-PI true])
    (when startPt?
      (let [sp (vec2 (+ cx radius) cy)
            [x y] (m/vec-rot sp angle' center)]
        (c/jsto canvas ["moveTo" cx cy] ["lineTo" x y])))
    (c/jsto canvas ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-draw
  "Draw a circle."
  [{:keys [radius pos] :as C} canvas & [center
                                        angle
                                        startPt?]]
  (-> (or pos center (m/vec-zero 2))
      (circle-draw* radius angle canvas startPt?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle
  "Create a circle shape."
  [radius & [center]]
  (addpt?? {:draw circle-draw
            :type :circle :radius radius} center))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect-draw
  "Draw a reactgle, not used."
  [{:keys [vertices width height] :as R} canvas & [angle]]
  (let [[x y] (nth vertices 0)]
    (c/jsto canvas
            ["save"]
            ["translate" x y]
            ["rotate" (num?? angle 0)]
            ["strokeRect" 0 0 width height] ["restore"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rot-rect-vertices
  "Rotate a set of points."
  [vs pivot angle]
  (let [[v0 v1 v2 v3] vs]
    (vector (m/vec-rot v0 angle pivot) (m/vec-rot v1 angle pivot)
            (m/vec-rot v2 angle pivot) (m/vec-rot v3 angle pivot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-rect-vertices
  "Find the vertices of a rectangle."
  [[x y :as center] width height]
  (let [hh (* .5 height)
        hw (* .5 width)
        base (if _cocos2dx? (- y hh) (+ y hh))
        top (if _cocos2dx? (+ y hh) (- y hh))
        rhs (+ x hw)
        lhs (- x hw)]
    (vector (vec2 lhs top) (vec2 rhs top)
            (vec2 rhs base) (vec2 lhs base))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle
  "Create a rectangle shape."
  [{:keys [width height] :as area} & [p0]]
  (let [p0 (or p0 (m/vec-zero 2))]
    (-> (calc-rect-vertices p0 width height)
        (Polygon p0)
        (assoc :type :rectangle :width width :height height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- line-draw
  "Draw a line onto canvas."
  [line canvas]
  (let [{:keys [v1 v2]} line
        [ax ay] v1
        [ex ey] v2]
    (c/jsto canvas
            ["beginPath"]
            ["moveTo" ax ay] ["lineTo" ex ey] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line
  "Create a line shape."
  [ptA ptB]
  {:v1 ptA :v2 ptB :draw line-draw :type :line})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

