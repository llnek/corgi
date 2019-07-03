;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.geo

  (:require [czlab.mcfud.afx.math :as m :refer [vec2]]
            [czlab.mcfud.afx.core :as c :refer [_1 _2 n#]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:dynamic *coordinate-system* :right-handed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Rect [x y width height])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Area [width height])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn area "" [width height] (new Area width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect
  ""
  ([x y width height] (new Rect x y width height))
  ([origin area]
   (new Rect (_1 origin) (_2 origin) (:width area) (:height area))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- poly-area
  "Calculate the area of this polygon."
  [{:keys [vertices] :as P}]
  (loop [i 0
         SZ (n# vertices) area 0]
    (if (>= i SZ)
      (* .5 (c/abs* area))
      (let [i2 (m/wrap?? i SZ)
            [xi yi] (nth vertices i)
            [xn yn] (nth vertices i2)]
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
            [xi yi] (nth vertices i)
            [xn yn] (nth vertices i2)]
        (recur A
               (+ 1 i)
               SZ
               (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
               (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Polygon [vertices type])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Line [v1 v2])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Circle [radius])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygon "" [vertices] (new Polygon vertices :polygon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle "" [radius] (assoc (new Circle radius) :type :circle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shift-vertices
  "Shift a set of points."
  [vs delta]
  (mapv #(m/vec-add % delta) vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rot-vertices
  "Rotate a set of points."
  [vs pivot angle]
  (mapv #(m/vec-rot % angle pivot) vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-rect-vertices
  "Find the vertices of a rectangle."
  ([center area]
   (calc-rect-vertices (_1 center)
                       (_2 center)
                       (:width area) (:height area)))
  ([x y width height]
   (let [hh (* .5 height)
         hw (* .5 width)
         base (- y hh)
         top (+ y hh)
         rhs (+ x hw)
         lhs (- x hw)]
     (vector (vec2 lhs top) (vec2 rhs top)
             (vec2 rhs base) (vec2 lhs base)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectangle
  ""
  ([area] (rectangle (:width area) (:height area)))
  ([width height]
   (let []
     (-> (calc-rect-vertices 0 0 width height)
         (polygon)
         (assoc :type :rectangle
                :width width :height height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn line "" [ptA ptB] (new Line ptA ptB))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-equals-rect?
  "If rects are equal?"
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (and (== x1 x2) (== y1 y2) (== w1 w2) (== t1 t2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-contains-rect?
  "If R contains r?"
  [{x1 :x y1 :y w1 :width t1 :height :as R}
   {x2 :x y2 :y w2 :width t2 :height :as r}]
  (not (or (>= x1 x2)
           (>= y1 y2)
           (<= (+ x1 w1) (+ x2 w2))
           (<= (+ y1 t1) (+ y2 t2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-maxX
  "Right side of rect."
  [{:keys [x width]}] (+ x width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-midX
  "Mid point of rect on the x-axis."
  [{:keys [x width]}] (+ x (* .5 width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-minX "Get left side of rect." [r] (:x r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-maxY
  "Top of the rect."
  [{:keys [y height]}] (+ y height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-midY
  "Mid point of rect on the y-axis."
  [{:keys [y height]}] (+ y (* .5 height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-minY "Bottom of rect." [r] (:y r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn contains-pt?
  "If point lies inside rect."
  [rect [px py :as P]]
  (and (>= px (rect-get-minX rect))
       (<= px (rect-get-maxX rect))
       (>= py (rect-get-minY rect))
       (<= py (rect-get-maxY rect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-intersects-rect?
  "If two rects intersect?"
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (not (or (< (+ x1 w1) x2)
           (< (+ x2 w2) x1)
           (< (+ y1 t1) y2)
           (< (+ y2 t2) y1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-unions-rect
  "Find the union of two rects."
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (let [x (min x1 x2)
        y (min y1 y2)]
    (rect x y
          (- (max (+ x1 w1) (+ x2 w2)) x)
          (- (max (+ y1 t1) (+ y2 t2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-intersects-rect
  "Find the intersection of two rects."
  [{x1 :x y1 :y w1 :width t1 :height :as rect1}
   {x2 :x y2 :y w2 :width t2 :height :as rect2}]
  (let [x (max x1 x2)
        y (max y1 y2)]
    (rect x y
          (- (min (rect-get-maxX rect1) (rect-get-maxX rect2)) x)
          (- (min (rect-get-maxY rect1) (rect-get-maxY rect2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn out-of-bound?
  "If entity is outside of B?"
  [r B] (not (rect-contains-rect? B r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect? "If object is a rect?" [obj] (instance? Rect obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

