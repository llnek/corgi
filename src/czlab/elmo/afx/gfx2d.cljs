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

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [_1 n# assoc!!]])

  (:require [oops.core :refer [oget oset! oapply!+ ocall!]]
            [czlab.elmo.afx.core
             :as ec :refer [num?? invert sqrt* abs* EPSILON]]
            [czlab.elmo.afx.math
             :as ma :refer [TWO-PI wrap?? vec2 vec-rot vec-zero]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? true)
(def _cocos2dx? false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn canvasBatchOps! "" [ctx & callArgs]
  (doseq [a callArgs] (oapply!+ ctx (_1 a) (rest a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toVec2 "" [& [x y]]
  (if (some? x)
    (if (number? y) (vec2 x y) x) (vec-zero 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toPoint2D "" [& [x y]] (toVec2 x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addPt?? [obj & [x y]]
  (let [p (if (some? x)
            (toPoint2D x y))]
    (if (some? p) (assoc obj :pos p) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Size2D "" [width height] {:width width :height height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Point2D "" [x y] (vec2 x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Area2D "" [pt sz] (merge pt sz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn textStyle
  "" [] {:font "14px 'Arial'"
         :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawShape
  "" [s canvas & more]
  (apply (:draw (if (map? s) s @s)) (concat [s canvas] more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyArea "" [p]
  (let [{:keys [vertices]} p]
    (loop [i 0 SZ (n# vertices) area 0]
      (if (>= i SZ)
        (/ (abs* area) 2)
        (let [i2 (wrap?? i SZ)
              [xn yn] (nth vertices i2)
              [xi yi] (nth vertices i)]
          (recur (+ 1 i)
                 SZ
                 (+ area (- (* xi yn) (* xn yi)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcPolyCenter "" [p]
  (let [A (* 6 (polyArea p))
        {:keys [vertices]} p]
    (loop [i 0 SZ (n# vertices) cx 0 cy 0]
      (if (>= i SZ)
        (Point2D (/ cx A) (/ cy A))
        (let [i2 (wrap?? i SZ)
              [xn yn] (nth vertices i2)
              [xi yi] (nth vertices i)]
          (recur (+ 1 i)
                 SZ
                 (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
                 (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfgStyle! "" [canvas styleObj]
  (when-some [line (:line styleObj)]
    (if-some [c (:cap line)] (oset! canvas "!lineCap" c))
    (if-some [w (:width line)] (oset! canvas "!lineWidth" w)))
  (when-some [k (:stroke styleObj)]
    (if-some [s (:style k)] (oset! canvas "!strokeStyle" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polyDraw* "" [vs canvas]
  (ocall! canvas "beginPath")
  (loop [i 0 SZ (n# vs)]
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
(defn- polyDraw "" [p canvas] (polyDraw* (:vertices p) canvas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [vertices & [x y]]
  (addPt?? {:type :polygon
            :draw polyDraw
            :vertices (or vertices [])} x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" [v1 v2] {:v1 v1 :v2 v2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circleDraw* "" [center radius angle canvas & [startPt?]]
  (let [[cx cy] center
        angle' (num?? angle 0)]
    (canvasBatchOps! canvas
                     ["beginPath"]
                     ["arc" cx cy radius 0 TWO-PI true])
    (when startPt?
      (let [sp (Point2D (+ cx radius) cy)
            [x y] (vec-rot sp angle' center)]
        (canvasBatchOps! canvas ["moveTo" cx cy] ["lineTo" x y])))
    (canvasBatchOps! canvas ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [C canvas & [center angle startPt?]]
  (let [{:keys [radius pos]} C
        [x y :as c] (or pos center (vec-zero 2))]
    (circleDraw* c radius angle canvas startPt?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [radius & [x y]]
  (addPt?? {:draw circleDraw
            :type :circle :radius radius} x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "not used" [R canvas & [angle]]
  (let [{:keys [vertices width height]} R
        [x y] (nth vertices 0)]
    (canvasBatchOps! canvas
                     ["save"]
                     ["translate" x y]
                     ["rotate" (num?? angle 0)]
                     ["strokeRect" 0 0 width height] ["restore"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotRectVertices "" [vs pos angle]
  (let [[v0 v1 v2 v3] vs]
    [(vec-rot v0 angle pos) (vec-rot v1 angle pos)
     (vec-rot v2 angle pos) (vec-rot v3 angle pos)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcRectVertices "" [pos width height]
  (let [hh (/ height 2)
        hw (/ width 2)
        [x y] pos
        bottom (if _cocos2dx? (- y hh) (+ y hh))
        top (if _cocos2dx? (+ y hh) (- y hh))
        right (+ x hw)
        left (- x hw)]
    [(Point2D left top) (Point2D right top)
     (Point2D right bottom) (Point2D left bottom)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [sz & [x0 y0]]
  (let [{:keys [width height]} sz
        pos (toPoint2D x0 y0)
        vs (calcRectVertices pos width height)]
    (-> (Polygon vs x0 y0)
        (assoc :type :rectangle
               :width width :height height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lineDraw "" [line canvas]
  (let [{:keys [v1 v2]} line
        [ax ay] v1
        [ex ey] v2]
    (canvasBatchOps! canvas
                     ["beginPath"]
                     ["moveTo" ax ay] ["lineTo" ex ey] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line "" [ptA ptB]
  {:v1 ptA :v2 ptB :draw lineDraw :type :line})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


