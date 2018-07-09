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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [n#]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert abs*]]
            [oops.core :refer [oget oset! ocall oapply ocall!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *pos-inf* js/Number.POSITIVE_INFINITY)
(def *neg-inf* js/Number.NEGATIVE_INFINITY)
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? true)
(def _cocos2dx? false)
(def PI js/Math.PI)
(def TWO-PI (* 2 PI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def V2_ZERO (vec2 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pythagSQ "" [x y] (+ (* x x) (* y y)))
(defn pythag "" [x y] (js/Math.sqrt (pythagSQ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-lensq "" [v] (pythagSQ (:x v) (:y v)))

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
(defn v2-xss "" [v1 v2] (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2))))

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
(defn- polyArea "" [s]
  (let [{:keys [edges]} @s
        sz (n# edges)
        sum (loop [i 0 area 0]
              (if (>= i sz)
                (abs* area)
                (let [;;modulo to get 0 if i is last vertex
                      {vn :v1} @(nth edges (mod (+ 1 i) sz))
                      {vi :v1} @(nth edges i)
                      {xi :x yi :y} vi
                      {xn :x yn :y} vn]
                  (recur (+ 1 i)
                         (+ area (- (* xi yn) (* xn yi)))))))] (/ sum 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcPolyCenter "" [s]
  (let [A (* 6 (polyArea s))
        {:keys [edges]} @s
        sz (n# edges)
        [cx cy]
        (loop [i 0 cx 0 cy 0]
          (if (>= i sz)
            [cx cy]
            (let [;;modulo to get 0 if i is last vertex
                  {vn :v1} @(nth edges (mod (+ 1 i) sz))
                  {vi :v1} @(nth edges i)
                  {xi :x yi :y} vi
                  {xn :x yn :y} vn]
              (recur (+ 1 i)
                     (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
                     (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi))))))))]

    (Point2D (/ cx A) (/ cy A))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfgStyle! "" [ctx styleObj]
  (if-some [x (get-in styleObj [:line :width])] (oset! ctx "!lineWidth" x))
  (if-some [x (get-in styleObj [:stroke :style])] (oset! ctx "!strokeStyle" x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx & [styleObj]]
  (let [{:keys [edges]} @p
        sz (n# edges)
        {x0 :x y0 :y}
        (:v1 @(nth edges 0))]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx "moveTo" x0 y0)
    (dotimes [i sz]
      (when-not (zero? i)
        (let [{:keys [x y]}
              (:v1 @(nth edges i))]
          (ocall! ctx "lineTo" x y))))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [&[pt edges]]
  (atom {:pos (or pt V2_ZERO)
         :type :polygon :draw polyDraw  :edges (or edges [])}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" [v1 v2] (atom {:v1 v1 :v2 v2}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [c1 ctx & [styleObj]]
  (let [{:keys [pos radius startPt]} @c1
        {cx :x cy :y} pos
        {sx :x sy :y} startPt]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx
            "arc" cx cy radius 0 TWO-PI true)
    (when (number? sx)
      (ocall! ctx "moveTo" sx sy)
      (ocall! ctx "lineTo" cx cy))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius]
  (let [s (Polygon pt)]
    (swap! s #(assoc (dissoc % :edges)
                     :draw circleDraw
                     :type :circle
                     :radius radius)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "" [r1 ctx & [styleObj]]
  (let [{:keys [edges width height angle]} @r1
        {:keys [x y]} (:v1 @(nth edges 0))]
    (ocall! ctx "save")
    (ocall! ctx "translate" x y)
    (ocall! ctx "rotate" angle)
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx "strokeRect" 0 0 width height)
    (ocall! ctx "restore")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz]
  (let [{:keys [width height]} sz
        {:keys [x y]} pt
        height_2 (/ height 2)
        width_2 (/ width 2)
        bottom (if _cocos2dx? (- y height_2) (+ y height_2))
        top (if _cocos2dx? (+ y height_2) (- y height_2))
        right (+ x width_2)
        left (- x width_2)
        [v0 v1 v2 v3]
        [(Point2D left top) (Point2D right top)
         (Point2D right bottom) (Point2D left bottom)]
        s (Polygon pt
                   [(Edge v0 v1)
                    (Edge v1 v2)
                    (Edge v2 v3)(Edge v3 v0)])]
    (swap! s #(assoc %
                     :type :rectangle
                     :draw polyDraw
                     :angle 0
                     :width width
                     :height height)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lineDraw "" [line ctx & [styleObj]]
  (let [{:keys [v1 v2]} @line
        {ax :x ay :y} v1
        {ex :x ey :y} v2]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj)
      (if-some [x (oget styleObj
                        "?line" "?cap")] (oset! ctx "!lineCap" x)))
    (ocall! ctx "moveTo" ax ay)
    (ocall! ctx "lineTo" ex ey)
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line "" [ptA ptB]
  (atom {:v1 ptA :v2 ptB :draw lineDraw :type :line}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn textStyle
  "" [] {:font "14px 'Arial'" :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawShape
  "" [s ctx & more] (apply (:draw @s) (concat [s ctx] more)) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


