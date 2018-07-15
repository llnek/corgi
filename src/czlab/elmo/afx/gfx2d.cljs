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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 n#]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [num?? invert abs* EPSILON]]
            [oops.core :refer [oget oset! oapply!+ ocall!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *pos-inf* js/Number.POSITIVE_INFINITY)
(def *neg-inf* js/Number.NEGATIVE_INFINITY)
;(def *coordinate-system* :right-handed)
;(def _cocos2dx? true)
(def _cocos2dx? false)
(def PI js/Math.PI)
(def TWO-PI (* 2 PI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *shapeNum* (atom 0))
(defn- nextShapeNum "" [] (swap! *shapeNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seqOps! "" [ctx & callArgs]
  (doseq [a callArgs
          :let [f (_1 a) r (rest a)]] (oapply!+ ctx (_1 a) (rest a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrap?? "" [i len] (mod (+ 1 i) len))

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
(defn v2-smult
  "" [v n] (let [{:keys [x y]} v] (vec2 (* n x) (* n y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sdiv
  "" [v n] (let [{:keys [x y]} v] (vec2 (/ x n) (/ y n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sadd
  "" [v n] (let [{:keys [x y]} v] (vec2 (+ x n) (+ y n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-ssub
  "" [v n] (let [{:keys [x y]} v] (vec2 (- x n) (- y n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-add "" [v1 v2] (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sub "" [v1 v2] (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-scale "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-neg "" [v] (v2-scale v -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dot "" [v1 v2] (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-xss "" [v1 v2] (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sxss
  "" [a v] (let [{:keys [x y]} v] (vec2 (* (- a) y) (* a x))))

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
(defn v2-norm "" [v]
  (let [z (v2-len v)
        {:keys [x y]} v]
    (if (> z EPSILON) (vec2 (/ x z) (/ y z)) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-min "" [v1 v2]
  (let [{x1 :x y1 :y} v1
        {x2 :x y2 :y} v2] (vec2 (min x1 x2) (min y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-max "" [v1 v2]
  (let [{x1 :x y1 :y} v1
        {x2 :x y2 :y} v2] (vec2 (max x1 x2) (max y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dist
  "" [v1 v2] (pythag (- (:x v2) (:x v1))
                     (- (:y v2) (:y v1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-distsq
  "" [v1 v2] (let [c (v2-sub v1 v2)] (v2-dot c c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat2 "" [& [c00 c01 c10 c11]]
  {:m00 (num?? c00 0) :m01 (num?? c01 0) :m10 (num?? c10 0) :m11 (num?? c11 0)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat2* "" [radians]
  (let [c (js/Math.cos radians)
        s (js/Math.sin radians)] (mat2 c (- s) s c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-abs "" [m]
  (let [{:keys [m00 m01 m10 m11]} m]
    (mat2 (abs* m00) (abs* m01) (abs* m10) (abs* m11))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-axisX "" [m]
  (let [{:keys [m00 m01 m10 m11]} m] (vec2 m00 m10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-axisY "" [m]
  (let [{:keys [m00 m01 m10 m11]} m] (vec2 m01 m11)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-xpose "" [m]
  (let [{:keys [m00 m01 m10 m11]} m] (mat2 m00 m10 m01 m11)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-vmult "" [m v2]
  (let [{:keys [x y]} v2
        {:keys [m00 m01 m10 m11]} m]
    (vec2 (+ (* m00 x) (* m01 y))
          (+ (* m10 x) (* m11 y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-mmult "" [m1 m2]
  (let [{a00 :m00 a01 :m01 a10 :m10 a11 :m11} m1
        {b00 :m00 b01 :m01 b10 :m10 b11 :m11} m2]
    (mat2 (+ (* a00 b00) (* a01 b10))
          (+ (* a00 b01) (* a01 b11))
          (+ (* a10 b00) (* a11 b10))
          (+ (* a10 b01)  (* a11 b11)))))

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
  "" [] {:font "14px 'Arial'" :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawShape
  "" [s ctx & more] (apply (:draw @s) (concat [s ctx] more)) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyArea "" [s]
  (let [{:keys [vertices]} @s
        sz (n# vertices)
        sum (loop [i 0 area 0]
              (if (>= i sz)
                (abs* area)
                (let [;;modulo to get 0 if i is last vertex
                      vn (nth vertices (wrap?? i sz))
                      vi (nth vertices i)
                      {xi :x yi :y} vi
                      {xn :x yn :y} vn]
                  (recur (+ 1 i)
                         (+ area (- (* xi yn) (* xn yi)))))))] (/ sum 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcPolyCenter "" [s]
  (let [A (* 6 (polyArea s))
        {:keys [vertices]} @s
        sz (n# vertices)
        [cx cy]
        (loop [i 0 cx 0 cy 0]
          (if (>= i sz)
            [cx cy]
            (let [;;modulo to get 0 if i is last vertex
                  vn (nth vertices (wrap?? i sz))
                  vi (nth vertices i)
                  {xi :x yi :y} vi
                  {xn :x yn :y} vn]
              (recur (+ 1 i)
                     (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
                     (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi))))))))]

    (Point2D (/ cx A) (/ cy A))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfgStyle! "" [ctx styleObj]
  (when-some [line (:line styleObj)]
    (if-some [c (:cap line)] (oset! ctx "!lineCap" c))
    (if-some [w (:width line)] (oset! ctx "!lineWidth" w)))
  (when-some [k (:stroke styleObj)]
    (if-some [s (:style k)] (oset! ctx "!strokeStyle" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx & [styleObj]]
  (let [{:keys [vertices]} @p
        sz (n# vertices)
        {x0 :x y0 :y}
        (nth vertices 0)]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (cfgStyle! ctx styleObj))
    (ocall! ctx "moveTo" x0 y0)
    (dotimes [i sz]
      (when-not (zero? i)
        (let [{:keys [x y]}
              (nth vertices i)]
          (ocall! ctx "lineTo" x y))))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [&[pt vertices]]
  (atom {:pos (or pt V2_ZERO)
         :type :polygon :draw polyDraw  :vertices (or vertices [])}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" [v1 v2] (atom {:v1 v1 :v2 v2}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [c1 ctx]
  (let [{:keys [pos radius startPt]} @c1
        {cx :x cy :y} pos
        {sx :x sy :y} startPt]
    (seqOps! ctx
             ["beginPath"]
             ["arc" cx cy radius 0 TWO-PI true])
    (when (number? sx)
      (seqOps! ctx ["moveTo" cx cy] ["lineTo" sx sy]))
    (seqOps! ctx ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius]
  (let [s (Polygon pt)]
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
        s (Polygon pt [v0 v1 v2 v3])]
    (swap! s #(assoc %
                     :type :rectangle
                     :draw polyDraw
                     :angle 0
                     :width width
                     :height height)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lineDraw "" [line ctx]
  (let [{:keys [v1 v2]} @line
        {ax :x ay :y} v1
        {ex :x ey :y} v2]
    (ocall! ctx "beginPath")
    (ocall! ctx "moveTo" ax ay)
    (ocall! ctx "lineTo" ex ey)
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line "" [ptA ptB]
  (atom {:v1 ptA :v2 ptB :draw lineDraw :type :line}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


