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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 n# assoc!!]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [num?? invert sqrt* abs* EPSILON]]
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
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def V2_ZERO (vec2 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn batchOps! "" [ctx & callArgs]
  (doseq [a callArgs] (oapply!+ ctx (_1 a) (rest a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toVec2 "" [& [x y]]
  (if (some? x)
    (if (number? y) (vec2 x y) x) V2_ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toPoint2D "" [& [x y]] (toVec2 x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addPt?? [obj & [x y]]
  (let [p (if (some? x)
            (toPoint2D x y))]
    (if (some? p) (assoc obj :pos p) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrap?? "" [i len] (mod (+ 1 i) len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pythagSQ "" [x y] (+ (* x x) (* y y)))
(defn pythag "" [x y] (sqrt* (pythagSQ x y)))

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
  "" [] {:font "14px 'Arial'"
         :fill "#dddddd" :align "left" :base "top" })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drawShape
  "" [s ctx & more]
  (apply (:draw (if (map? s) s @s)) (concat [s ctx] more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyArea "" [p]
  (let [{:keys [vertices]} p]
    (loop [i 0 SZ (n# vertices) area 0]
      (if (>= i SZ)
        (/ (abs* area) 2)
        (let [i2 (wrap?? i SZ)
              {xn :x yn :y} (nth vertices i2)
              {xi :x yi :y} (nth vertices i)]
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
              {xn :x yn :y} (nth vertices i2)
              {xi :x yi :y} (nth vertices i)]
          (recur (+ 1 i)
                 SZ
                 (+ cx (* (+ xi xn) (- (* xi yn) (* xn yi))))
                 (+ cy (* (+ yi yn) (- (* xi yn) (* xn yi))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfgStyle! "" [ctx styleObj]
  (when-some [line (:line styleObj)]
    (if-some [c (:cap line)] (oset! ctx "!lineCap" c))
    (if-some [w (:width line)] (oset! ctx "!lineWidth" w)))
  (when-some [k (:stroke styleObj)]
    (if-some [s (:style k)] (oset! ctx "!strokeStyle" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx]
  (let [{:keys [vertices]} p]
    (ocall! ctx "beginPath")
    (loop [i 0 SZ (n# vertices)]
      (when (< i SZ)
        (let [i2 (wrap?? i SZ)
              {x1 :x y1 :y} (nth vertices i)
              {x2 :x y2 :y} (nth vertices i2)]
          (batchOps! ctx
                     ["moveTo" x1 y1]
                     ["lineTo" x2 y2])
          (recur (+ 1 i) SZ))))
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [vertices & [x y]]
  (addPt?? {:type :polygon
            :draw polyDraw
            :vertices (or vertices [])} x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" [v1 v2] {:v1 v1 :v2 v2})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [C ctx & [center angle startPt?]]
  (let [{:keys [radius pos]} C
        {cx :x cy :y :as c}
        (or pos center V2_ZERO)]
    (batchOps! ctx
               ["beginPath"]
               ["arc" cx cy radius 0 TWO-PI true])
    (when startPt?
      (let [sp (Point2D cx (if _cocos2dx?
                             (+ cy radius) (- cy radius)))
            {:keys [x y]} (v2-rot sp c angle)]
        (batchOps! ctx ["moveTo" cx cy] ["lineTo" x y])))
    (batchOps! ctx ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [radius & [x y]]
  (addPt?? {:draw circleDraw
            :type :circle :radius radius} x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "not used" [R ctx & [angle]]
  (let [{:keys [vertices width height]} R
        {:keys [x y]} (nth vertices 0)]
    (batchOps! ctx
               ["save"]
               ["translate" x y]
               ["rotate" (num?? angle 0)]
               ["strokeRect" 0 0 width height] ["restore"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotRectVertices "" [vs pos angle]
  (let [[v0 v1 v2 v3] vs]
    [(v2-rot v0 pos angle) (v2-rot v1 pos angle)
     (v2-rot v2 pos angle) (v2-rot v3 pos angle)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcRectVertices "" [pos width height]
  (let [hh (/ height 2)
        hw (/ width 2)
        {:keys [x y]} pos
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
(defn- lineDraw "" [line ctx]
  (let [{:keys [v1 v2]} line
        {ax :x ay :y} v1
        {ex :x ey :y} v2]
    (batchOps! ctx
               ["beginPath"]
               ["moveTo" ax ay] ["lineTo" ex ey] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line "" [ptA ptB]
  {:v1 ptA :v2 ptB :draw lineDraw :type :line})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


