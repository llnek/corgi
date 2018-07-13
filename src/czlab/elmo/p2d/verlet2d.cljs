;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.verlet2d

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 n# assoc!! nexth]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert abs* sqr* num??]]
            [czlab.elmo.p2d.physics2d
             :as py :refer [dynamic? rigidBody!]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [V2_ZERO *pos-inf* *neg-inf*
                            Point2D vec2
                            v2-len v2-scale v2-add v2-dist v2-sdiv
                            v2-rot v2-sub v2-dot v2-neg v2-norm]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *gWorld* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Vertex "" [body pos]
  (atom {:body body :pos pos :prev pos :accel V2_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Edge "" [body v1 v2]
  (atom {:v1 (Vertex body v1)
         :v2 (Vertex body v2)
         :body body :olen (v2-dist v1 v2)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCenter! "" [body]
  (let [{:keys [edges]} @body]
    (loop [i 0 SZ (n# edges)
           cx 0 cy 0
           xmin *pos-inf* ymin *pos-inf*
           xmax *neg-inf* ymax *neg-inf*]
      (if (>= i SZ)
        (do (assoc!! body
                     :pmin (vec2 xmin ymin)
                     :pmax (vec2 xmax ymax)
                     :pos (v2-sdiv (vec2 cx cy) SZ)) body)
        (let [{:keys [v1 v2]} @(nth edges i)
              {:keys [x y]} (:pos @v1)]
          (recur (+ 1 i)
                 SZ
                 (+ cx x) (+ cy y)
                 (min xmin x) (min ymin y)
                 (max xmax x) (max ymax y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx & [styleObj]]
  (let [{:keys [edges]} @p
        SZ (n# edges)
        end (dec SZ)
        v' (:v1 @(_1 edges))
        {x0 :x y0 :y} (:pos @v')]
    (ocall! ctx "beginPath")
    (gx/cfgStyle! ctx styleObj)
    (ocall! ctx "moveTo" x0 y0)
    (dotimes [i SZ]
      (when-not (zero? i)
        (let [v' (:v1 @(nth edges i))
              {:keys [x y]} (:pos @v')]
          (ocall! ctx "lineTo" x y))))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyRotate "" [s angle']
  (let [{center :pos :keys [angle edges]} @s
        vs (mapv (fn [e] (:v1 @e)) edges)
        SZ (n# vs)
        _ (doseq [v vs
                  :let [p (v2-rot (:pos @v) center angle')]]
            (assoc!! v :pos p :prev p))
        vs' (conj vs (_1 vs))]
    (dotimes [i SZ]
      (let [a (nth vs' i)
            b (nexth vs' i)
            e (nth edges i)]
        (assoc!! e :v1 a :v2 b :olen (v2-dist a b))))
    (assoc!! s :angle (+ angle angle'))
    (calcCenter! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [vs & [mass friction bounce]]
  (let [ret (py/Polygon)]
    (assoc!! ret
             :rotate polyRotate
             :edges
             (loop [i 0 END (dec (n# vs)) e' []]
               (if (= i END)
                 (conj e' (Edge ret (nth vs i) (nth vs 0)))
                 (recur (inc i)
                        END
                        (conj e' (Edge ret (nth vs i) (nexth vs i)))))))
    (-> (calcCenter! ret)
        (rigidBody! mass friction bounce))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[depth normal edge vertex]]
  (atom {:depth (num?? depth 0)
         :normal (or normal V2_ZERO)
         :edge (or edge nil)
         :vertex (or vertex nil)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVertex! "" [v t2]
  (let [{:keys [pos prev accel]} @v]
    (assoc!! v
             :prev pos
             :pos (v2-add pos
                          (v2-add (v2-sub pos prev)
                                  (v2-scale accel t2)))) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCenters! "" []
  (ec/eachStore (:samples @*gWorld*)
                (fn [b _]
                  (if (dynamic? b) (calcCenter! b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [s _]
        (if (dynamic? s)
          (doseq [e (:edges @s)
                  :let [{:keys [v1]} @e]]
            (updateVertex! v1 t2))))
      (ec/eachStore (:samples @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateEdges! "" []
  (->>
    (fn [s _]
      (if (dynamic? s)
        (doseq [e (:edges @s)
                :let [{:keys [olen v1 v2]} @e
                      {p1 :pos} @v1
                      {p2 :pos} @v2
                      v12 (v2-sub p2 p1)
                      diff (- (v2-len v12) olen)
                      N (-> (v2-norm v12)
                            (v2-scale (* diff 0.5)))]]
          ;;push apart by half of the difference
          (assoc!! v1 :pos (v2-add p1 N))
          (assoc!! v2 :pos (v2-sub p2 N)))))
    (ec/eachStore (:samples @*gWorld*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [body axis]
  (let [{:keys [edges]} @body]
    (loop [i 0 SZ (n# edges)
           minp *pos-inf* maxp *neg-inf*]
      (if (>= i SZ)
        [minp maxp]
        (let [{:keys [v1]} @(nth edges i)
              dp (v2-dot (:pos @v1) axis)]
          (recur (+ 1 i)
                 SZ (min dp minp) (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intersection??
  "" [[minA maxA] [minB maxB]]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision* "" [B1 B2 ci]
  (let [{c1 :pos edges :edges} @B1
        {c2 :pos} @B2
        {cn :normal} @ci
        sign (v2-dot cn (v2-sub c1 c2))]

    ;;line equation is N*( R - R0 ). We choose B2 ;;as R0
    ;;the normal N is given by the collision normal
    ;;revert the collision normal if it points away from B1
    (if-not (pos? sign)
      (assoc!! ci :normal (v2-neg cn)))

    (loop [i 0 SZ (n# edges)
           cn (:normal @ci) dist *pos-inf*]
      (when (< i SZ)
        ;;calc dist of the vertex from the line using the line equation
        (let [{:keys [v1]} @(nth edges i)
              d (v2-dot cn (v2-sub (:pos @v1) c2))
              t? (< d dist)]
          (if t? (assoc!! ci :vertex v1))
          (recur (+ 1 i)
                 SZ
                 cn
                 (if t? d dist)))))
    true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionTest?? "" [B1 B2 ci]
  (let [{e1 :edges} @B1
        {e2 :edges} @B2
        ec1 (n# e1)
        ec2 (n# e2)]
    (loop [i 0 SZ (+ ec1 ec2)
           minDist *pos-inf* break? false]
      (if (or break? (>= i SZ))
        (when-not break?
          (assoc!! ci :depth minDist)
          ;;ensure that the body containing the collision edge lies in
          ;;B2 and the one containing the collision vertex in B1
          (if (not= B2 (:body @(:edge @ci)))
            (collision* B2 B1 ci) (collision* B1 B2 ci)))
        (let [e' (if (< i ec1) (nth e1 i) (nth e2 (- i ec1)))
              {:keys [v1 v2]} @e'
              {x1 :x y1 :y} (:pos @v1)
              {x2 :x y2 :y} (:pos @v2)
              ;;calc the axis normal to this edge, rotate 90deg left
              axis (v2-norm (vec2 (- y1 y2) (- x2 x1)))
              lineA (projectToAxis B1 axis)
              lineB (projectToAxis B2 axis)
              dist (intersection?? lineA lineB)
              dist' (abs* dist)
              lesso? (<  dist' minDist)]
          (when lesso?
            (assoc!! ci :normal axis :edge e'))
          (recur (+ 1 i)
                 SZ
                 (if lesso? dist' minDist) (> dist 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? [B1 B2]
  (let [{pm1 :pmin px1 :pmax} @B1
        {pm2 :pmin px2 :pmax} @B2
        {minX1 :x minY1 :y} pm1
        {maxX1 :x maxY1 :y} px1
        {minX2 :x minY2 :y} pm2
        {maxX2 :x maxY2 :y} px2]
    (and (<= minX1 maxX2)
         (<= minY1 maxY2)
         (>= maxX1 minX2)
         (>= maxY2 minY1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyActingForces! "" []
  (let [{:keys [gravity]} @*gWorld*]
    (->>
      (fn [s _]
        (if (dynamic? s)
          (doseq [e (:edges @s)
                  :let [{:keys [v1]} @e]]
            (assoc!! v1 :accel gravity))))
      (ec/eachStore (:samples @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [ci]
  (let [{:keys [vertex edge normal depth]} @ci
        {vx :x vy :y} (:pos @vertex)
        {:keys [v1 v2]} @edge
        {x1 :x y1 :y} (:pos @v1)
        {x2 :x y2 :y} (:pos @v2)
        cv (v2-scale normal depth)
        {nx :x ny :y} cv
        T (if (> (abs* (- x1 x2)) (abs* (- y1 y2)))
            (/ (- vx nx x1) (- x2 x1))
            (/ (- vy ny y1) (- y2 y1)))
        T' (- 1 T)
        offset (invert (+ (sqr* T) (sqr* T')))]
    (when (dynamic? (:body @edge))
      (assoc!! v1
               :pos (v2-sub (:pos @v1)
                            (v2-scale cv (* T' 0.5 offset))))
      (assoc!! v2
               :pos (v2-sub (:pos @v2)
                            (v2-scale cv (* T 0.5 offset)))))
    (if (dynamic? (:body @vertex))
      (assoc!! vertex
               :pos (v2-add (:pos @vertex)
                            (v2-scale cv 0.5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkCollision* "" [posCorrection]
  (let [{:keys [samples]} @*gWorld*
        len (ec/countStore samples)]
    (dotimes [i len]
      (let [si (ec/nthStore samples i)]
        (when (:valid? @si)
          (dotimes [j len]
            (when (not= i j)
              (let [sj (ec/nthStore samples j)
                    ci (ci-info)]
                (when (and (:valid? @sj)
                           (overlap? si sj)
                           (collisionTest?? si sj ci))
                  (resolveCollision ci)
                  (updateEdges!)
                  (calcCenters!))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [validator]} @*gWorld*] (validator s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAlgo "" [algoIterCount posCorrection]
  (let [bin #js []
        {:keys [samples frameSecs]} @*gWorld*]
    (applyActingForces!)
    (updateVerlet! frameSecs)
    (updateEdges!)
    (calcCenters!)
    (dotimes [_ algoIterCount]
      (checkCollision* posCorrection))
    (comment
    (ec/eachStore samples
                  (fn [s i]
                    (if-not (:valid? @s)
                      (.push bin s)
                      (updateShape! s frameSecs)))))
    (when (pos? (count bin))
      (doseq [b bin]
        (ec/delFromStore! samples b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (set! *gWorld* (py/initPhysics gravity fps world options))
  (swap! *gWorld* #(assoc %
                          :algoRunner runAlgo
                          :polygon {:draw polyDraw}))
  *gWorld*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



