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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#* _1 n# assoc!! nexth]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [invert abs* sqr* num?? *pos-inf* *neg-inf*]]
            [czlab.elmo.p2d.core
             :as pc :refer [*gWorld* dynamic? Body]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [canvasBatchOps! Point2D]]
            [czlab.elmo.afx.math
             :as ma :refer [vec-zero vec2 PI TWO-PI wrap??
                            vec-len vec-scale vec-add vec-dist
                            vec-rot vec-sub vec-dot vec-neg vec-unit]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Vertex "" [body pos]
  (atom {:body body :pos pos :prev pos :accel (vec-zero 2)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Strut "" [body V1 V2]
  (atom {:v1 V1
         :v2 V2
         :body body
         :olen (vec-dist (:pos @V1) (:pos @V2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCenter! "" [B]
  (let [{:keys [edges]} @B
        pos (loop [i 0 SZ (n# edges) cx 0 cy 0]
              (if (>= i SZ)
                (vec-scale (vec2 cx cy) (/ 1 SZ))
                (let [{:keys [v1]} @(nth edges i)
                      [x y] (:pos @v1)]
                  (recur (+ 1 i) SZ (+ cx x) (+ cy y)))))
        [pmin pmax]
        (pc/calcMinMax (mapv #(:pos @(:v1 (deref %))) edges))]
    (assoc!! B :pos pos :pmin pmin :pmax pmax) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ensureRigidity "" [B]
  (let [{{:keys [vertices]} :shape} @B
        SZ (n# vertices)]
    (when (> SZ 3)
      (loop [i 0 out [] bin #{}]
        (if (>= i SZ)
          (assoc!! B :struts out)
          (let [[o' b'] (if-not (contains? bin i)
                          (let [i2 (mod (+ i 2) SZ)]
                            [(conj out (Strut B
                                              (nth vertices i)
                                              (nth vertices i2)))
                             (conj bin i i2)])
                          [out bin])]
            (recur (+ 1 i) o' b'))))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [B ctx]
  (let [{outer :edges S :shape inner :struts} @B
        {:keys [vertices]} S]
    (ocall! ctx "beginPath")
    (dotimes [i (n# outer)]
      (let [{:keys [v1 v2]} @(nth outer i)
            [x1 y1] (:pos @v1)
            [x2 y2] (:pos @v2)]
        (canvasBatchOps! ctx ["moveTo" x1 y1] ["lineTo" x2 y2])))
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyRotate "" [B angle']
  (let [{S :shape center :pos :keys [angle]} @B
        {:keys [vertices]} S]
    (assoc!! B :angle (+ angle angle'))
    (doseq [v vertices
            :let [p (vec-rot (:pos @v) angle' center)]]
      (assoc!! v :pos p :prev p))
    (assoc!! B
             :edges
             (loop [i 0 SZ (n# vertices) out []]
               (if (>= i SZ)
                 out
                 (let [i2 (wrap?? i SZ)
                       v1 (nth vertices i)
                       v2 (nth vertices i2)]
                   (recur (+ 1 i)
                          SZ (conj out (Strut B v1 v2)))))))
    (-> (ensureRigidity B) (calcCenter! ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- id1 "" [B & more] B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [vs & [options]]
  (let [B (Body (gx/Polygon [])
                {:rotate polyRotate :repos id1 :draw polyDraw})
        {:keys [shape]} @B
        vs' (mapv #(Vertex B %) vs)]
    (assoc!! B
             :shape (assoc shape
                           :fnv #(:pos (deref %)) :vertices vs')
             :struts []
             :edges (loop [i 0 SZ (dec (n# vs')) e' []]
                      (if (>= i SZ)
                        (conj e'
                              (Strut B (nth vs' i) (nth vs' 0)))
                        (recur (+ 1 i)
                               SZ
                               (conj e' (Strut B (nth vs' i) (nexth vs' i)))))))
    (ensureRigidity B)
    (calcCenter! B)
    (pc/setBodyAttrs! B options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[depth normal edge vertex]]
  (atom {:depth (num?? depth 0)
         :normal (or normal (vec-zero 2))
         :edge (or edge nil)
         :vertex (or vertex nil)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVertex! "" [v t2]
  (let [{:keys [pos prev accel]} @v]
    (assoc!! v
             :prev pos
             :pos (vec-add pos
                           (vec-add (vec-sub pos prev)
                                    (vec-scale accel t2)))) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ensureStrut!"" [edge]
  (let [{:keys [olen v1 v2]} @edge
        {p1 :pos} @v1
        {p2 :pos} @v2
        v12 (vec-sub p2 p1)
        diff (- (vec-len v12) olen)
        N (-> (vec-unit v12)
              (vec-scale (* diff 0.5)))]
    ;;push apart by half of the difference
    (assoc!! v1 :pos (vec-add p1 N))
    (assoc!! v2 :pos (vec-sub p2 N)) edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateStruts! "" [B]
  (doseq [e (:edges @B)] (ensureStrut! e))
  (doseq [e (:struts @B)] (ensureStrut! e)) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resyncBody! "" [B]
  (when (dynamic? B) (updateStruts! B) (calcCenter! B)) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [B _]
        (when (dynamic? B)
          (let [{{:keys [vertices]} :shape} @B]
            (doseq [v vertices] (updateVertex! v t2))
            (resyncBody! B))))
      (ec/eachStore (:samples @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [B axis]
  (let [{:keys [edges]} @B]
    (loop [i 0 SZ (n# edges)
           minp *pos-inf* maxp *neg-inf*]
      (if (>= i SZ)
        [minp maxp]
        (let [{:keys [v1]} @(nth edges i)
              dp (vec-dot (:pos @v1) axis)]
          (recur (+ 1 i)
                 SZ (min dp minp) (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intersection??
  "" [[minA maxA] [minB maxB]]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision* "" [B1 B2 ci]
  (let [{:keys [edges] c1 :pos} @B1
        {c2 :pos} @B2
        {cn :normal} @ci
        sign (vec-dot cn (vec-sub c1 c2))]

    ;;line equation is N*( R - R0 ). We choose B2 ;;as R0
    ;;the normal N is given by the collision normal
    ;;revert the collision normal if it points away from B1
    (if (neg? sign)
      (assoc!! ci :normal (vec-neg cn)))

    (loop [i 0 SZ (n# edges)
           cn (:normal @ci) dist *pos-inf*]
      (when (< i SZ)
        ;;calc dist of the vertex from the line using the line equation
        (let [{:keys [v1]} @(nth edges i)
              d (vec-dot cn (vec-sub (:pos @v1) c2))
              t? (< d dist)]
          (if t? (assoc!! ci :vertex v1))
          (recur (+ 1 i)
                 SZ
                 cn
                 (if t? d dist)))))
    true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collide? "" [B1 B2 ci]
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
              [x1 y1] (:pos @v1)
              [x2 y2] (:pos @v2)
              axis (vec-unit (vec2 (- y1 y2) (- x2 x1)))
              lineA (projectToAxis B1 axis)
              lineB (projectToAxis B2 axis)
              dist (intersection?? lineA lineB)
              dist' (abs* dist)
              lesso? (<  dist' minDist)]
          (when lesso? (assoc!! ci :normal axis :edge e'))
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
      (fn [B _]
        (if (dynamic? B)
          (let [{{:keys [vertices]} :shape} @B]
            (doseq [v vertices]
              (assoc!! v :accel gravity)))))
      (ec/eachStore (:samples @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [ci]
  (let [{:keys [vertex edge normal depth]} @ci
        {vx :x vy :y} (:pos @vertex)
        {:keys [v1 v2]} @edge
        [x1 y1] (:pos @v1)
        [x2 y2] (:pos @v2)
        cv (vec-scale normal depth)
        [nx ny] cv
        T (if (> (abs* (- x1 x2)) (abs* (- y1 y2)))
            (/ (- vx nx x1) (- x2 x1))
            (/ (- vy ny y1) (- y2 y1)))
        T' (- 1 T)
        offset (invert (+ (sqr* T) (sqr* T')))]
    (when (dynamic? (:body @edge))
      (assoc!! v1
               :pos (vec-sub (:pos @v1)
                             (vec-scale cv (* T' 0.5 offset))))
      (assoc!! v2
               :pos (vec-sub (:pos @v2)
                             (vec-scale cv (* T 0.5 offset)))))
    (if (dynamic? (:body @vertex))
      (assoc!! vertex
               :pos (vec-add (:pos @vertex)
                             (vec-scale cv 0.5))))))

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
                           ;(overlap? si sj)
                           (collide? si sj ci))
                  (resolveCollision ci)
                  (resyncBody! si)
                  (resyncBody! sj))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAlgo "" [algoIterCount posCorrection]
  (let [{:keys [samples frameSecs]} @*gWorld*]
    (applyActingForces!)
    (updateVerlet! frameSecs)
    (dotimes [_ algoIterCount]
      (checkCollision* posCorrection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawBody "" [B & args]
  (let [{:keys [shape]} @B
        {:keys [type]} shape]
    (apply (:draw shape) (concat [shape] args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (pc/initPhysics gravity
                  fps
                  world
                  (merge options
                         {:bodyDrawer drawBody}
                         {:algoRunner runAlgo})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



