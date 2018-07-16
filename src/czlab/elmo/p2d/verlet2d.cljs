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
            [czlab.elmo.p2d.core
             :as pc :refer [*gWorld* dynamic? rigidBody!]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [V2_ZERO *pos-inf* *neg-inf*
                            Point2D vec2 PI TWO-PI
                            v2-len v2-scale v2-add v2-dist v2-sdiv
                            v2-rot v2-sub v2-dot v2-neg v2-norm]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Vertex "" [body pos]
  (atom {:body body :pos pos :prev pos :accel V2_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Edge "" [body V1 V2 & [inner?]]
  (atom {:v1 V1
         :v2 V2
         :body body
         :olen (v2-dist (:pos @V1) (:pos @V2))
         :inner? (if (true? inner?) true false)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listOuterEdges "" [obj]
  (filterv #(false? (:inner? (deref %))) (:edges @obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listInnerEdges "" [obj]
  (filterv #(true? (:inner? (deref %))) (:edges @obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- listVertices "" [obj]
  (mapv #(get (deref %) :v1) (listOuterEdges obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCenter! "" [body]
  (let [edges (listOuterEdges body)]
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
(defn- ensureRigidity "" [obj]
  (let [edges (listOuterEdges obj)
        vs (listVertices obj)
        SZ (n# vs)]
    (when (> SZ 3)
      (loop [i 0 out [] bin #{}]
        (if (>= i SZ)
          (assoc!! obj :edges (concat edges out))
          (let [[o' b'] (if-not (contains? bin i)
                          (let [i2 (mod (+ i 2) SZ)]
                            [(conj out (Edge obj
                                              (nth vs i)
                                              (nth vs i2) true))
                             (conj bin i i2)])
                          [out bin])]
            (recur (+ 1 i) o' b'))))) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx & [styleObj]]
  (let [outer (listOuterEdges p)
        vs (listVertices p)
        inner (listInnerEdges p)]
    (gx/cfgStyle! ctx styleObj)
    (ocall! ctx "beginPath")
    (dotimes [i (n# outer)]
      (let [{:keys [v1 v2]} @(nth outer i)
            {x1 :x y1 :y} (:pos @v1)
            {x2 :x y2 :y} (:pos @v2)]
        (ocall! ctx "moveTo" x1 y1)
        (ocall! ctx "lineTo" x2 y2)))
    (ocall! ctx "stroke")
    ;;inner lines...
    (when (not-empty inner)
      (ocall! ctx "beginPath")
      (oset! ctx "!strokeStyle" "black")
      (dotimes [i (n# inner)]
        (let [{:keys [v1 v2]} @(nth inner i)
              {x1 :x y1 :y} (:pos @v1)
              {x2 :x y2 :y} (:pos @v2)]
          (ocall! ctx "moveTo" x1 y1)
          (ocall! ctx "lineTo" x2 y2)))
      (ocall! ctx "stroke"))
    ;; vertices...
    (doseq [v vs
            :let [{:keys [x y]} (:pos @v)]]
      (ocall! ctx "beginPath")
      (ocall! ctx "arc" x y 2 0 TWO-PI true)
      (oset! ctx "!fillStyle" "black")
      (ocall! ctx "fill")
      (oset! ctx "!strokeStyle" "black")
      (ocall! ctx "stroke"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyRotate "" [s angle']
  (let [{center :pos :keys [angle]} @s
        vs (listVertices s)]
    (assoc!! s :angle (+ angle angle'))
    (doseq [v vs
            :let [p (v2-rot (:pos @v) center angle')]]
      (assoc!! v :pos p :prev p))
    (assoc!! s
             :edges
             (loop [i 0 SZ (n# vs) out []]
               (if (>= i SZ)
                 out
                 (let [i2 (mod (+ 1 i) SZ)
                       v1 (nth vs i)
                       v2 (nth vs i2)]
                   (recur (+ 1 i)
                          SZ (conj out (Edge s v1 v2)))))))
    (-> (ensureRigidity s) (calcCenter! ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [vs & [mass friction bounce]]
  (let [ret (pc/Polygon)
        VS (mapv #(Vertex ret %) vs)]
    (assoc!! ret
             :rotate polyRotate
             :edges
             (loop [i 0 END (dec (n# vs)) e' []]
               (if (= i END)
                 (conj e' (Edge ret (nth VS i) (nth VS 0)))
                 (recur (+ 1 i)
                        END
                        (conj e' (Edge ret (nth VS i) (nexth VS i)))))))
    (-> (ensureRigidity ret)
        (calcCenter!)
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
(defn- updateEdge!"" [edge]
  (let [{:keys [olen v1 v2]} @edge
        {p1 :pos} @v1
        {p2 :pos} @v2
        v12 (v2-sub p2 p1)
        diff (- (v2-len v12) olen)
        N (-> (v2-norm v12)
              (v2-scale (* diff 0.5)))]
    ;;push apart by half of the difference
    (assoc!! v1 :pos (v2-add p1 N))
    (assoc!! v2 :pos (v2-sub p2 N)) edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateEdges! "" [obj]
  (doseq [e (:edges @obj)] (updateEdge! e)) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resyncShape! "" [obj]
  (when (dynamic? obj) (updateEdges! obj) (calcCenter! obj)) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [s _]
        (when (dynamic? s)
          (doseq [v (listVertices s)]
            (updateVertex! v t2))
          (resyncShape! s)))
      (ec/eachStore (:samples @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [body axis]
  (let [edges (listOuterEdges body)]
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
  (let [edges (listOuterEdges B1)
        {c1 :pos} @B1
        {c2 :pos} @B2
        {cn :normal} @ci
        sign (v2-dot cn (v2-sub c1 c2))]

    ;;line equation is N*( R - R0 ). We choose B2 ;;as R0
    ;;the normal N is given by the collision normal
    ;;revert the collision normal if it points away from B1
    (if (neg? sign)
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
(defn- collide? "" [B1 B2 ci]
  (let [e1 (listOuterEdges B1)
        e2 (listOuterEdges B2)
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
              {:keys [inner? v1 v2]} @e'
              {x1 :x y1 :y} (:pos @v1)
              {x2 :x y2 :y} (:pos @v2)
              axis (v2-norm (vec2 (- y1 y2) (- x2 x1)))
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
      (fn [s _]
        (if (dynamic? s)
          (doseq [v (listVertices s)]
            (assoc!! v :accel gravity))))
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
                           ;(overlap? si sj)
                           (collide? si sj ci))
                  (resolveCollision ci)
                  (resyncShape! si)
                  (resyncShape! sj))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAlgo "" [algoIterCount posCorrection]
  (let [{:keys [samples frameSecs]} @*gWorld*]
    (applyActingForces!)
    (updateVerlet! frameSecs)
    (dotimes [_ algoIterCount]
      (checkCollision* posCorrection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (pc/initPhysics gravity
                  fps
                  world
                  (merge options
                         {:algoRunner runAlgo
                          :polygon {:draw polyDraw}})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



