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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [n# nexth]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert abs*]]
            [czlab.elmo.p2d.physics2d
             :as py :refer [dynamic? rigidBody!]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [VEC2_ZERO *pos-inf* *neg-inf*
                            Point2D vec2
                            v2-len v2-scale v2-add v2-dist
                            v2-rot v2-sub v2-dot v2-negate v2-norm]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *gWorld* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Vertex "" [body pos]
  (let [v (atom {:body body
                 :pos pos
                 :prev pos
                 :accel VEC2_ZERO})
        {:keys [verts]} @*gWorld*]
    (ec/addToStore! verts v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Edge "" [body v1 v2]
  (let [e (gx/Edge (Vertex body v1)
                   (Vertex body v2))
        {:keys [edges]} @*gWorld*]
    (swap! e
           #(assoc %
                   :body body
                   :olen (v2-dist v1 v2)))
    (ec/addToStore! edges e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCenter "" [body]
  (let [{:keys [edges]} @body
        sz (n# edges)
        [C M X]
        (loop [i 0 cx 0 cy 0
               xmin *pos-inf* ymin *pos-inf*
               xmax *neg-inf* ymax *neg-inf*]
          (if (>= i sz)
            [(vec2 cx cy) (vec2 xmin ymin) (vec2 xmax ymax)]
            (let [{:keys [v1 v2]} @(nth edges i)
                  {:keys [x y]} (:pos @v1)]
              (recur (inc i)
                     (+ cx x) (+ cy y)
                     (min xmin x) (min ymin y)
                     (max xmax x) (max ymax y)))))]
    (swap! body
           #(assoc %
                   :pmin M :pmax X
                   :pos (v2-scale C (invert sz)))) body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [p ctx & [styleObj]]
  (let [{:keys [edges]} @p
        sz (n# edges)
        v' (:v1 @(nth edges 0))
        {x0 :x y0 :y} (:pos @v')]
    (ocall! ctx "beginPath")
    (when (some? styleObj)
      (gx/cfgStyle! ctx styleObj))
    (ocall! ctx "moveTo" x0 y0)
    (dotimes [i sz]
      (when-not (zero? i)
        (let [v' (:v1 @(nth edges i))
              {:keys [x y]} (:pos @v')]
          (ocall! ctx "lineTo" x y))))
    (ocall! ctx "closePath")
    (ocall! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyRotate "" [s angle']
  (let [{center :pos :keys [angle edges]} @s
        vs (vec (map (fn [e] (:v1 @e)) edges))
        sz (count vs)
        _ (doseq [v vs
                  :let [p (v2-rot (:pos @v) center angle')]]
            (swap! v #(assoc % :pos p :prev p)))
        vs' (conj vs (nth vs 0))]
    (dotimes [i sz]
      (let [a (nth vs' i)
            b (nexth vs' i)
            e (nth edges i)]
        (swap! e #(assoc % :v1 a :v2 b))))
    (swap! s #(assoc % :angle (+ angle angle')))
    (calcCenter s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [vs & [mass friction bounce]]
  (let [{:keys [samples]} @*gWorld*
        end (dec (n# vs))
        ret (gx/Polygon)
        edges (transient [])]
    (loop [i 0 e' edges]
      (if (= i end)
        (conj! e'
               (Edge ret (nth vs i) (nth vs 0)))
        (recur (inc i)
               (conj! e'
                      (Edge ret (nth vs i) (nexth vs i))))))
    (swap! ret #(assoc %
                       :rotate polyRotate
                       :edges (persistent! edges)))
    (js/console.log (str "adding new poly: " (ec/countStore samples)))
    (-> (calcCenter ret)
        (rigidBody! mass friction bounce))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[depth normal edge vertex]]
  (atom {:depth depth :normal normal :edge edge :vertex vertex}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [v _]
        (let [{:keys [body pos prev accel]} @v]
          (if (dynamic? body)
            (swap! v
                   #(assoc %
                           :prev pos
                           :pos (v2-add pos
                                        (v2-add (v2-sub pos prev)
                                                (v2-scale accel t2))))))))
      (ec/eachStore (:verts @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateEdges! "" []
  (->>
    (fn [e _]
      (let [{:keys [body olen v1 v2]} @e]
        (if (dynamic? body)
          (let [{p1 :pos} @v1
                {p2 :pos} @v2
                v12 (v2-sub p2 p1)
                diff (- (v2-len v12) olen)
                n (v2-norm v12)
                N (v2-scale n (* diff 0.5))]
            ;;push both vertices apart by half of the difference respectively
            ;;so the distance between them equals the original length
            (swap! v1 #(assoc % :pos (v2-add p1 N)))
            (swap! v2 #(assoc % :pos (v2-sub p2 N)))))))
    (ec/eachStore (:edges @*gWorld*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [body axis]
  (let [{:keys [edges]} @body
        sz (n# edges)]
    (loop [i 0
           minp *pos-inf* maxp *neg-inf*]
      (if (>= i sz)
        [minp maxp]
        (let [{:keys [v1]} @(nth edges i)
              dp (v2-dot axis (:pos @v1))]
          (recur (inc i) (min dp minp) (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intersection??
  "" [[minA maxA] [minB maxB]]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision* "" [B1 B2 ci]
  (let [{c1 :pos edges :edges} @B1
        ecnt (count edges)
        {c2 :pos} @B2
        {cn :normal} @ci
        sign (ec/sgn (v2-dot cn (v2-sub c1 c2)))
        ;;line equation is N*( R - R0 ). We choose B2 ;;as R0
        ;;the normal N is given by the collision normal
        ;;revert the collision normal if it points away from B1
        _ (if-not (pos? sign)
            (swap! ci #(assoc %
                              :normal (v2-negate cn))))
        {cn' :normal} @ci]
    (loop [i 0 dist *pos-inf*]
      (when (< i ecnt)
        ;;calc dist of the vertex from the line using the line equation
        (let [{:keys [v1 v2]} @(nth edges i)
              d (v2-dot cn' (v2-sub (:pos @v1) c2))]
          (if (< d dist) (swap! ci #(assoc % :vertex v1)))
          (recur (inc i) (if (< d dist) d dist)))))
    ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionTest?? "" [B1 B2]
  (let [{e1 :edges} @B1
        {e2 :edges} @B2
        ec1 (count e1)
        ec2 (count e2)
        sz (+ ec1 ec2)
        [quit? minDist axis edge]
        (loop [break? false i 0
               minDist *pos-inf* axis nil edge nil]
          (if (or break? (>= i sz))
            [break? minDist axis edge]
            (let [e' (if (< i ec1)
                       (nth e1 i) (nth e2 (- i ec1)))
                  ii (+ 1 i)
                  {:keys [v1 v2]} @e'
                  {x1 :x y1 :y} (:pos @v1)
                  {x2 :x y2 :y} (:pos @v2)
                  ;;calc the axis normal to this edge, rotate 90deg left
                  axis' (v2-norm (vec2 (- y1 y2) (- x2 x1)))
                  lineA (projectToAxis B1 axis')
                  lineB (projectToAxis B2 axis')
                  dist (intersection?? lineA lineB)
                  dist' (abs* dist)
                  lesso? (<  dist' minDist)]
              (recur (> dist 0)
                     ii
                     (if lesso? dist' minDist)
                     (if lesso? axis' axis)
                     (if lesso? e' edge)))))]
    (when-not quit?
      (let [ci (ci-info minDist axis edge)]
        ;;ensure that the body containing the collision edge lies in
        ;;B2 and the one containing the collision vertex in B1
        (if (not= B2 (:body @edge))
          (collision* B2 B1 ci) (collision* B1 B2 ci))))))

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
  (let [{:keys [gravity verts]} @*gWorld*]
    (ec/eachStore verts
                  (fn [v _]
                    (if (dynamic? (:body @v))
                      (swap! v #(assoc % :accel gravity)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [ci]
  (let [{:keys [vertex edge normal depth]} @ci
        {vx :x vy :y} (:pos @vertex)
        {:keys [v1 v2]} @edge
        PB (:body @vertex)
        B1 (:body @v1)
        B2 (:body @v2)
        {x1 :x y1 :y} (:pos @v1)
        {x2 :x y2 :y} (:pos @v2)
        {nx :x ny :y} (v2-scale normal depth)
        T (if (> (abs* (- x1 x2)) (abs* (- y1 y2)))
            (/ (- vx nx x1) (- x2 x1))
            (/ (- vy ny y1) (- y2 y1)))
        T' (- 1 T)
        offset (invert (+ (* T T) (* T' T')))
        ;;calculate the mass at the intersection point
        edgeMass (+ (* T (:mass @B2)) (* T' (:mass @B1)))
        invTotalMass (invert (+ edgeMass (:mass @PB)))
        ratio1 (* (:mass @PB) invTotalMass)
        ratio2 (* edgeMass invTotalMass)
        dt' (* ratio1 offset T')
        dt (* ratio1 offset T)]
    (if (dynamic? (:body @v1))
      (swap! v1
             #(assoc % :pos (vec2 (- x1 (* nx dt')) (- y1 (* ny dt'))))))
    (if (dynamic? (:body @v2))
      (swap! v2
             #(assoc % :pos (vec2 (- x2 (* nx dt)) (- y2 (* ny dt))))))
    (if (dynamic? (:body @vertex))
      (swap! vertex
             #(assoc % :pos (vec2 (+ vx (* nx ratio2)) (+ vy (* ny ratio2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- processCollision "" [ci]
  (let [{:keys [vertex normal edge depth]} @ci
        {e1 :v1 e2 :v2} @edge
        B1 (:body @e1)
        B2 (:body @e2)
        BV (:body @vertex)
        pv (:pos @vertex)
        p1 (:pos @e1)
        p2 (:pos @e2)
        collisionVectorX (* (:x normal) depth)
        collisionVectorY (* (:y normal) depth)
        t
        (if (> (abs* (- (:x p1) (:x p2)))
               (abs* (- (:y p1) (:y p2))))
          (/ (- (:x pv) collisionVectorX (:x p1)) (- (:x p2) (:x p1)))
          (/ (- (:y pv) collisionVectorY (:y p1)) (- (:y p2) (:y p1))))
        lambda (/ 1 (+ (* t t) (* (- 1 t)(- 1 t))))
        edgeMass  (+ (* t (:mass @B2)) (* (- 1 t)(:mass @B1)))
        invCollisionMass (/ 1 (+ edgeMass (:mass @BV)))
        ratio1 (* (:mass @BV) invCollisionMass)
        ratio2 (* edgeMass invCollisionMass)]
    (when (dynamic? B1)
      (swap! e1 #(assoc %
                        :pos
                        (vec2 (- (:x p1) (* collisionVectorX (* (- 1 t) ratio1 lambda)))
                              (- (:y p1) (* collisionVectorY  (* (- 1 t) ratio1 lambda)))))))
    (when (dynamic? B2)
      (swap! e2 #(assoc %
                        :pos
                        (vec2 (- (:x p2) (* collisionVectorX (* t ratio1 lambda)))
                              (- (:y p2) (* collisionVectorY (* t ratio1 lambda)))))))
    (when (dynamic? BV)
      (swap! vertex
             #(assoc %
                     :pos
                     (vec2 (+ (:x pv) (* collisionVectorX ratio2))
                           (+ (:y pv) (* collisionVectorY  ratio2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkCollision* "" [posCorrection]
  (let [{:keys [samples]} @*gWorld*
        len (ec/countStore samples)]
    (updateEdges!)
    (ec/eachStore samples
                  (fn [b _]
                    (if (dynamic? b)
                      (calcCenter b))))
    (dotimes [i len]
      (loop [j (inc i)]
        (when-not (>= j len)
          (let [si (ec/nthStore samples i)
                sj (ec/nthStore samples j)]
            (when (and (:valid? @si)
                       (:valid? @sj))
              (when (overlap? si sj)
                (when-some [ci (collisionTest?? si sj)]
                  (processCollision ci)))))
          (recur (+ 1 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [validator]} @*gWorld*] (validator s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAlgo "" [algoIterCount posCorrection]
  (let [{:keys [samples
                edges
                verts
                frameSecs]} @*gWorld* bin #js []]
    (applyActingForces!)
    (updateVerlet! frameSecs)
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
        (doseq [e (:edges @b)
                :let [{:keys [v1 v2]} @e]]
          (ec/delFromStore! verts v1)
          (ec/delFromStore! verts v2)
          (ec/delFromStore! edges e))
        (ec/delFromStore! samples b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (set! *gWorld* (py/initPhysics gravity fps world options))
  (swap! *gWorld* #(assoc %
                          :polygon {:draw polyDraw}
                          :algoRunner runAlgo
                          :edges (ec/createStore 10)
                          :verts (ec/createStore 10)))
  *gWorld*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



