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

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [VEC2_ZERO *pos-inf* *neg-inf*
                            Point2D
                            vec2 v2-scale v2-add v2-dist
                            v2-sub v2-dot v2-negate v2-norm]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *gWorld* (atom {:samples (ec/createStore 10)
                               :edges (ec/createStore 10)
                               :verts (ec/createStore 10)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Vertex "" [body pos]
  (let [v (atom {:body body
                 :pos pos
                 :prev nil
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
        sz (count edges)
        [C M X]
        (loop [i 0 cx 0 cy 0
               xmin *pos-inf* ymin *pos-inf*
               xmax *neg-inf* ymax *neg-inf*]
          (if (>= i sz)
            [(vec2 cx cy) (vec2 xmin ymin) (vec2 xmax ymax)]
            (let [v (:v1 (deref (nth edges i)))
                  {:keys [x y]} (:pos @v)]
              (recur (inc i)
                     (+ cx x) (+ cy y)
                     (min xmin x) (min ymin y)
                     (max xmax x) (max ymax y)))))]
    (swap! body
           #(assoc %
                   :pmin M :pmax X
                   :pos (v2-scale C (invert sz)))) body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [v0 v1 v2 & more]
  (let [vs (concat [v0 v1 v2] more)
        {:keys [samples]} *gWorld*
        ret (gx/Polygon VEC2_ZERO)
        sz (dec (count vs))
        E
        (loop [i 0 edges (transient [])]
          (if (= i sz)
            (persistent! (conj! edges
                                (Edge ret
                                      (nth vs i)
                                      (nth vs 0))))
            (let [a (nth vs i)
                  ii (inc i)
                  b (nth vs ii)]
              (recur ii (conj! edges (Edge ret a b))))))]
    (swap! p #(assoc % :edges E))
    (ec/addToStore! samples
                    (calcCenter p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[depth normal edge vertex]]
  (atom {:depth depth :normal normal :edge edge :vertex vertex}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [v _]
        (let [{:keys [pos prev accel]} @v]
          (swap! v
                 #(assoc %
                         :prev pos
                         :pos (v2-add pos
                                      (v2-add (v2-sub pos prev)
                                              (v2-scale accel t2)))))))
      (ec/eachStore (:verts @*gWorld*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateEdges! "" []
  (->>
    (fn [e _]
      (let [{:keys [olen v1 v2]} @e
            {p1 :pos} @v1
            {p2 :pos} @v2
            v12 (v2-sub p2 p1)
            diff (- (v2-len v12) olen)
            n (v2-norm v12)
            N (v2-scale n (* diff 0.5))]
        ;;push both vertices apart by half of the difference respectively
        ;;so the distance between them equals the original length
        (swap! v1 #(assoc % :pos (v2-add p1 N)))
        (swap! v2 #(assoc % :pos (v2-sub p2 N)))))
    (ec/eachStore (:edges @*gWorld*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [body axis]
  (let [{:keys [edges]} @body
        sz (count edges)]
    (loop [i 0
           minp *pos-inf* maxp *neg-inf*]
      (if (>= i sz)
        [minp maxp]
        (let [{:keys [v1]} (deref (nth edges i))
              dp (v2-dot axis (:pos @v1))]
          (recur (inc i) (min dp minp) (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intervalDistance
  "" [minA maxA minB maxB]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision* "" [B1 B2 ci]
  ;;make sure that the collision normal is pointing at B1
  (let [{c1 :center edges :edges} @B1
        eCnt (count edges)
        {c2 :center} @B2
        {cn :normal} @ci
        sign (ec/sgn (v2-dot cn (v2-sub c1 c2)))
        ;;line equation is N*( R - R0 ). We choose B2->Center
        ;;as R0 the normal N is given by the collision normal
        ;;revert the collision normal if it points away from B1
        _ (if-not (pos? sign) (swap! ci #(assoc % :normal (v2-negate cn))))]
    (loop [i 0
           minDist PosInf]
      (when (< i eCnt)
        (let [;;calc dist of the vertex from the line using the line equation
              vi (:v1 (deref (nth edges i)))
              ii (inc i)
              dist (v2-dot (:normal @ci) (v2-sub (:pos @vi) c2))]
          (if (< dist minDist)
            (swap! ci #(assoc % :vertex vi)))
          (recur ii
                 (if (< dist minDist) dist minDist))))) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionTest?? "" [B1 B2 ci]
  (let [{e1 :edges} @B1
        {e2 :edges} @B2
        ec1 (count e1)
        ec2 (count e2)
        sz (+ ec1 ec2)
        [quit? minDist axis edge]
        (loop [break? false i 0
               minDist PosInf axis nil edge nil]
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
                  [minA maxA] (projectToAxis B1 axis')
                  [minB maxB] (projectToAxis B2 axis')
                  dist (intervalDistance minA maxA minB maxB)
                  dist' (js/Math.abs dist)
                  lesso? (<  dist' minDist)]
              (recur (> dist 0)
                     ii
                     (if lesso? dist' minDist)
                     (if lesso? axis' axis)
                     (if lesso? e' edge)))))]
    (if quit?
      false
      (do (swap! ci #(assoc % :depth minDist :normal axis :edge edge))
          ;;ensure that the body containing the collision edge lies in
          ;;B2 and the one containing the collision vertex in B1
          (if (not= B2 (:body @edge))
            (collision* B2 B1 ci) (collision* B1 B2 ci))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? [b1 b2]
  (let [{minX1 :minX minY1 :minY maxX1 :maxX maxY1 :maxY} @b1
        {minX2 :minX minY2 :minY maxX2 :maxX maxY2 :maxY} @b2]
    (and (<= minX1 maxX2)
         (<= minY1 maxY2)
         (>= maxX1 minX2)
         (>= maxY2 minY1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyActingForces! "" []
  (let [{:keys [gravity verts]} @*gWorld*]
    (ec/eachStore verts (fn [v _]
                          (swap! v #(assoc % :accel gravity))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [posCorrect b1 b2 ci]
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
    (swap! v1
           #(assoc % :pos (vec2 (- x1 (* nx dt')) (- y1 (* ny dt')))))
    (swap! v2
           #(assoc % :pos (vec2 (- x2 (* nx dt)) (- y2 (* ny dt)))))

    (swap! vertex
           #(assoc % :pos (vec2 (+ vx (* nx ratio2)) (+ vy (* ny ratio2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkCollision* "" [posCorrection]
  (let [{:keys [samples]} @*gWorld*
        len (ec/countStore samples)]
    (updateEdges!)
    (ec/eachStore samples
                  (fn [b _] (calcCenter b)))
    (dotimes [i len]
      (loop [j (inc i)]
        (when-not (>= j len)
          (let [si (ec/nthStore samples i)
                ci (ci-info)
                sj (ec/nthStore samples j)]
            (when (and (:valid? @si)
                       (:valid? @sj)
                       (overlap? si sj)
                       (collisionTest?? si sj ci))
              (resolveCollision posCorrection si sj ci)))
          (recur (+ 1 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [validator]} @*gWorld*] (validator s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAlgo "" [algoIterCount posCorrection]
  (applyActingForces!)
  (updateVerlet!)
  (dotimes [_ algoIterCount]
    (checkCollision* posCorrection))
  (let [{:keys [samples
                edges
                verts
                frameSecs]} @*gWorld* bin #js []]
    (ec/eachStore samples
                  (fn [s i]
                    (if-not (:valid? @s)
                      (.push bin s)
                      (updateShape! s frameSecs))))
    (when (pos? (count bin))
      (doseq [b bin]
        (doseq [e (:edges @b)
                :let [{:keys [v1 v2]} @e]]
          (ec/delFromStore! verts v1)
          (ec/delFromStore! verts v2)
          (ec/delFromStore! edges e))
        (ec/delFromStore! samples b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



