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
             :as gx :refer [VEC2_ZERO PosInf NegInf
                            Point2D Polygon
                            vec2 v2-scale v2-add
                            v2-sub v2-dot v2-negate v2-norm]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Vertex "" [x y]
  (atom {:body nil :pos (Point2D x y) :prev nil :accel VEC2_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" [v1 v2]
  (atom {:body nil :v1 v1 :v2 v2
         :olen (v2-dist (:pos @v1) (:pos @v2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn PhysicsBody "" [mass]
  (let [p (Polygon)]
    (swap! p #(assoc % :invMass (invert mass)))
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn CollisionInfo "" []
  (atom {:depth 0 :normal nil :edge nil :vertex nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Physics "" []
  (atom {:edges (ec/createStore 10)
         :points (ec/createStore 10) :timestep 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [phy]
  (let [{:keys [timestep points]} @phy
        t2 (* timestep timestep)]
    (ec/eachStore
      points
      (fn [p i]
        (let [{:keys [pos prev accel]} @p]
          (swap! p
                 #(assoc %
                         :prev pos
                         :pos (v2-add pos
                                      (v2-add (v2-sub pos prev)
                                              (v2-scale accel t2)))))))) phy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateEdges! "" [phy]
  (let [{:keys [edges]} @phy]
    (ec/eachStore
      edges
      (fn [e i]
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
          (swap! v2 #(assoc % :pos (v2-sub p2 N)))))) phy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn PhysicsBody "" []
  (atom {:center nil :vertices (array) :edges (array)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projectToAxis "" [body axis]
  (let [{:keys [vertices]} @body
        sz (count vertices)]
    (loop [i 0
           minp PosInf maxp NegInf]
      (if (>= i sz)
        [minp maxp]
        (let [vi (nth vertices i)
              {:keys [pos]} @vi
              dp (v2-dot axis pos)]
          (recur (+ i 1)
                 (min dp minp)
                 (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intervalDistance
  "" [minA maxA minB maxB]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision* "" [phy B1 B2 ci]
  ;;make sure that the collision normal is pointing at B1
  (let [{cn :normal} @ci
        {c1 :center b1vs :vertices} @B1
        b1vsCnt (count b1vs)
        {c2 :center} @B2
        sign (ec/sgn (v2-dot cn (v2-sub c1 c2)))
        ;;line equation is N*( R - R0 ). We choose B2->Center
        ;;as R0 the normal N is given by the collision normal
        ;;revert the collision normal if it points away from B1
        _ (if-not (pos? sign) (swap! ci #(assoc % :normal (v2-negate cn))))]
    (loop [i 0
           minDist PosInf]
      (when (< i b1vsCnt)
        (let [;;calc dist of the vertex from the line using the line equation
              vi (nth b1vs i)
              ii (inc i)
              dist (v2-dot (:normal @ci) (v2-sub (:pos @vi) c2))]
          (if (< dist minDist)
            (swap! ci #(assoc % :vertex vi)))
          (recur ii
                 (if (< dist minDist) dist minDist))))) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision?? "" [phy B1 B2]
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
      (let [ci (CollisionInfo :depth minDist :normal axis :edge edge)
            {:keys [parent]} @edge]
        ;;ensure that the body containing the collision edge lies in
        ;;B2 and the one containing the collision vertex in B1
        (if (not= parent B2)
          (collision* phy B2 B1 ci)
          (collision* phy B1 B2 ci))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- processCollision "" [phy ci]
  (let [{:keys [depth normal edge vertex]} @ci
        {xv :x yv :y} (:pos @vertex)
        {:keys [v1 v2]} @edge
        {x1 :x y1 :y} (:pos @v1)
        {x2 :x y2 :y} (:pos @v2)
        N (v2-scale normal depth)
        {xn :x yn :y} @N
        T (if (> (js/Math.abs (- x1 x2))
                 (js/Math.abs (- y1 y2)))
            (/ (- xv xn x1) (- x2 x1))
            (/ (- yv yn y1) (- y2 y1)))
        offset  (/ 1 (+ (* T T)
                        (* (- 1 T)(- 1 T))))
        offset2 (* 0.5 offset)]
    (swap! v1
           #(assoc %
                   :pos
                   (v2-sub (:pos @v1) (v2-scale N (* offset2 (- 1 T))))))
    (swap! v2
           #(assoc %
                   :pos
                   (v2-sub (:pos @v2) (v2-scale N (* T offset2)))))
    (swap! vertex
           #(assoc % :pos (v2-add (:pos @vertex) (v2-scale N 0.5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



