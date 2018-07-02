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

  (:require [czlab.elmo.afx.core :as ec]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [VEC2_ZERO PosInf NegInf
                            vec2 v2-scale v2-add
                            v2-sub v2-dot v2-negate v2-norm]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Point2D "" []
  (atom {:pos VEC2_ZERO :prev VEC2_ZERO :accel VEC2_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Physics "" []
  (atom {:edgeCount 0 :pointCount 0 :points (ec/createStore 10) :timestep 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateVerlet! "" [phy]
  (let [{:keys [timestep points]} @phy]
    (ec/eachStore points
                  (fn [p i]
    (doseq [p points
            :let [{:keys [pos prev accel]} @p]]
      (swap! p
             #(assoc %
                     :pos
                     (v2-add pos
                             (v2-sub pos (v2-add prev
                                                 (v2-scale accel
                                                           (* timestep timestep)))))
                     :prev pos)))
    phy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Edge "" []
  (atom {:v1 nil :v2 nil :olen 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateEdges! "" [phy]
  (let [{:keys [edgeCount edges]} @phy]
    (doseq [e edges
            :let [{:keys [olen v1 v2]} @e
                  v1v2 (v2-sub (:pos @v2) (:pos @v1))
                  v1v2Length (v2-len v1v2)
                  diff (- v1v2Length olen)
                  n (v2-norm v1v2)
                  N (v2-scale n (* diff 0.5))]]
      ;;push both vertices apart by half of the difference respectively
      ;;so the distance between them equals the original length
      (swap! v1
             #(assoc % :pos (v2-add (:pos @v1) N)))
      (swap! v2
             #(assoc % :pos (v2-sub (:pos @v2) N))))
    phy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn PhysicsBody "" []
  (atom {:vertexCount 0 :edgeCount 0 :vertices (array) :edges (array)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn projectToAxis "" [body axis]
  (let [{:keys [vertices]} @body
        sz (count vertices)
        v0 (nth vertices 0)
        {:keys [pos]} @v0
        dotP (v2-dot axis pos)]
    (loop [i 1 minp dotP maxp dotP]
      (if (>= i sz)
        [minp maxp]
        (let [vi (nth vertices i)
              dp (v2-dot axis (:pos @vi))]
          (recur (+ i 1)
                 (min dp minp)
                 (max dp maxp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn detectCollision? "" [phy B1 B2]
  (let [{:keys []} @phy
        {e1 :edges ec1 :edgeCount} @B1
        {e2 :edges ec2 :edgeCount} @B2
        sz (+ ec1 ec2)]
    (loop [i 0 break? false]
      (if (or break?
              (>= i sz))
        (not break?)
        (let [E (if (< i ec1)
                  (nth e1 i)
                  (nth e2 (- i ec1)))
              {:keys [v1 v2]} @E
              {x1 :x y1 :y} @v1
              {x2 :x y2 :y} @v2
              ;;calc the axis normal to this edge
              axis (v2-normal (vec2 (- y1 y2) (- x2 x1)))
              [minA maxA] (projectToAxis B1 axis)
              [minB maxB] (projectToAxis B2 axis)
              dist (intervalDistance minA maxA minB maxB)]
          ;;if the intervals don't overlap, no collision
          (recur (inc i) (> dist 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- intervalDistance
  "" [minA maxA minB maxB]
  (if (< minA minB) (- minB maxA) (- minA maxB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn CollisionInfo "" []
  (atom {:depth 0 :normal nil :edge nil :vertex nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision? "" [phy B1 B2]
  (let [{:keys []} @phy
        {e1 :edges ec1 :edgeCount} @B1
        {e2 :edges ec2 :edgeCount} @B2
        sz (+ ec1 ec2)
        [quit? minDist axis edge]
        (loop [break? false i 0
               minDist js/POSITIVE_INFINITY axis nil edge nil]
          (if (or break? (>= i sz))
            [break? minDist axis edge]
            (let [E' (if (< i ec1)
                       (nth e1 i) (nth e2 (- i ec1)))
                  ii (+ 1 i)
                  {:keys [v1 v2]} @E'
                  {x1 :x y1 :y} @v1
                  {x2 :x y2 :y} @v2
                  ;;calc the axis normal to this edge
                  axis' (v2-normal (vec2 (- y1 y2) (- x2 x1)))
                  [minA maxA] (projectToAxis B1 axis')
                  [minB maxB] (projectToAxis B2 axis')
                  dist (intervalDistance minA maxA minB maxB)
                  dist' (js/Math.abs dist)]
              (cond
                (> dist 0)
                (recur true nil nil nil nil nil)
                (<  dist' minDist)
                (recur break? ii dist' axis' E')
                :else
                (recur break? ii minDist axis edge)))))]
    (if break?
      false
      (let [ci (CollisionInfo :depth minDist :normal axis :edge edge)
            {:keys [parent]} @edge]
        ;;ensure that the body containing the collision edge lies in
        ;;B2 and the one containing the collision vertex in B1
        (if (not= parent B2)
          (xxx B2 B1 ...)
          (xxx B1 B2 ...))))))

(defn xxx "" []
  ;;make sure that the collision normal is pointing at B1
  (let [sign (sgn (v2-dot (:normal @ci)
                          (v2-sub (:center @B1) (:center @B2))))
        ;;line equation is N*( R - R0 ). We choose B2->Center
        ;;as R0 the normal N is given by the collision normal
        ;;revert the collision normal if it points away from B1
        _ (if (not= sign 1)
            (swap! ci #(assoc % :normal (v2-negate (:normal @ci)))))
        {:keys [vertices vertexCount]} @B1]
    (loop [i 0
           minDist js/POSITIVE_INFINITY]
      (if (>= i vertexCount)
        []
        (let [;;calc dist of the vertex from the line using the line equation
              vi (nth vertices i)
              ii (inc i)
              dist (v2-dot (:normal @ci)
                           (v2-sub (:pos @vi) (:center @B2)))]
          (if (< dist minDist)
            (swap! ci #(assoc % :vertex vi)))
          (if (< dist minDist)
            (recur ii dist)
            (recur ii minDist)))))
    true))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn processCollision "" [phy ci]
  (let [{:keys [depth normal edge vertex]} @ci
        {xv :x yv :y} @vertex
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



