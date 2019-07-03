;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.verlet2d

  (:require [czlab.mcfud.p2d.core :as pc :refer [gWorld]]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.math :as m :refer [vec2]]
            [czlab.mcfud.afx.core :as c :refer [n# _1 _2 POS-INF NEG-INF]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vertex "" [body pos]
  (atom {:body body :pos pos :prev pos :accel (m/vz2)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- strut "" [body V1 V2]
  (atom {:v1 V1
         :v2 V2
         :body body
         :olen (m/vec-dist (:pos @V1) (:pos @V2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-center! "" [B]
  (let [{:keys [edges]} @B
        pos (loop [i 0 SZ (n# edges) cx 0 cy 0]
              (if (>= i SZ)
                (m/vec-scale (vec2 cx cy) (/ 1 SZ))
                (let [{:keys [v1]} @(nth edges i)
                      [x y] (:pos @v1)]
                  (recur (+ 1 i) SZ (+ cx x) (+ cy y)))))
        [pmin pmax]
        (pc/calc-min-max (mapv #(:pos @(:v1 (deref %))) edges))]
    (c/assoc!! B :pos pos :pmin pmin :pmax pmax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ensure-rigidity "" [B]
  (let [{{:keys [vertices]} :shape} @B
        SZ (n# vertices)]
    (when (> SZ 3)
      (loop [i 0 out [] bin #{}]
        (if (>= i SZ)
          (c/assoc!! B :struts out)
          (let [[o' b'] (if-not (contains? bin i)
                          (let [i2 (mod (+ i 2) SZ)]
                            [(conj out (strut B
                                              (nth vertices i)
                                              (nth vertices i2)))
                             (conj bin i i2)])
                          [out bin])]
            (recur (+ 1 i) o' b'))))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-poly "" [B ctx]
  (let [{outer :edges S :shape inner :struts} @B
        {:keys [vertices]} S]
    (c/call-js! ctx "beginPath")
    (dotimes [i (n# outer)]
      (let [{:keys [v1 v2]} @(nth outer i)
            [x1 y1] (:pos @v1)
            [x2 y2] (:pos @v2)]
        (gx/canvas-batch! ctx ["moveTo" x1 y1] ["lineTo" x2 y2])))
    (c/call-js! ctx "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rot-poly "" [B angle']
  (let [{S :shape center :pos :keys [angle]} @B
        {:keys [vertices]} S]
    (c/assoc!! B :angle (+ angle angle'))
    (doseq [v vertices
            :let [p (m/vec-rot (:pos @v) angle' center)]]
      (c/assoc!! v :pos p :prev p))
    (c/assoc!! B
               :edges
               (loop [i 0 SZ (n# vertices) out []]
                 (if (>= i SZ)
                   out
                   (let [i2 (m/wrap?? i SZ)
                         v1 (nth vertices i)
                         v2 (nth vertices i2)]
                     (recur (+ 1 i)
                            SZ (conj out (strut B v1 v2)))))))
    (calc-center! (ensure-rigidity B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- id1 "" [B & more] B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygon "" [vs & [options]]
  (let [B (pc/body (g/polygon [])
                   {:rotate rot-poly :repos id1 :draw draw-poly})
        {:keys [shape]} @B
        vs' (mapv #(vertex B %) vs)]
    (c/assoc!! B
               :shape (assoc shape
                             :fnv #(:pos (deref %)) :vertices vs')
               :struts []
               :edges (loop [i 0 SZ (dec (n# vs')) e' []]
                        (if (>= i SZ)
                          (conj e'
                                (strut B (nth vs' i) (nth vs' 0)))
                          (recur (+ 1 i)
                                 SZ
                                 (conj e' (strut B (nth vs' i) (c/nexth vs' i)))))))
    (ensure-rigidity B)
    (calc-center! B)
    (pc/set-body-attrs! B options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[depth normal edge vertex]]
  (atom {:depth (c/num?? depth 0)
         :normal (or normal (m/vz2))
         :edge (or edge nil)
         :vertex (or vertex nil)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-vertex! "" [v t2]
  (let [{:keys [pos prev accel]} @v]
    (c/assoc!! v
               :prev pos
               :pos (m/vec-add pos
                             (m/vec-add (m/vec-sub pos prev)
                                        (m/vec-scale accel t2)))) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ensure-strut!"" [edge]
  (let [{:keys [olen v1 v2]} @edge
        {p1 :pos} @v1
        {p2 :pos} @v2
        v12 (m/vec-sub p2 p1)
        diff (- (m/vec-len v12) olen)
        N (-> (m/vec-unit v12)
              (m/vec-scale (* diff 0.5)))]
    ;;push apart by half of the difference
    (c/assoc!! v1 :pos (m/vec-add p1 N))
    (c/assoc!! v2 :pos (m/vec-sub p2 N)) edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-struts! "" [B]
  (doseq [e (:edges @B)] (ensure-strut! e))
  (doseq [e (:struts @B)] (ensure-strut! e)) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resync-body! "" [B]
  (when (pc/dynamic? B) (update-struts! B) (calc-center! B)) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-verlet! "" [dt]
  (let [t2 (* dt dt)]
    (->>
      (fn [B _]
        (when (pc/dynamic? B)
          (let [{{:keys [vertices]} :shape} @B]
            (doseq [v vertices] (update-vertex! v t2))
            (resync-body! B))))
      (c/each-set (:samples @gWorld)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- project->axis "" [B axis]
  (let [{:keys [edges]} @B]
    (loop [i 0 SZ (n# edges)
           minp POS-INF maxp NEG-INF]
      (if (>= i SZ)
        [minp maxp]
        (let [{:keys [v1]} @(nth edges i)
              dp (m/vec-dot (:pos @v1) axis)]
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
        sign (m/vec-dot cn (m/vec-sub c1 c2))]
    ;;line equation is N*( R - R0 ). We choose B2 ;;as R0
    ;;the normal N is given by the collision normal
    ;;revert the collision normal if it points away from B1
    (if (neg? sign)
      (c/assoc!! ci :normal (m/vec-neg cn)))
    (loop [i 0 SZ (n# edges)
           cn (:normal @ci) dist POS-INF]
      (when (< i SZ)
        ;;calc dist of the vertex from the line using the line equation
        (let [{:keys [v1]} @(nth edges i)
              d (m/vec-dot cn (m/vec-sub (:pos @v1) c2))
              t? (< d dist)]
          (if t? (c/assoc!! ci :vertex v1))
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
           minDist POS-INF break? false]
      (if (or break? (>= i SZ))
        (when-not break?
          (c/assoc!! ci :depth minDist)
          ;;ensure that the body containing the collision edge lies in
          ;;B2 and the one containing the collision vertex in B1
          (if (not= B2 (:body @(:edge @ci)))
            (collision* B2 B1 ci) (collision* B1 B2 ci)))
        (let [e' (if (< i ec1) (nth e1 i) (nth e2 (- i ec1)))
              {:keys [v1 v2]} @e'
              [x1 y1] (:pos @v1)
              [x2 y2] (:pos @v2)
              axis (m/vec-unit (vec2 (- y1 y2) (- x2 x1)))
              lineA (project->axis B1 axis)
              lineB (project->axis B2 axis)
              dist (intersection?? lineA lineB)
              dist' (c/abs* dist)
              lesso? (<  dist' minDist)]
          (when lesso? (c/assoc!! ci :normal axis :edge e'))
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
(defn- apply-acting-forces! "" []
  (let [{:keys [gravity]} @gWorld]
    (->>
      (fn [B _]
        (if (pc/dynamic? B)
          (let [{{:keys [vertices]} :shape} @B]
            (doseq [v vertices]
              (c/assoc!! v :accel gravity)))))
      (c/each-set (:samples @gWorld)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolve-collision "" [ci]
  (let [{:keys [vertex edge normal depth]} @ci
        {vx :x vy :y} (:pos @vertex)
        {:keys [v1 v2]} @edge
        [x1 y1] (:pos @v1)
        [x2 y2] (:pos @v2)
        cv (m/vec-scale normal depth)
        [nx ny] cv
        T (if (> (c/abs* (- x1 x2)) (c/abs* (- y1 y2)))
            (/ (- vx nx x1) (- x2 x1))
            (/ (- vy ny y1) (- y2 y1)))
        T' (- 1 T)
        offset (c/num-flip (+ (c/sqr* T) (c/sqr* T')))]
    (when (pc/dynamic? (:body @edge))
      (c/assoc!! v1
                 :pos (m/vec-sub (:pos @v1)
                                 (m/vec-scale cv (* T' 0.5 offset))))
      (c/assoc!! v2
                 :pos (m/vec-sub (:pos @v2)
                                 (m/vec-scale cv (* T 0.5 offset)))))
    (if (pc/dynamic? (:body @vertex))
      (c/assoc!! vertex
                 :pos (m/vec-add (:pos @vertex)
                                 (m/vec-scale cv 0.5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-collision* "" [posCorrection]
  (let [{:keys [samples]} @gWorld
        len (c/count-set samples)]
    (dotimes [i len]
      (let [si (c/nth-set samples i)]
        (when (:valid? @si)
          (dotimes [j len]
            (when (not= i j)
              (let [sj (c/nth-set samples j)
                    ci (ci-info)]
                (when (and (:valid? @sj)
                           ;(overlap? si sj)
                           (collide? si sj ci))
                  (resolve-collision ci)
                  (resync-body! si)
                  (resync-body! sj))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-algo "" [algoIterCount posCorrection]
  (let [{:keys [samples frame-secs]} @gWorld]
    (apply-acting-forces!)
    (update-verlet! frame-secs)
    (dotimes [_ algoIterCount]
      (check-collision* posCorrection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-body "" [B & args]
  (let [{:keys [shape]} @B
        {:keys [type]} shape]
    (apply (:draw shape) (concat [shape] args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init-physics "" [gravity fps world & [options]]
  (pc/init gravity
           fps
           world
           (merge options
                  {:body-drawer draw-body}
                  {:algo-runner run-algo})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



