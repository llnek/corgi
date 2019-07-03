;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.physics2d

  (:require [czlab.mcfud.p2d.core :as pc :refer [gWorld]]
            [czlab.mcfud.afx.math :as m :refer [V2]]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.core
             :as c :refer [_1 _2 do->true
                           POS-INF NEG-INF n# num??]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collision-test??
  "" [s1 s2 ci] ((:collision-test @s1) s1 s2 ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info
  "Collision info."
  [& [d n s e]]
  (atom {:depth (c/num?? d 0)
         :normal (or n (m/vz2))
         :start (or s (m/vz2)) :end (or e (m/vz2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- chgci!
  "Change collision info."
  [ci d n s]
  (c/assoc!! ci
             :depth d :normal n
             :start s :end (m/vec-add s (m/vec-scale n d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rev-ci-dir!
  "Reverse collision direction."
  [ci]
  (let [{:keys [start end normal]} @ci]
    (c/assoc!! ci
               :start end :end start :normal (m/vec-neg normal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- validate-body "" [B]
  (let [{[x y] :pos} @B
        {:keys [width height]
         [left bottom] :origin} @gWorld]
    (if (or (< x left)
            (< y bottom)
            (> x (+ left width))
            (> y (+ bottom height))) (c/assoc!! B :valid? false)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-body! "" [B dt]
  (let [{:keys [width height samples validator]} @gWorld
        {:keys [oid vel accel gvel torque]} @B
        gv' (+ gvel (* torque dt))
        v' (m/vec-add vel (m/vec-scale accel dt))]
    (when true
      ;;update vel += a*t
      ;;move object += v*dt
      ;;update angular vel
      ;rotate object
      (c/assoc!! B :vel v' :gvel gv')
      (pc/move! B (m/vec-scale v' dt))
      (pc/rotate! B (* gv' dt)))
    ;;;;;;
    (if (fn? validator) (validator B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [B1 B2]
  (let [{p1 :pos r1 :bx-radius} @B1
        {p2 :pos r2 :bx-radius} @B2
        v1to2 (m/vec-sub p2 p1)] (not (> (m/vec-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- spoint?? "" [r1Pt n R2]
  (let [{{:keys [vertices]} :shape} @R2]
    ;easier to deal with +ve values
    (loop [i 0 SZ (n# vertices)
           dir (m/vec-neg n) dist NEG-INF sp nil]
      (if (>= i SZ)
        [(some? sp) dist sp]
        (let [v' (nth vertices i)
              proj (m/vec-dot (m/vec-sub v' r1Pt) dir)
              t? (and (pos? proj) (> proj dist))]
          ;;find the longest +ve distance with edge
          (recur (+ 1 i) SZ dir (if t? proj dist) (if t? v' sp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- find-penetration??
  "Shortest axis that's overlapping."
  [R B2]
  (let [{{:keys [normals vertices]} :shape} @R]
    ;;all vertices have corresponding support points?
    (loop [i 0 SZ (n# normals)
           depth POS-INF
           vert nil n' nil support? true]
      (if-not (and support? (< i SZ))
        (if support?
          (chgci! (ci-info)
                  depth
                  n'
                  (m/vec-add vert (m/vec-scale n' depth))))
        (let [v' (nth vertices i)
              dir (nth normals i)
              [ok? dist pt]
              (spoint?? v' dir B2)
              t? (and ok? (< dist depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? dist depth) (if t? pt vert) (if t? dir n') ok?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- test-rect-rect?
  "Check for collision between 2 rectangles"
  [R1 R2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (find-penetration?? R1 R2)
        ci_2 (if ci_1 (find-penetration?? R2 R1))]
    ;;if both are overlapping, choose the shorter normal
    (when ci_2
      (let [{d1 :depth n1 :normal s1 :start} @ci_1
            {d2 :depth n2 :normal s2 :start} @ci_2]
        (if (< d1 d2)
          (chgci! ci d1 n1 (m/vec-sub s1 (m/vec-scale n1 d1)))
          (chgci! ci d2 (m/vec-neg n2) s2))))
    (and ci_1 ci_2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-inside-rect?
  ""
  [R C1 ci nEdge depth]
  (let [{:keys [pos] {:keys [radius]} :shape} @C1
        {{:keys [normals]} :shape} @R
        n (nth normals nEdge)
        rvec (m/vec-scale n radius)]
    (chgci! ci (- radius depth) n (m/vec-sub pos rvec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nface->circle?? "" [R C1]
  (let [{center :pos} @C1
        {{:keys [normals vertices]} :shape} @R]
    (loop [i 0 SZ (n# normals)
           depth NEG-INF nEdge 0 inside? true]
      (if (or (not inside?) (>= i SZ))
        [inside? depth nEdge]
        (let [proj (-> (->> (nth vertices i)
                            (m/vec-sub center))
                       (m/vec-dot (nth normals i)))
              t? (or (pos? proj) (> proj depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? proj depth)
                 (if t? i nEdge)
                 ;;center is outside of rectangle?
                 (if (pos? proj) false inside?)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-outside-rect? "" [R C1 ci nEdge depth]
  (let [{center :pos {:keys [radius]} :shape} @C1
        {{:keys [normals vertices]} :shape} @R
        vn (nth vertices nEdge)
        en (nth normals nEdge)
        ;;V1 is from left vertex of face to center of circle
        ;;V2 is from left vertex of face to right vertex of face
        len (n# normals)
        eX (m/wrap?? nEdge len)
        vX (nth vertices eX)
        V1 (m/vec-sub center vn)
        V2 (m/vec-sub vX vn)
        dot (m/vec-dot V1 V2)]
    ;;the circle is in corner region of vertex[nEdge]
    (if (neg? dot)
      (let [dis (m/vec-len V1)
            n (m/vec-unit V1)
            rvec (m/vec-scale n (- radius))]
        (if-not (> dis radius)
          (do->true (chgci! ci (- radius dis) n (m/vec-add center rvec)))))
      ;;;else
      ;the center of circle is in corner region of vertex[nEdge+1]
      ;v1 is from right vertex of face to center of circle
      ;v2 is from right vertex of face to left vertex of face
      (let [v1 (m/vec-sub center vX)
            v2 (m/vec-neg V2)
            dot (m/vec-dot v1 v2)]
        (cond
          (neg? dot)
          (let [dis (m/vec-len v1)
                n (m/vec-unit v1)
                rvec (m/vec-scale n (- radius))]
            (if-not (> dis radius)
              (do->true (chgci! ci (- radius dis) n (m/vec-add center rvec)))))
          ;;the circle is in face region of face[nEdge]
          (< depth radius)
          (let [rvec (m/vec-scale en radius)]
            (do->true (chgci! ci (- radius depth) en (m/vec-sub center rvec))))
          :else false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collided-rect-circ "" [R C1 ci]
  (let [[inside? depth nEdge] (nface->circle?? R C1)]
    (if inside?
      (circle-inside-rect? R C1 ci nEdge depth)
      (circle-outside-rect? R C1 ci nEdge depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect-collision-test "" [R B2 ci]
  (if (= :circle (get-in @B2 [:shape :type]))
    (collided-rect-circ R B2 ci) (test-rect-rect? R B2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rectangle stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- create-face-normals
  "0--Top;1--Right;2--Bottom;3--Left"
  [vertices]
  (let [[v0 v1 v2 v3] vertices]
    [(m/vec-unit (m/vec-sub v1 v2)) (m/vec-unit (m/vec-sub v2 v3))
     (m/vec-unit (m/vec-sub v3 v0)) (m/vec-unit (m/vec-sub v0 v1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect-rotate "" [B angle']
  (let [{{:keys [vertices] :as S} :shape :keys [angle pos]} @B
        vs' (g/rot-vertices vertices pos angle')]
    (c/assoc!! B
               :angle (+ angle angle')
               :shape
               (assoc S
                      :vertices vs'
                      :normals (create-face-normals vs')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect-move "" [B offset]
  (let [{:keys [pos] {:keys [vertices] :as S} :shape} @B
        v (mapv #(m/vec-add % offset) vertices)]
    (c/assoc!! B
               :pos (m/vec-add pos offset)
               :shape (assoc S
                             :vertices v
                             :normals (create-face-normals v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rect-repos "" [R pt]
  (let [{{:keys [vertices] :as S} :shape :keys [pos]} @R
        [cx cy] pos
        [px py] pt
        delta (V2 (- px cx)(- py cy))
        v (g/shift-vertices vertices delta)]
    (c/assoc!! R
               :pos pt
               :shape (assoc S
                             :vertices v
                             :normals (create-face-normals v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-poly "" [P ctx]
  (let [{:keys [shape pos]} @P] (gx/draw-poly shape ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-rect "" [R ctx]
  (let [{:keys [shape pos]} @R] (gx/draw-poly shape ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectangle
  ""
  ([sz options] (rectangle (:width sz) (:height sz) options))
  ([width height options]
   (pc/set-body-attrs!
     (pc/body (assoc (pc/config??
                       (g/rectangle width height)) :normals [])
              {:rotate rect-rotate
               :move rect-move
               :repos rect-repos
               :collision-test rect-collision-test
               :bx-radius (/ (m/pythag width height) 2)}) options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;circle-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-move "" [C offset]
  (let [{:keys [pos]} @C]
    (c/assoc!! C :pos (m/vec-add pos offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-rotate
  "Rotate angle in counterclockwise"
  [C angle']
  (let [{:keys [angle]} @C]
    (c/assoc!! C :angle (+ angle angle'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collided-circ-circ "" [C1 C2 ci]
  (let [{c1 :pos {r1 :radius} :shape} @C1
        {c2 :pos {r2 :radius} :shape} @C2
        v1to2 (m/vec-sub c2 c1)
        rSum (+ r1 r2)
        dist (m/vec-len v1to2)]
    (cond
      (> dist rSum) ;;no overlap
      false
      (zero? dist) ;;centers overlap
      (do->true
        (chgci! ci
                rSum
                (V2 0 -1)
                (if (> r1 r2)
                  (m/vec-add c1 (V2 0 r1))
                  (m/vec-add c2 (V2 0 r2)))))
      :else ;overlap
      (do->true
        (let [rC2 (-> (m/vec-neg v1to2)
                      (m/vec-unit) (m/vec-scale r2))]
          (chgci! ci
                  (- rSum dist)
                  (m/vec-unit v1to2) (m/vec-add c2 rC2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-collision-test "" [C B2 ci]
  (if (= (get-in @B2 [:shape :type]) :circle)
    (collided-circ-circ C B2 ci) (collided-rect-circ B2 C ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-repos "" [C pt] (c/assoc!! C :pos pt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-circle "" [C ctx]
  (let [{:keys [shape pos angle]} @C]
    (gx/draw-circle shape ctx pos angle true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle "" [radius & [options]]
  (pc/set-body-attrs!
    (pc/body (pc/config?? (g/circle radius))
             {:rotate  circle-rotate
              :repos circle-repos
              :move circle-move
              :bx-radius radius
              :collision-test circle-collision-test}) options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- correct-pos! "" [posCorrection B1 B2 ci]
  (let [{:keys [depth normal]} @ci
        {m1 :im} @B1
        {m2 :im} @B2
        n (* posCorrection
             (/ depth (+ m1 m2)))
        jiggle (m/vec-scale normal n)]
    (if-not (pc/static? B1)
      (pc/move! B1
                (m/vec-scale jiggle (- m1))))
    (if-not (pc/static? B2)
      (pc/move! B2 (m/vec-scale jiggle m2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolve-collision*
  "Compute and apply response impulses for each object."
  [B1 B2 r1 r2 ci rvel rvel_n]
  (let [{e1 :ii b1 :bounce f1 :statF m1 :im} @B1
        {e2 :ii b2 :bounce f2 :statF m2 :im} @B2
        {:keys [normal]} @ci
        bounce' (min b1 b2)
        sticky' (min f1 f2)
        ;;R cross N
        r1xN (m/vec-xss r1 normal)
        r2xN (m/vec-xss r2 normal)
        ;;Calc impulse scalar
        ;;the formula of jN can be found in http://www.myphysicslab.com/collision.html
        jN (/ (* (- (+ 1 bounce')) rvel_n)
              (+ m1 m2 (* r1xN r1xN e1) (* r2xN r2xN e2)))
        ;;impulse is in direction of normal ( from s1 to s2)
        ;;impulse = F dt = m * ?v , ?v = impulse / m
        impulse (m/vec-scale normal jN)]
    (let [{:keys [gvel vel]} @B1]
      (c/assoc!! B1
                 :gvel (- gvel (* r1xN jN e1))
                 :vel (m/vec-sub vel (m/vec-scale impulse m1))))
    (let [{:keys [gvel vel]} @B2]
      (c/assoc!! B2
                 :gvel (+ gvel (* r2xN jN e2))
                 :vel (m/vec-add vel (m/vec-scale impulse m2))))
    ;;rVelocity.dot(tangent) should less than 0
    (let [tangent (->> (m/vec-dot rvel normal)
                       (m/vec-scale normal)
                       (m/vec-sub rvel)
                       (m/vec-unit)
                       (m/vec-neg))
          r1xT (m/vec-xss r1 tangent)
          r2xT (m/vec-xss r2 tangent)
          jT' (/ (* (- (+ 1 bounce')) (m/vec-dot rvel tangent) sticky')
                 (+ m1 m2 (* r1xT r1xT e1) (* r2xT r2xT e2)))
          ;;friction should less than force in normal direction
          jT (if (> jT' jN) jN jT')
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (m/vec-scale tangent jT)]
      (let [{:keys [gvel vel]} @B1]
        (c/assoc!! B1
                   :gvel (- gvel (* r1xT jT e1))
                   :vel (m/vec-sub vel (m/vec-scale impulse m1))))
      (let [{:keys [gvel vel]} @B2]
        (c/assoc!! B2
                   :gvel (+ gvel (* r2xT jT e2))
                   :vel (m/vec-add vel (m/vec-scale impulse m2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolve-collision "" [posCorrection B1 B2 ci]
  (when-not (and (pc/static? B1) (pc/static? B2))
    (correct-pos! posCorrection B1 B2 ci)
    ;;the direction of collisionInfo is always from s1 to s2
    ;;but the Mass is inversed, so start scale with s2 and end scale with s1
    (let
      [{:keys [normal start end]} @ci
       {m1 :im c1 :pos vs1 :vel av1 :gvel} @B1
       {m2 :im c2 :pos vs2 :vel av2 :gvel} @B2
       start' (m/vec-scale start (/ m2 (+ m1 m2)))
       end' (m/vec-scale end (/ m1 (+ m1 m2)))
       p (m/vec-add start' end')
       [r1x r1y :as r1] (m/vec-sub p c1)
       [r2x r2y :as r2] (m/vec-sub p c2)
       ;;newV = V + mAngularVelocity cross R
       v1 (m/vec-add vs1 (V2 (- (* av1 r1y)) (* av1 r1x)))
       v2 (m/vec-add vs2 (V2 (- (* av2 r2y)) (* av2 r2x)))
       rvel (m/vec-sub v2 v1)
       rvel_n (m/vec-dot rvel normal)]
      ;;if objects moving apart ignore
      (when-not (pos? rvel_n)
        (resolve-collision* B1 B2 r1 r2 ci rvel rvel_n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-collision "" [posCorrection]
  (let [{:keys [samples context]} @gWorld
        len (c/count-set samples)]
    (dotimes [i len]
      (loop [j (+ 1 i)]
        (when-not (>= j len)
          (let [si (c/nth-set samples i)
                ci (ci-info)
                sj (c/nth-set samples j)]
            (when (and (:valid? @si)
                       (:valid? @sj)
                       (overlap? si sj)
                       (collision-test?? si sj ci))
              ;;make sure the normal (direction)
              ;;is always si -> sj
              (when (neg? (m/vec-dot (:normal @ci)
                                     (m/vec-sub (:pos @sj)
                                                (:pos @si))))
                (rev-ci-dir! ci))
              (resolve-collision posCorrection si sj ci)))
          (recur (+ 1 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-algo "" [algoIterCount posCorrection]
  (dotimes [_ algoIterCount] (check-collision posCorrection))
  (let [{:keys [samples frame-secs]} @gWorld]
    (c/each-set samples (fn [b _] (update-body! b frame-secs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-body "" [B & args]
  (let [t (get-in @B [:shape :type])]
    (condp = t
      :rectangle (apply draw-rect B args)
      :polygon (apply draw-poly B args)
      :circle (apply draw-circle B args)
      (c/raise! "Unknown shape type: " t)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init-physics
  ""
  [gravity fps world & [options]]
  (pc/init gravity
           fps
           world
           (merge {:body-drawer draw-body
                   :algo-runner run-algo} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

