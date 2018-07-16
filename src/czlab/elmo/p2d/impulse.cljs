;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.impulse

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [assoc!! half* _1 _2 n# do-with]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [abs* sqr* sqrt* fuzzyZero?
                            invert EPSILON fuzzyEqual?]]
            [czlab.elmo.p2d.core :as pc :refer [*gWorld*]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [PI TWO-PI V2_ZERO Point2D
                            wrap?? *pos-inf* *neg-inf*
                            mat2 mat2* m2-vmult m2-xpose
                            v2-neg v2-norm v2-scale v2-sdiv
                            v2-len v2-lensq v2-dist v2-distsq
                            vec2 v2-add v2-sub v2-dot v2-xss v2-sxss]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *dispatch* (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti calcMass! "" (fn [a density] (:type a)))
(defmulti setOrient! "" (fn [a radians] (:type a)))
(defmulti init! "" (fn [a] (:type a)))
(defmulti drawShape "" (fn [a] (:type a)))
(defmulti applyImpulse! "" (fn [a & more] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Shape "" [e & [pt]] (atom {:type e :body nil :pos (or pt V2_ZERO)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt r] (pc/Circle pt r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod init! :circle [c] (calcMass! c 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod calcMass! :circle [c density]
  (let [{:keys [body radius]} @c
        r2 (sqr* radius)
        m (* PI r2 density)
        i (* m r2)]
    (assoc!! body
             :mass m
             :invMass (invert m) :inertia i :invInertia (invert i)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient!
  ::circle [c radians] (swap! c #(assoc % :angle radians)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod drawShape ::circle [c ctx] c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [pt]
  (do-with [s (Shape ::polygon pt)]
           (swap! s #(assoc %
                            :normals []
                            :u (mat2)
                            :vertices []))
           (ec/addToStore! (:samples @*gWorld*) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod init! ::polygon [p] (calcMass! p 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod calcMass! ::polygon [p density]
  (let [{:keys [vertices]} @p
        inv3 (/ 1 3)
        ;;calculate centroid and moment of interia
        [C M I]
        (loop [i 0 SZ (n# vertices)
               c V2_ZERO area 0 I 0]
          (if (>= i SZ)
            [(v2-scale c (invert area)) (* density area) (* density I)]
            (let [{x2 :x y2 :y :as p2} (nth vertices (wrap?? i SZ))
                  {x1 :x y1 :y :as p1} (nth vertices i)
                  D (v2-xss p1 p2)
                  ;;triangle, 3rd vertex is origin
                  triArea (* 0.5 D)
                  x' (+ (sqr* x1) (* x2 x1) (sqr* x2))
                  y' (+ (sqr* y1) (* y2 y1) (sqr* y2))]
              ;;use area to weight the centroid average, not just vertex position
              (recur (+ 1 i)
                     SZ
                     (v2-add c (v2-scale (v2-add p1 p2)
                                         (* triArea inv3)))
                     (+ area triArea)
                     (+ I (* 0.25 inv3 D (+ x' y')))))))]
    ;;;translate vertices to centroid (make the centroid (0, 0)
    ;;for the polygon in model space)
    ;;Not really necessary
    (assoc!! p
             :m M :im (invert M)
             :i I :ii (invert I)
             :vertices (mapv #(v2-sub % C) vertices)) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient! ::polygon [p radians]
  (swap! p #(assoc % :u (mat2* radians))) p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod drawShape ::polygon [p] p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setPolygonBox! "" [p sz]
  (let [{:keys [width height]} sz
        hw (half* width)
        hh (half* height)]
    (swap! p
           #(assoc %
                   :vertices [(Point2D (- hw) (- hh)) (Point2D hw (- hh))
                              (Point2D hw hh) (Point2D (- hw) hh)]
                   :normals [(vec2 0 -1) (vec2 1 0) (vec2 0 1) (vec2 -1 0)])) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcFaceNormals! "" [p]
  (let [{:keys [vertices]} @p
        EE (sqr* EPSILON)]
    (loop [i 0 SZ (n# vertices) out []]
      (if (>= i SZ)
        (do (swap! p #(assoc % :normals out)) p)
        (let [i2 (wrap?? i SZ)
              face (v2-sub (nth vertices i2)
                           (nth vertices i))]
          (assert (> (v2-lensq face) EE))
          (recur (+ 1 i)
                 SZ
                 (conj out (v2-norm (vec2 (:y face)
                                          (- (:x face)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findRightMost?? "" [vertices]
  (loop [i 1 SZ (n# vertices)
         right 0 cx (:x (_1 vertices))]
    (if (>= i SZ)
      right
      (let [x (:x (nth vertices i))
            [r x'] (if (> x cx)
                     [i x]
                     (if (and (= x cx)
                              (< (:y (nth vertices i))
                                 (:y (nth vertices right))))
                       [i cx] [right cx]))] (recur (+ 1 i) SZ r x')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setPolyonVertices! "" [p _vertices]
  (let [rightMost (findRightMost?? _vertices)]
    (loop [hull [rightMost]
           curIndex rightMost break? false]
      (if break?
        (do (->> (loop [i 0 SZ (n# hull) out []]
                   (if (>= i SZ)
                     out
                     (recur (+ 1 i) SZ (conj out (nth _vertices i)))))
                 (assoc!! p :vertices))
            (calcFaceNormals! p))
        (let [nextIndex
              (loop [i 1 SZ (n# _vertices) pos 0]
                (if (>= i SZ)
                  pos
                  (recur (+ 1 i)
                         SZ
                         (if (= pos curIndex)
                           i
                           (let [v' (nth _vertices (last hull))
                                 e1 (v2-sub (nth _vertices pos) v')
                                 e2 (v2-sub (nth _vertices i) v')
                                 c (v2-xss e1 e2)]
                             (if (or (neg? c)
                                     (and (zero? c)
                                          (> (v2-lensq e2)
                                             (v2-lensq e1)))) i pos))))))]
          (recur (conj hull nextIndex)
                 nextIndex
                 (= nextIndex rightMost))))) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The extreme point along a direction within a polygon
(defn- findSupportPoint?? "" [p dir]
  (let [{:keys [vertices]} @p]
    (loop [i 0 SZ (n# vertices) proj *neg-inf* bv nil]
      (if (>= i SZ)
        bv
        (let [v (nth vertices i)
              p' (v2-dot v dir)
              b? (> p' proj)]
          (recur (inc i) SZ (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn manifold "" [& [a b]]
  (atom {:type :manifold
         :A (or a nil)
         :B (or b nil)
         ;;depth of penetration from collision
         :penetration 0
         ;;From A to B
         :normal V2_ZERO
         ;;Points of contact during collision
         :contacts []
         ;;Mixed restitution
         :bounce 0
         ;;Mixed dynamic friction
         :dynaF 0
         ;;Mixed static friction
         :statF 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn solveManifold "" [M]
  (let [{:keys [A B]} @M
        {sa :shape} @A
        {sb :shape} @B]
    ((get (get @*dispatch* (:type @sa)) (:type @sb)) M A B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod init! ::manifold [M]
  (let [{:keys [gravity frameSecs]} @*gWorld*
        {:keys [contacts A B]} @M
        {gvelA :gvel velA :vel posA :pos bounceA :bounce dfA :dynaF sfA :statF} @A
        {gvelB :gvel velB :vel posB :pos bounceB :bounce dfB :dynaF sfB :statF} @B]
    (assoc!! M
             :dynaF (sqrt* (* dfA dfB))
             :statF (sqrt* (* sfA sfB))
             :bounce (min bounceA bounceB))
    (loop [i 0 SZ (n# contacts) E 911]
      (if (or (zero? E)
              (>= i SZ))
        (if (zero? E) (assoc!! M :bounce 0))
        ;;calculate radii from COM to contact
        (let [c (nth contacts i)
              ra (v2-sub c posA)
              rb (v2-sub c posB)
              rv (v2-sub (v2-sub (v2-add velB
                                         (v2-sxss gvelB rb)) velA)
                         (v2-sxss gvelA ra))]
          ;;Determine if we should perform a resting collision or not
          ;;The idea is if the only thing moving this object is gravity,
          ;;then the collision should be performed without any restitution
          (recur (+ 1 i)
                 SZ
                 (if (< (v2-lensq rv)
                        (+ (v2-lensq (v2-scale gravity frameSecs)) EPSILON)) 0 E))))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyImpulseOnManifold! "" [M]
  (let [{:keys [A B bounce contacts normal statF dynaF]} @M]
    (loop [i 0 SZ (n# contacts) loop? true]
      (when (and loop? (< i SZ))
        (let [{imA :im iiA :ii gvelA :gvel velA :vel posA :pos} @A
              {imB :im iiB :ii gvelB :gvel velB :vel posB :pos} @B]
          (recur (+ 1 i)
                 SZ
                 (let [c (nth contacts i)
                       ;;calculate radii from COM to contact
                       ra (v2-sub c posA)
                       rb (v2-sub c posB)
                       ;;relative velocity
                       rv (v2-sub (v2-sub (v2-add velB
                                                  (v2-sxss gvelB rb)) velA)
                                  (v2-sxss gvelA ra))
                       contactVel (v2-dot rv normal)]
                   ;;do not resolve if velocities are separating
                   (if (pos? contactVel)
                     false
                     (let [raCrossN (v2-xss ra normal)
                           rbCrossN (v2-xss rb normal)
                           invMass (+ imA imB
                                      (* (sqr* raCrossN ) iiA)
                                      (* (sqr* rbCrossN ) iiB))
                           ;;calculate impulse scalar
                           j (-> (- (+ 1 bounce))
                                 (* contactVel) (/ invMass) (/ SZ))
                           impulse (v2-scale normal j)]
                       ;;Apply impulse
                       (applyImpulse! A (- impulse) ra)
                       (applyImpulse! B impulse rb)
                       ;;Friction impulse
                       (let [{gvelA :gvel velA :vel} @A
                             {gvelB :gvel velB :vel} @B
                             rv (v2-sub (v2-sub (v2-add velB
                                                        (v2-sxss gvelB rb)) velA)
                                        (v2-sxss gvelA ra))
                             T (->> (v2-dot rv normal)
                                    (v2-scale normal)
                                    (v2-sub rv) (v2-norm))
                             ;; j tangent magnitude
                             jT (-> (- (v2-dot rv T)) (/ invMass) (/ SZ))]
                         ;; Don't apply tiny friction impulses
                         (if (ec/fuzzyZero? jT)
                           false
                           ;;coulumb's law
                           (let [tangentImpulse
                                 (if (< (abs* jT) (* j statF))
                                   (v2-scale T jT)
                                   (v2-scale T (* dynaF (- j))))]
                             ;;Apply friction impulse
                             (applyImpulse! A (v2-neg tangentImpulse) ra)
                             (applyImpulse! B tangentImpulse rb)
                             true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod applyImpulse! ::manifold [M]
  ;;Early out and positional correct if both objects have infinite mass
  (let [{:keys [A B]} @M
        {imA :im velA :vel} @A
        {imB :im velB :vel} @B]
    (if (ec/fuzzyZero? (+ imA imB))
      ;;infinite mass Correction
      (do (assoc!! A :vel V2_ZERO)
          (assoc!! B :vel V2_ZERO))
      (applyImpulseOnManifold! M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positionalCorrection! "" [M]
  (let [{:keys [A B normal penetration]} @M
        {posA :pos imA :im} @A
        {posB :pos imB :im} @B
        slop' 0.05 ;;Penetration allowance
        percent' 0.4 ;; Penetration percentage to correct
        jiggle (v2-scale normal
                         (* percent'
                            (/ (max (- penetration slop') 0) (+ imA imB))))]
    (assoc!! A :pos (v2-sub posA (v2-scale jiggle imA)))
    (assoc!! B :pos (v2-add posB (v2-scale jiggle imB))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Body "" [shape & [x y]]
  (do-with [body (atom {})]
    (assoc!! body
             :pos (if (some? x)
                    (if (number? y) (Point2D x y) x) V2_ZERO)
             :type :body
             :vel V2_ZERO
             :ii 0
             :im 0
             :i 0
             :m 0
             :gvel 0
             :torque 0
             :angle 0
             :force V2_ZERO
             :statF 0.5
             :dynaF 0.3
             :bounce 0.2)
    (assoc!! shape :body body)
    (init! shape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyForce "" [b f]
  (swap! b (fn [{:keys [force] :as root}]
             (assoc root :force (v2-add force f)))) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod applyImpulse! ::body [b impulse contactVector]
  (let [{:keys [vel im ii gvel]} @b]
    (assoc!! b
             :vel (v2-add vel (v2-scale impulse im))
             :gvel (+ gvel (* ii (v2-xss contactVector impulse)))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic! "" [b]
  (assoc!! b :ii 0 :im 0 :m 0 :i 0) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient! ::body [obj radians]
  (assoc!! obj :angle radians)
  (setOrient! (:shape @obj) radians) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateForces! "" [b dt]
  (let [{:keys [im ii vel force gvel torque]} @b
        dt2 (half* dt)
        {:keys [gravity]} @*gWorld*]
    (when-not (zero? im)
      (assoc!! b
               :gvel (+ gvel (* dt2 torque ii))
               :vel (v2-add vel (v2-scale (-> gravity
                                              (v2-add (v2-scale force im))) dt2)))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateVelocity! "" [b dt]
  (let [{:keys [pos im vel gvel angle]} @b]
    (when-not (zero? im)
      (assoc!! b
               :angle (+ angle (* dt gvel))
               :pos (v2-add pos (v2-scale vel dt)))
      (setOrient! b (:angle @b))
      (integrateForces! b dt)) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- step2 "" [dt algoIterCount]
  (let [{:keys [samples manifolds]} @*gWorld*]
    ;;integrate forces
    (ec/eachStore samples
                  (fn [b _] (integrateForces! b dt)))
    ;;initialize collisions
    (doseq [c manifolds] (init! c))
    ;;solve collisions
    (dotimes [_ algoIterCount]
      (doseq [c manifolds] (applyImpulse! c)))
    ;;integrate velocities
    (ec/eachStore samples
                  (fn [b _] (integrateVelocity! b dt)))
    ;;correct positions
    (doseq [c manifolds] (positionalCorrection! c))
    ;;clear all forces
    (ec/eachStore samples
                  (fn [b _]
                    (assoc!! b :force V2_ZERO :torque 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" [dt algoIterCount]
  (let [{:keys [samples]} @*gWorld*]
    (loop [i 0 SZ (ec/countStore samples) ms []]
      (if (>= i SZ)
        (do (assoc!! *gWorld* :manifolds ms)
            (step2 dt algoIterCount))
        (let [A (ec/nthStore samples i)]
          (recur (+ 1 i)
                 SZ
                 (loop [j (+ i 1) ms' ms]
                   (if (>= j SZ)
                     ms'
                     (let [B (ec/nthStore samples j)]
                       (recur (+ 1 j)
                              (if-not (and (zero? (:im @A))
                                           (zero? (:im @B)))
                                (let [m (solveManifold (manifold A B))
                                      c? (not-empty (:contacts @m))]
                                  (if c? (conj ms' m) ms'))
                                ms')))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToCircle "" [M B1 B2]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {radA :radius} @A
        {radB :radius} @B
        radius (+ radA radB)
        ;;calculate translational vector, which is normal
        normal (v2-sub posB posA)
        distSQ (v2-lensq normal)]
    (cond
      (>= distSQ (sqr* radius))
      ;;Not in contact
      (assoc!! M :contacts [])
      :else
      (let [dist (sqrt* distSQ)]
        (if (zero? dist)
          (assoc!! M
                   :penetration radA
                   :normal (vec2 1 0) :contacts [posA])
          (swap! M
                 #(let [n (v2-sdiv normal dist)]
                    (assoc %
                           :penetration (- radius dist)
                           :normal n
                           :contacts [(v2-add (v2-scale n radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToPolygon* "" [M B1 B2 center separation faceNormal]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [normals vertices] bu :u} @B
        {radA :radius} @A
        ;;grab face's vertices
        v1 (nth vertices faceNormal)
        SZ (n# vertices)
        i2 (wrap?? faceNormal SZ)
        v2 (nth vertices i2)]
    ;;check to see if center is within polygon
    (if (< separation EPSILON)
      (swap! M
             #(let [n (v2-neg (m2-vmult bu
                                       (nth normals faceNormal)))]
                (assoc %
                       :normal n
                       :penetration radA
                       :contacts [(v2-add (v2-scale n radA) posA)])))
      ;;determine which voronoi region of the edge center of circle lies within
      (let [dot1 (v2-dot (v2-sub center v1) (v2-sub v2 v1))
            dot2 (v2-dot (v2-sub center v2) (v2-sub v1 v2))]
        (assoc!! M :penetration (- radA separation))
        (cond
          ;;Closest to v1
          (<= dot1 0)
          (when-not (> (v2-distsq center v1) (sqr* radA))
            (assoc!! M
                     :normal (v2-norm (m2-vmult bu (v2-sub v1 center)))
                     :contacts [(v2-add (m2-vmult bu v1) posB)]))
          ;;Closest to v2
          (<= dot2 0)
          (when-not (> (v2-distsq center v2) (sqr* radA))
            (assoc!! M
                     :normal (v2-norm (m2-vmult bu (v2-sub v2 center)))
                     :contacts [(v2-add (m2-vmult bu v2) posB)]))
          ;;Closest to face
          :else
          (let [n (nth normals faceNormal)
                n' (v2-neg (m2-vmult bu n))]
            (when-not (> (v2-dot (v2-sub center v1) n) radA)
              (assoc!! M
                       :normal n'
                       :contacts [(v2-add (v2-scale n' radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToPolygon "" [M B1 B2]
  (swap! M #(assoc % :contacts []))
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [vertices normals] bu :u} @B
        {radA :radius} @A
        ;;Transform circle center to Polygon model space
        center (m2-vmult (m2-xpose bu)
                        (v2-sub posA posB))]
    ;;Find edge with minimum penetration
    ;;Exact concept as using support points in Polygon vs Polygon
    (loop [i 0 SZ (n# vertices)
           sep *neg-inf* n 0 break? false]
      (if (or break? (>= i SZ))
        (if-not break? (circleToPolygon* M B1 B2 center sep n))
        (let [s (v2-dot (nth normals i)
                        (v2-sub center (nth vertices i)))
              [s' n' t']
              (cond (> s radA) [sep n true]
                    (> s sep) [s i false]
                    :else [sep n false])] (recur (+ 1 i) SZ s' n' t'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygonToCircle "" [M B1 B2]
  (circleToPolygon M B2 B1)
  (assoc!! M :normal (v2-neg (:normal @M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findAxisLeastPenetration "" [A B]
  (let [{bA :body au :u :keys [vertices normals]} @A
        {bB :body bu :u} @B
        {posA :pos} @bA
        {posB :pos} @bB]
  (loop [i 0 SZ (n# vertices) bestD *neg-inf* bestIndex 0]
    (if (>= i SZ)
      [bestD bestIndex]
      (let [nw (m2-vmult au (nth normals i))
            ;;Transform face normal into B's model space
            buT (m2-xpose bu)
            n (m2-vmult buT nw)
            ;;Retrieve support point from B along -n
            s (findSupportPoint?? B (v2-neg n))
            ;;Retrieve vertex on face from A, transform into
            ;;B's model space
            v (m2-vmult buT
                       (v2-sub (v2-add (->> (nth vertices i)
                                            (m2-vmult au)) posA) posB))
            ;;Compute penetration distance (in B's model space)
            d (v2-dot n (v2-sub s v))
            b? (> d bestD)]
        (recur (+ 1 i)
               SZ
               (if b? d bestD)
               (if b? i bestIndex)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findIncidentFace?? "" [refPoly incPoly refIndex]
  (let [{bI :body iu :u :keys [normals vertices]} @incPoly
        {posI :pos} @bI
        ;;Calculate normal in incident's frame of reference
        refNormal (->> (nth (:normals @refPoly) refIndex)
                       (m2-vmult (:u @refPoly)) ;; To world space
                       (m2-vmult (m2-xpose iu)))]  ;To incident's model space
    ;;Find most anti-normal face on incident polygon
    (loop [i 0 SZ (n# vertices)
           iFace 0 minDot *pos-inf*]
      (if (>= i SZ)
        ;;Assign face vertices for incidentFace
        [(v2-add (m2-vmult iu (nth vertices iFace)) posI)
         (v2-add (m2-vmult iu
                          (nth vertices (wrap?? iFace SZ))) posI)]
        ;loop
        (let [dot (v2-dot refNormal (nth normals i))
              b? (< dot minDot)]
          (recur (+ 1 i)
                 SZ
                 (if b? i iFace) (if b? dot minDot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clip?? "" [n c faces]
  (let [[face0 face1] faces
        out (array face0 face1)
        ;;Retrieve distances from each endpoint to the line
        ;;d = ax + by - c
        d1 (- (v2-dot n face0) c)
        d2 (- (v2-dot n face1) c)
        sp 0
        ;;If negative (behind plane) clip
        sp (if (<= d1 0) (do (aset out sp face0) (inc sp)) sp)
        sp (if (<= d2 0) (do (aset out sp face1) (inc sp)) sp)
        ;;If the points are on different sides of the plane
        sp (if (< (* d1 d2) 0) ;;less than to ignore -0.0f
             (let [;;Push interesection point
                   alpha (/ d1 (- d1 d2))]
                 (aset out sp (v2-add face0 (v2-scale (v2-sub face1 face0) alpha)))
                 (inc sp)) sp)]
  (assert (not= sp 3))
  ;;Assign our new converted values
  [sp [(aget out 0) (aget out 1)]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygonToPolygon*
  "" [M B1 B2 [penetrationA faceA] [penetrationB faceB]]

  (let [{A :shape} @B1
        {B :shape} @B2
        ;flip Always point from a to b
        [refPoly incPoly refIndex flip?]
        ;;Determine which shape contains reference face
        (if (ec/biasGreater? penetrationA penetrationB)
          [A B faceA false]
          [B A faceB true])
        {bR :body} @refPoly
        {bI :body} @incPoly
        {posR :pos} @bR
        ;;World space incident face
        incidentFaces (findIncidentFace?? refPoly incPoly refIndex)
        {rverts :vertices ru :u} @refPoly
        ;;Setup reference face vertices
        v1 (nth rverts refIndex)
        refIndex (wrap?? refIndex (n# rverts))
        v2 (nth rverts refIndex)
        ;;Transform vertices to world space
        v1 (v2-add (m2-vmult ru v1) posR)
        v2 (v2-add (m2-vmult ru v2) posR)
        ;;Calculate reference face side normal in world space
        sidePlaneNormal (v2-norm (v2-sub v2 v1))
        ;;Orthogonalize
        refFaceNormal (vec2 (:y sidePlaneNormal) (- (:x sidePlaneNormal)))
        ;;ax + by = c
        ;; c is distance from origin
        refC (v2-dot refFaceNormal v1)
        negSide (- (v2-dot sidePlaneNormal v1))
        posSide (v2-dot sidePlaneNormal v2)
        ;;Clip incident face to reference face side planes
        [sp incidentFaces]
        (clip?? (v2-neg sidePlaneNormal) negSide incidentFaces)
        ;;Due to floating point error, possible to not have required points
        quit1? (< sp 2)
        [sp incidentFaces]
        (if quit1?
          [sp incidentFaces]
          (clip?? sidePlaneNormal posSide incidentFaces))
        ;Due to floating point error, possible to not have required points
        quit2? (< sp 2)]
    (when-not (or quit1? quit2?)
      ;;Flip
      (assoc!! M
               :normal
               (if flip? (v2-neg refFaceNormal) refFaceNormal))
      ;;Keep points behind reference face
      (let [sep (- (v2-dot refFaceNormal (_1 incidentFaces)) refC)]
        (if (<= sep 0)
          (swap! M
                 (fn [{:keys [contacts] :as root}]
                   (assoc root
                          :contacts (conj contacts (_1 incidentFaces))
                          :penetration (- sep))))
          (assoc!! M :penetration 0)))
      (let [sep (- (v2-dot refFaceNormal (_2 incidentFaces)) refC)]
        (when (<= sep 0)
          (swap! M
                 (fn [{:keys [penetration contacts] :as root}]
                   (let [cs (conj contacts (_2 incidentFaces))
                         cz (n# cs)]
                     (assoc root
                            :contacts cs
                            ;;average penetration
                            :penetration (/ (- penetration sep) cz))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygonToPolygon "" [M B1 B2]
  (swap! M #(assoc % :contacts []))
  (let [{A :shape} @B1
        {B :shape} @B2
        ;;Check for a separating axis with A's face planes
        [penetrationA faceA] (findAxisLeastPenetration A B)
        skipA? (>= penetrationA 0)
        ;;Check for a separating axis with B's face planes
        [penetrationB faceB] (if skipA?
                               [1 1] ;; hack to skip
                               (findAxisLeastPenetration B A))
        skipB? (>= penetrationB 0)]
    (when-not (or skipA? skipB?)
      (polygonToPolygon* M B1 B2
                         [penetrationA faceA] [penetrationB faceB]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! *dispatch*
        {:circle {:circle circleToCircle :polygon circleToPolygon}
         :polygon {:circle polygonToCircle :polygon polygonToPolygon}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF







