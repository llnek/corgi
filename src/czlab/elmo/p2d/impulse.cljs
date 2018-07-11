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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 _2 n#]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [fuzzyZero? invert EPSILON fuzzyEqual?]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [PI TWO-PI V2_ZERO Point2D
                            *pos-inf* *neg-inf*
                            mat2 mat2* m2-mult
                            vec2 v2-add v2-sub v2-dot v2-xss v2-sxss]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nxi "" [i len] (mod (+ 1 i) len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti computeMass "" (fn [a density] (:type a)))
(defmulti setOrient "" (fn [a radians] (:type a)))
(defmulti copyShape "" (fn [a] (:type a)))
(defmulti initShape "" (fn [a] (:type a)))
(defmulti drawShape "" (fn [a] (:type a)))
(defmulti applyImpulse! "" (fn [a & more] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- Shape
  "" [e] (atom {:type e :body nil})); :radius 0 :u nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [r]
  (let [c (Shape :circle)]
    (swap! c #(assoc % :radius r)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initShape :circle [c] (computeMass c 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod copyShape :circle [c] (atom @c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod computeMass :circle [c density]
  (let [{:keys [body radius]} @c
        r2 (sqr* radius)]
    (swap! body
           #(let [m (* PI r2 density)
                  i (* m r2)]
              (assoc % :m m :im (invert m) :i i :ii (invert i)))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient
  :circle [c radians] (swap! c #(assoc % :angle radians)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod drawShape :circle [c ctx] c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [] (let [s (Shape :polygon)] s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initShape :polygon [p] (computeMass p 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod copyShape :polygon [p] (atom @p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod computeMass :polygon [p density]
  (let [{:keys [vertices]} @p
        SZ (n# vertices)
        inv3 (/ 1 3)
        ;;calculate centroid and moment of interia
        [C M I]
        (loop [i 0 c V2_ZERO area 0 I 0]
          (if (>= i SZ)
            [(v2-scale c (invert area)) (* density area) (* density I)]
            (let [{x2 :x y2 :y :as p2} (nth vertices (nxi i SZ))
                  {x1 :x y1 :y :as p1} (nth vertices i)
                  D (v2-xss p1 p2)
                  ;;triangle, 3rd vertex is origin
                  triArea (* 0.5 D)
                  x' (+ (sqr* x1) (* x2 x1) (sqr* x2))
                  y' (+ (sqr* y1) (* y2 y1) (sqr* y2))]
              ;;use area to weight the centroid average, not just vertex position
              (recur (+ 1 i)
                     (v2-add c (v2-scale (v2-add p1 p2) (* triArea inv3)))
                     (+ area triArea)
                     (+ I (* 0.25 inv3 D (+ x' y')))))))]
    ;;;translate vertices to centroid (make the centroid (0, 0)
    ;;for the polygon in model space)
    ;;Not really necessary
    (swap! p
           #(assoc %
                   :m M :im (invert M)
                   :i I :ii (invert I)
                   :vertices (mapv #(v2-sub % C) vertices))) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient :polygon [p radians]
  (swap! p #(assoc % :u (mat2* radians))) p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod drawShape :polygon [p] p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setPolygonBox "" [p hw hh]
  (swap! p
         #(assoc %
                 :vertices [(Point2D -hw -hh) (Point2D hw -hh)
                            (Point2D hw hh) (Point2D -hw hh)]
                 :normals [(vec2 0 -1) (vec2 1 0) (vec2 0 1) (vec2 -1 0)])) p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcFaceNormals! "" [p]
  (let [{:keys [vertices]} @p
        EE (sqr* EPSILON)
        SZ (n# vertices)
        out (transient [])]
    (dotimes [i SZ]
      (let [i2 (nxi i SZ)
            face (v2-sub (nth vertices i2)
                         (nth vertices i))]
        ;;ensure meaningful length
        (assert (> (v2-lensq face) EE))
        (conj! out (v2-norm (vec2 (:y face)
                                  (- (:x face)))))))
    (swap! p #(assoc % :normals (persistent! out))) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findRightMost "" [vertices]
  (loop [i 1 SZ (n# vertices) right 0 cx (:x (_1 vertices))]
    (if (>= i SZ)
      right
      (let [x (:x (nth vertices i))
            [r x'] (if (> x cx)
                     [i x]
                     (if (and (= x cx)
                              (< (:y (nth vertices i))
                                 (:y (nth vertices right))))
                       [i cx] [right cx]))] (recur (inc i) SZ r x')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setPolyonVertices! "" [p _vertices]
  (let [rightMost (findRightMost _vertices)]
    (loop [hull [rightMost]
           curIndex rightMost break? false]
      (if break?
        (let [verts (transient [])]
          (doseq [i hull] (conj! verts (nth _vertices i)))
          (swap! p #(assoc % :vertices (persistent! verts)))
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
                                 e1 (v2-sub (nth vertices pos) v')
                                 e2 (v2-sub (nth vertices i) v')
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
(defn- findSupportPoint "" [p dir]
  (let [{:keys [vertices]} @p]
    (loop [i 0 SZ (n# vertices) proj *neg-inf* bv nil]
      (if (>= i SZ)
        bv
        (let [v (nth vertices i)
              p' (v2-dot v dir)
              b? (> p' proj)]
          (recur (inc i) SZ (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn manifold "" []
  (atom {:type :manifold
         :A nil
         :B nil
         ;;depth of penetration from collision
         :penetration 0
         ;;From A to B
         :normal nil
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
    ((get (get *dispatch* (:type sa)) (:type sb)) M A B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initManifold "" [M]
  (let [{:keys [contacts A B]} @M
        {gvelA :angVel velA :vel posA :pos bounceA :bounce dfA :dynaF sfA :statF} @A
        {gvelB :angVel velB :vel posB :pos bounceB :bounce dfB :dynaF sfB :statF} @B]
    (swap! M
           #(assoc %
                   :bounce (min bounceA bounceB)
                   :dynaF (sqrt* (* dfA dfB))
                   :statF (sqrt* (* sfA sfB))))
    (loop [i 0 SZ (n# contacts) E 911]
      (if (or (zero? E)
              (>= i SZ))
        (if (zero? E) (swap! M #(assoc % :bounce 0)))
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
                        (+ (v2-lensq
                             (v2-scale gravity
                                       (/ 1 60))) EPSILON)) 0 E))))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyImpulseOnManifold! "" [M]
  (let [{:keys [A B bounce contacts normal statF dynaF]} @M]
    (loop [i 0 SZ (n# contacts) loop? true]
      (when (and loop? (< i SZ))
        (let [{imA :im iiA :ii gvelA :angVel velA :vel posA :pos} @A
              {imB :im iiB :ii gvelB :angVel velB :vel posB :pos} @B]
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
                       (let [{gvelA :angVel velA :vel} @A
                             {gvelB :angVel velB :vel} @B
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
                                   (v2-scale T (* df (- j))))]
                             ;;Apply friction impulse
                             (applyImpulse! A (v2-negate tangentImpulse) ra)
                             (applyImpulse! B tangentImpulse rb)
                             true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod applyImpulse! :manifold [M]
  ;;Early out and positional correct if both objects have infinite mass
  (let [{:keys [A B]} @M
        {imA :im velA :vel} @A
        {imB :im velB :vel} @B]
    (if (ec/fuzzyZero? (+ imA imB))
      ;;infinite mass Correction
      (do (swap! A #(assoc % :vel V2_ZERO))
          (swap! B #(assoc % :vel V2_ZERO)))
      (applyImpulseOnManifold! M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positionalCorrection "" [M]
  (let [{:keys [A B penetration]} @M
        {posA :pos imA :im} @A
        {posB :pos imB :im} @B
        slop' 0.05 ;;Penetration allowance
        percent' 0.4 ;; Penetration percentage to correct
        jiggle (v2-scale normal
                         (* percent'
                            (/ (max (- penetration slop') 0) (+ imA imB))))]
    (swap! A
           #(assoc %
                   :pos
                   (v2-sub posA (v2-scale jiggle imA))))
    (swap! B
           #(assoc %
                   :pos
                   (v2-add posB (v2-scale jiggle imB)))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body "" [shape & [x y]]
  (let [body (atom {})]
    (swap! body
           #(assoc %
                   :pos (if (some? x)
                          (if (number? y) (Point2D x y) x) V2_ZERO)
                   :type :body
                   :vel V2_ZERO
                   :ii 0
                   :im 0
                   :i 0
                   :m 0
                   :angVel 0
                   :torque 0
                   :angle 0
                   :force V2_ZERO
                   :statF 0.5
                   :dynaF 0.3
                   :bounce 0.2))
    (swap! shape #(assoc % :body body))
    (initShape shape)
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyForce "" [b f]
  (swap! b (fn [{:keys [force] :as root}]
             (assoc root :force (v2-add force f)))) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod applyImpulse! :body [b impulse contactVector]
  (let [{:keys [vel im ii angVel]} @b]
    (swap! b
           #(assoc %
                   :vel (v2-add vel (v2-scale impulse im))
                   :angVel (+ angVel
                              (* ii (v2-xss contactVector impulse))))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic! "" [b]
  (swap! b #(assoc %
                   :ii 0 :im 0 :m 0 :i 0)) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient :body [obj radians]
  (swap! obj #(assoc % :angle radians))
  (setOrient (:shape @obj) radians) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateForces "" [b dt]
  (let [{:keys [im ii vel force angVel torque]} @b
        dt' (/ dt 2)
        {:keys [gravity]} @*gWorld*]
    (when-not (zero? im)
      (swap! b
             #(assoc %
                     :angVel (+ angVel (* dt' torque ii))
                     :vel (+ vel (* dt' (+ (* im force) gravity)))))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateVelocity "" [b dt]
  (let [{:keys [pos im vel angVel angle]} @b]
    (when-not (zero? im)
      (swap! b
             #(assoc %
                     :angle (+ angle (* dt angVel))
                     :pos (v2-add pos (v2-scale vel dt))))
      (setOrient b (:angle @b))
      (integrateForces b dt))
    b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- step2 "" [contacts dt]
  (let [{:keys [samples]} @*gWorld*]
    ;;integrate forces
    (ec/eachStore samples
                  (fn [b _] (integrateForces b dt)))
    ;;initialize collision
    (doseq [c contacts] (initContact c))
    ;;solve collisions
    (dotimes [_ XXX]
      (doseq [c contacts] (applyImpulse! c)))
    ;;integrate velocities
    (ec/eachStore samples
                  (fn [b _] (integrateVelocity b dt)))
    ;;correct positions
    (doseq [c contacts] (positionalCorrection c))
    ;;clear all forces
    (ec/eachStore samples
                  (fn [b _]
                    (.Set (:force @b) 0 0)
                    (swap! b #(assoc % :torque 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" [dt]
  (let [{:keys [samples]} @*gWorld*
        contacts (transient [])
        sz (ec/countStore samples)]
    (dotimes [i sz]
      (let [A (ec/nthStore samples i)]
        (loop [j (+ i 1)]
          (when (< j sz)
            (let [B (ec/nthStore samples j)]
              (when-not (and (zero? (:im @A))
                             (zero? (:im @B)))
                (let [m (solve (manifold A B))]
                  (if (pos? (:contact_count m)) (conj! contacts m))))
              (recur (inc j)))))))
    (step2 (persistent! contacts) dt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circleToCircle "" [m B1 B2]
  (let [{:keys []} @m
        {A :shape} @B1
        {B :shape} @B2
        ;;calculate translational vector, which is normal
        normal (v2-sub (:pos @B2) (:pos @B1))
        dist_sqr (v2-lensq normal)
        radius (+ (:radius @A) (:radius @B))]
    (cond
      (>= dist_sqr (sqr radius))
      ;;Not in contact
      (swap! m #(assoc % :contacts []))
      :else
      (let [distance (js/Math.sqrt dist_sqr)]
        (if (zero? distance)
          (swap! m
                 #(assoc %
                         :penetration (:radius @A)
                         :normal (vec2 1 0)
                         :contacts [(:pos @B1)]))
          (swap! m
                 #(assoc %
                         :penetration (- radius distance)
                         :normal (v2-scale normal (invert distance)) ;;Faster than using Normalized since we already performed sqrt
                         :contacts [(v2-add (v2-scale (:normal @m)
                                                       (:radius @A)) (:pos @B1))])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circleToPolygon "" [m B1 B2]
  (swap! m #(assoc % :contacts []))
  (let [{:keys []} @m
        {posa :pos A :shape} @B1
        {posb :pos B :shape} @B2
        {bnorms :normals bverts :vertices} @B
        {rada :radius} @A
        sz (n# bverts)
        ;;Transform circle center to Polygon model space
        center (m2-mult (m2-xpose (:u @B))
                        (v2-sub posa posb))]
    ;;Find edge with minimum penetration
    ;;Exact concept as using support points in Polygon vs Polygon
    (loop [i 0 sep *neg-inf* n 0 quit? false]
      (if (or break? (>= i sz))
        (if-not break? (circleToPolygon* m B1 B2 sep n))
        (let [s (v2-dot (nth bnorms i)
                        (v2-sub center (nth bverts i)))
              [s' n' t'] (cond (> s rada) [sep n true]
                               (> s sep) [s i false]
                               :else [sep n false])]
          (recur (inc i) s' n' t'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToPolygon* "" [m B1 B2 separation faceNormal]
  (let [{:keys []} @m
        {posa :pos A :shape} @B1
        {posb :pos B :shape} @B2
        {bnorms :normals bverts :vertices} @B
        {rada :radius} @A
        sz (n# bverts)
        ;;grab face's vertices
        v1 (nth bverts faceNormal)
        i2 (mod (+ 1 faceNormal) sz)
        v2 (nth bverts i2)]
    ;;check to see if center is within polygon
    (if (< separation EPSILON)
      (swap! m
             #(assoc %
                     :normal (v2-negate (m2-mult (:u @B) (nth bnorms faceNormal)))
                     :contacts [(v2-add (v2-scale (:normal @m) rada) (:pos @B1))]
                     :penetration rada))
      ;;determine which voronoi region of the edge center of circle lies within
      (let [dot1 (v2-dot (v2-sub center v1) (v2-sub v2 v1))
            dot2 (v2-dot (v2-sub center v2) (v2-sub v1 v2))]
        (swap! m #(assoc % :penetration (- rada separation)))
        (cond
          ;;Closest to v1
          (<= dot1 0)
          (when-not (> (sqr* (v2-dist center v1)) (sqr rada))
            (swap! m
                   #(assoc %
                           :normal (v2-norm (m2-mult (:u @B) (v2-sub v1 center)))
                           :contacts [(v2-add (m2-mult (:u @B) v1) (:pos @B2))])))
          ;;Closest to v2
          (<= dot2 0)
          (when-not (> (sqr* (v2-dist center v2)) (sqr* rada))
            (swap! m
                   #(assoc %
                           :normal (v2-norm (m2-mult (:u @B) (v2-sub v2 center)))
                           :contacts [(v2-add (m2-mult (:u @B) v2) (:pos @B2))])))
          ;;Closest to face
          :else
          (let [n (nth bnorms faceNormal)
                n' (v2-negate (m2-mult (:u @B) n))]
            (when-not (> (v2-dot (v2-sub center v1) n) rada)
              (swap! m
                     #(assoc %
                             :normal n'
                             :contacts [(v2-add (v2-scale n' rada) (:pos @B1))])))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygonToCircle "" [m B1 B2]
  (circleToPolygon m B2 B1)
  (swap! m #(assoc % :normal (v2-negate (:normal @m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findAxisLeastPenetration "" [A B]
  (let [{bodya :body au :u averts :vertices anorms :normals} @A
        {bodyb :body bu :u} @B
        sz (n# averts)]
  (loop [i 0 bestD *neg-inf* bestIndex 0]
    (if (>= i sz)
      [bestD bestIndex]
      (let [nw (->> (nth anorms i)
                    (m2-mult au))
            ;;Transform face normal into B's model space
            buT (m2-xpose bu)
            n (m2-mult buT nw)
            ;;Retrieve support point from B along -n
            s (getSupport B (v2-negate n))
            ;;Retrieve vertex on face from A, transform into
            ;;B's model space
            v (m2-mult buT
                       (v2-sub (v2-add (->> (nth averts i)
                                            (m2-mult au))
                                       (:pos @bodya))
                               (:pos @bodyb)))
            ;;Compute penetration distance (in B's model space)
            d (v2-dot n (v2-sub s v))
            b? (> d bestD)]
        (recur (inc i)
               (if b? d bestD)
               (if b? i bestIndex)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findIncidentFace "" [refPoly incPoly refIndex]
  (let [{bodyr :body rnorms :normals rverts :vertices} @refPoly
        {bodyi :body inorms :normals iverts :vertices} @incPoly
        vz (n# iverts)
        sz (n# inorms)
        ;;Calculate normal in incident's frame of reference
        refNormal (->> (nth rnorms refIndex)
                       (m2-mult (:u @refPoly)) ;; To world space
                       (m2-mult (m2-xpose (:u @incPoly))))]  ;To incident's model space
    ;;Find most anti-normal face on incident polygon
    (loop [i 0 iFace 0 minDot *pos-inf*]
      (if (>= i sz)
        ;;Assign face vertices for incidentFace
        [(v2-add (m2-mult (:u @incPoly) (nth iverts iFace)) (:pos @bodyi))
         (v2-add (m2-mult (:u @incPoly)
                          (nth iverts (mod (inc iFace) vz))) (:pos @bodyi))]
        (let [dot (v2-dot refNormal (nth inorms i))
              b? (< dot minDot)]
          (recur (inc i)
                 (if b? i iFace) (if b? dot minDot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clip "" [n c face]
  (let [[face0 face1] face
        out (array face0 face1)
        ;;Retrieve distances from each endpoint to the line
        ;;d = ax + by - c
        d1 (- (v2-dot n face0) c)
        d2 (- (v2-dot n face1) c)
        ;;If negative (behind plane) clip
        sp (if (<= d1 0) (do (aset out 0 face0) 1) 0)
        sp' (if (<= d2 0) (do (aset out sp face1) (inc sp) sp))
        ;;If the points are on different sides of the plane
        sp'' (if (< (* d1 d2) 0) ;;less than to ignore -0.0f
               (let [;;Push interesection point
                     alpha (/ d1 (- d1 d2))]
                 (aset out sp' (v2-add face0 (v2-scale (v2-sub face1 face0) alpha)))
                 (inc sp'))
               sp')]
  (assert (not= sp'' 3))
  ;;Assign our new converted values
  [sp'' [(aget out 0) (aget out 1)]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygonToPolygon "" [m B1 B2]
  (swap! m #(assoc % :contacts []))
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
      (polygonToPolygon* m B1 B2
                         [penetrationA faceA] [penetrationB faceB]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygonToPolygon*
  "" [m B1 B2 [penetrationA faceA] [penetrationB faceB]]

  (let [{A :shape} @B1
        {B :shape} @B2
        ;flip Always point from a to b
        [refPoly incPoly refIndex flip?]
        ;;Determine which shape contains reference face
        (if (ec/biasGreater? penetrationA penetrationB )
          [A B faceA false]
          [B A faceB true])
        {bodyr :body} @refPoly
        {bodyi :body} @incPoly
        ;;World space incident face
        incidentFace (findIncidentFace refPoly incPoly refIndex)
        ;;Setup reference face vertices
        v1 (nth (:vertices @refPoly) refIndex)
        refIndex (mod (inc refIndex) (n# (:vertices @refPoly)))
        v2 (nth (:vertices @refPoly) refIndex)
        ;;Transform vertices to world space
        v1 (v2-add (m2-mult (:u @refPoly) v1) (:pos @bodyr))
        v2 (v2-add (m2-mult (:u @refPoly) v2) (:pos @bodyr))
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
        [sp incidentFace]
        (Clip (v2-negate sidePlaneNormal) negSide incidentFace)
        ;;Due to floating point error, possible to not have required points
        quit1? (< sp 2)
        [sp incidentFace]
        (if quit1?
          [sp incidentFace]
          (Clip sidePlaneNormal posSide incidentFace))
        ;Due to floating point error, possible to not have required points
        quit2? (< sp 2)]
    (when-not (or quit1? quit2?)
      ;;Flip
      (swap! m
             #(assoc % :normal
                     (if flip? (v2-negate refFaceNormal) refFaceNormal)))
      ;;Keep points behind reference face
      (let [sep (- (v2-dot refFaceNormal (_1 incidentFace)) refC)]
        (if (<= sep 0)
          (swap! m
                 (fn [{:keys [contacts] :as root}]
                   (assoc root
                          :contacts (conj contacts (_1 incidentFace))
                          :penetration (- sep))))
          (swap! m #(assoc % :penetration 0))))
      (let [sep (- (v2-dot refFaceNormal (_2 incidentFace)) refC)]
        (when (<= sep 0)
          (swap! m
                 (fn [{:keys [penetration contacts] :as root}]
                   (let [cs (conj contacts (_2 incidentFace))
                         cz (n# cs)]
                     (assoc root
                            :contacts cs
                            ;;average penetration
                            :penetration (/ (- penetration sep) cz))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF






