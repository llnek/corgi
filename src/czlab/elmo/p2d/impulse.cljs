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
                            vec2 v2-add v2-sub v2-dot v2-xss]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti computeMass "" (fn [a density] (:type a)))
(defmulti setOrient "" (fn [a radians] (:type a)))
(defmulti copyShape "" (fn [a] (:type a)))
(defmulti initShape "" (fn [a] (:type a)))
(defmulti drawShape "" (fn [a] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Shape "" [e]
  (atom {:type e :body nil :radius 0 :u nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [r]
  (let [c (Shape :circle)]
    (swap! c #(assoc % :radius r)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initShape :circle [c] (computeMass c 1))
(defmethod copyShape :circle [c] (atom @c))
(defmethod computeMass :circle [c density]
  (let [{:keys [body radius]} @c
        r2 (* radius radius)
        bm (* PI r2 density)
        I (* bm r2)]
    (swap! body
           #(assoc %
                   :m bm
                   :im (invert bm)
                   :I I
                   :iI = (invert I))) c))
(defmethod setOrient :circle [c radians] c)
(defmethod drawShape :circle [c ctx] c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" []
  (let [s (Shape :polygon)]
    s))

(defmethod initShape :polygon [p] (computeMass p 1))
(defmethod copyShape :polygon [p] (atom @p))
(defmethod computeMass :polygon [p density]
  (let [{:keys [edges]} @p
        inv3 (invert 3)
        sz (n# edges)
        ;;calculate centroid and moment of interia
        [C M I]
        (loop [i 0 c V2_ZERO area 0 I 0]
          (if (>= i sz)
            [(v2-scale c (invert area)) (* density area) (* density I)]
            (let [{p2 :v1} @(->> (mod (+ 1 i) sz)
                                 (nth edges))
                  {p1 :v1} @(nth edges i)
                  {x1 :x y1 :y} p1
                  {x2 :x y2 :y} p2
                  D (v2-xss p1 p2)
                  ;;triangle vertices, third vertex implied as (0, 0)
                  triArea (* 0.5 D)
                  x' (+ (* x1 x1) (* x2 x1) (* x2 x2))
                  y' (+ (* y1 y1) (* y2 y1) (* y2 y2))]
              ;;use area to weight the centroid average, not just vertex position
              (recur (+ 1 i)
                     (v2-add c (v2-scale (v2-add p1 p2) (* triArea inv3)))
                     (+ area triArea)
                     (+ I (* 0.25f * inv3 * D (+ x' y')))))))]
    (swap! p
           #(assoc %
                   :m M :im (invert M) :I I :iI (invert I)))
    ;;;translate vertices to centroid (make the centroid (0, 0)
    ;;for the polygon in model space)
    ;;Not really necessary
    (loop [i 0 lastE nil]
      (if (>= i sz)
        (swap! lastE #(assoc % :v2 (:v1 @(nth edges 0))))
        (let [e (nth edges i)
              {:keys [v1 v2]} @e
              v1' (v2-sub v1 C)
              e' (if (pos? i) (nth edges (- i 1)))]
          (swap! e #(assoc % :v1 v1'))
          (if (some? e') (swap! e' #(assoc % :v2 v1')))
          (recur (+ 1 i) e))))) p)

(defmethod setOrient :polygon [p radians]
  (let [{:keys [u]} @p]
    (.Set u radians ) p))

(defmethod drawShape :polygon [p] p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setBox "" [p hw hh]
  (swap! p
         #(assoc %
                 :edges [(Point2D -hw -hh)
                         (Point2D hw -hh)
                         (Point2D hw hh)
                         (Point2D -hw hh)]
                 :normal [(vec2 0 -1)
                          (vec2 1 0) (vec2 0 1) (vec2 -1 0)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcFaceNormals "" [p]
  (let [{:keys [vertices]} @p
        ns (transient [])
        sz (n# vertices)]
    (dotimes [i sz]
      (let [i2 (mod (+ 1 i) sz)
            face (v2-sub (nth vertices i2) (nth vertices i))]
        ;;ensure no zero-length edges
        (assert (> (v2-lensq face) (* EPSILON EPSILON)))
        ;;calculate normal with 2D cross product between vector and scalar
        (conj! ns (v2-norm (vec2 (:y face) (- (:x face)))))))
    (swap! p #(assoc % :normals (persistent! ns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set "" [p vertices]
  (let [sz (n# vertices)
        ;;Find the right most point on the hull
        rightMost
        (loop [i 1 rightMost 0 cx (:x (_1 vertices))]
          (if (>= i sz)
            rightMost
            (let [x (:x (nth vertices i))
                  [r x'] (cond (> x cx) [i x]
                               :else
                               (if (and (= x cx)
                                        (< (:y (nth vertices i))
                                           (:y (nth vertices rightMost))))
                                 [i cx]
                                 [rightMost cx]))]
              (recur (inc i) r x'))))
        [numVerts]
        (loop [hull [rightMost] outCount 0 indexHull rightMost loop? true]
          (if-not loop?
            [outCount]
            (let [nextHullIndex
                  (loop [i 1 nextHullIndex 0]
                    (if (>= i sz)
                      nextHullIndex
                      (recur (+ 1 i)
                             (if (= nextHullIndex indexHull)
                               i
                               (let [v' (nth vertices (nth hull outCount))
                                     e1 (v2-sub (nth vertices nextHullIndex) v')
                                     e2 (v2-sub (nth vertices i) v')
                                     c (v2-xss e1 e2)]
                                 (if (or (neg? c)
                                         (and (zero? c)
                                              (> (v2-lensq e2)
                                                 (v2-lensq e1)))) i nextHullIndex))))))]
              (recur hull
                     (+ 1 outCount)
                     nextHullIndex
                     (not= nextHullIndex rightMost)))))
        verts (transient [])]
    (dotimes [i numVerts]
      (conj! verts (nth vertices (nth hull i))))
    (swap! p #(assoc % :vertices (persistent! verts)))
    (calcFaceNormals p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The extreme point along a direction within a polygon
(defn getSupport "" [p dir]
  (let [{:keys [vertices]} @p
        sz (n# vertices)]
    (loop [i 0 proj *neg-inf* bv nil]
      (if (>= i sz)
        bv
        (let [v (nth vertices i)
              p' (v2-dot v dir)
              b? (> p' proj)]
          (recur (inc i) (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn manifold "" []
  (atom {:A nil
         :B nil
         ;;Depth of penetration from collision
         :penetration 0
         ;;From A to B
         :normal nil
         ;;Points of contact during collision
         :contacts []
         ;;Mixed restitution
         :bounce 0
         ;;Mixed dynamic friction
         :df 0
         ;;Mixed static friction
         :sf 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn solveManifold "" [m]
  (let [{:keys [A B]} @m
        {sa :shape} @A
        {sb :shape} @B]
  ;Dispatch[A->shape->GetType( )][B->shape->GetType( )]( this, A, B );
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initManifold "" [m]
  (let [{:keys [contacts A B]} @m
        {ba :bounce posa :pos vela :vel ava :angVel} @A
        {bb :bounce posb :pos velb :vel avb :angVel} @B
        sz (count contacts)
        e
        (loop [i 0 e (min ba bb)]
          (if (>= i sz)
            e
            ;;calculate radii from COM to contact
            (let [c (nth contacts i)
                  ra (v2-sub c posa)
                  rb (v2-sub c posb)
                  rv (v2-sub (v2-sub (v2-add velb
                                             (v2-xss avb rb)) vela)
                             (v2-xss ava ra))]
              ;;Determine if we should perform a resting collision or not
              ;;The idea is if the only thing moving this object is gravity,
              ;;then the collision should be performed without any restitution
              (recur (inc i)
                     (if (< (v2-lensq rv)
                            (+ (v2-lensq (v2-scale gravity dt)) EPSILON)) 0 e)))))]
    (swap! m
           #(assoc %
                   :bounce e
                   :sf (js/Math.sqrt (* (:staticFriction @A)
                                        (:staticFriction @A)))
                   :df (js/Math.sqrt (* (:dynamicFriction @A)
                                        (:dynamicFriction @A))))) m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod applyImpulse :manifold [m]
  ;;Early out and positional correct if both objects have infinite mass
  (let [{:keys [A B]}]
    (if (gx/fuzzyZero? (+ (:im A) (:im B)))
      (infiniteMassCorrection m)
      (applyImpulseOnManifold m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyImpulseOnManifold "" [m]
  (let [{:keys [A B bounce contacts normal]} @m
        {ima :im iia :ii ava :angVel vela :vel posa :pos} @A
        {imb :im iib :ii avb :angVel velb :vel posb :pos} @B
        sz (n# contacts)]
    (loop [ok? true i 0]
      (when (and ok? (< i sz))
        (recur
          (let [c (nth contacts i)
                ii (inc i)
                ;;calculate radii from COM to contact
                ra (v2-sub c posa)
                rb (v2-sub c posb)
                ;;relative velocity
                rv (v2-sub (v2-sub (v2-add velb
                                           (v2-xss avb rb)) vela) (v2-xss ava ra))
                ;;relative velocity along the normal
                contactVel (v2-dot rv normal)]
            ;;do not resolve if velocities are separating
            (if (pos? contactVel)
              false
              (let [raCrossN (v2-xss ra normal)
                    rbCrossN (v2-xss rb normal)
                    invMassSum (+ ima imb (* (sqr* raCrossN ) iia) (* (sqr* rbCrossN ) iib))
                    ;;calculate impulse scalar
                    j (* (- (+ 1 bounce)) contactVel)
                    j' (/ j invMassSum)
                    j'' (/ j' sz)
                    ;;Apply impulse
                    impulse (v2-scale normal j'')]
                (applyImpulse A  (- impulse) ra)
                (applyImpulse B  impulse rb)
                ;;Friction impulse
                (let [rv (v2-sub (v2-sub (v2-add (:vel @B)
                                                 (v2-xss (:angVel @B) rb )) (:vel @A))
                                 (v2-xss (:angVel @A) ra ))
                      t (v2-norm (v2-sub rv (v2-scale normal (v2-dot rv normal))))
                      ;; j tangent magnitude
                      jt (- (v2-dot rv t))
                      jt' (/ jt  invMassSum)
                      jt'' (/ jt' sz)]
                  ;; Don't apply tiny friction impulses
                  (if (gx/fuzzyZero? jt'')
                    false
                    (let [;;coulumb's law
                          tangentImpulse
                          (if (< (abs* jt'') (* j'' (:sf @m)))
                            (v2-scale t jt'')
                            (v2-scale t (* df (- j''))))]
                      ;;Apply friction impulse
                      (applyImpulse A (v2-negate tangentImpulse) ra)
                      (applyImpulse B tangentImpulse rb)
                      true))))))
          (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def k_slop  0.05) ;;Penetration allowance
(def k_percent 0.4);; Penetration percentage to correct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn positionalCorrection "" [m]
  (let [{:keys [A B penetration]} @m
        {posa :pos ima :im} @A
        {posb :pos imb :im} @B
        correction (v2-scale normal
                             (* k_percent
                                (/ (max (- penetration k_slop) 0) (+ ima imb))))]
    (swap! A #(assoc % :pos (v2-sub posa (v2-scale correction ima))))
    (swap! B #(assoc % :pos (v2-add posb (v2-scale correction imb))))
    m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn infiniteMassCorrection "" [m]
  (let [{:keys [A B]} @m]
    (swap! A #(assoc % :vel V2_ZERO))
    (swap! B #(assoc % :vel V2_ZERO)) m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body "" [shape x y]
  (let [body (atom {})]
    (swap! body
           #(assoc %
                   :pos (Point2D x y)
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
                   :staticFriction 0.5
                   :dynamicFriction 0.3
                   :bounce 0.2))
    (swap! shape #(assoc % :body body))
    (initShape shape)
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyForce "" [b f]
  (swap! b (fn [{:keys [force] :as root}]
             (assoc root :force (+ force f)))) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyImpulse "" [b impulse contactVector]
  (let [{:keys [vel im ii angVel]} @b]
    (swap! b
           #(assoc %
                   :vel (v2-add vel (v2-scale impulse im))
                   :angVel (+ angVel
                              (* ii (v2-xss contactVector impulse))))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic "" [b]
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
      (doseq [c contacts] (applyImpulse c)))
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






