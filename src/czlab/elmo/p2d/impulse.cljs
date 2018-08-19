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
                    :as ec :refer [do->false do->true
                                   assoc!! half* _1 _2 n# do-with]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [abs* sqr* sqrt* fuzzyZero?
                            num?? *pos-inf* *neg-inf*
                            invert EPSILON fuzzyEqual?]]
            [czlab.elmo.p2d.core
             :as pc :refer [Body *gWorld* dynamic? static?]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [Point2D polyDraw*]]
            [czlab.elmo.afx.math
             :as ma :refer [PI TWO-PI vec-zero wrap??
                            mat2 rotation2x2 mat-vmult mat-xpose
                            vec-normal
                            vec-neg vec-unit vec-scale
                            vec-len vec-lensq vec-dist vec-distsq
                            vec2 vec-add vec-sub vec-dot vec-xss v2-xss*]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *dispatch* (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setOrient! "" [B radians]
  (let [{:keys [shape]} @B]
    (assoc!! B
             :angle radians
             :shape ((:setAngle shape) shape radians)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setCircleAngle "" [C radians] C)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawCircle "" [C ctx]
  (let [{:keys [angle pos] {:keys [radius]} :shape} @C]
    (gx/circleDraw* pos radius angle ctx true) C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setPosCircle "" [C pt & more] (assoc!! C :pos pt) C)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [r & [options]]
  (-> (Body (assoc (gx/Circle r)
                   :setAngle setCircleAngle)
            {:repos setPosCircle}) (pc/setBodyAttrs! options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setPolyAngle
  "" [P radians] (assoc P :u (rotation2x2 radians)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawPoly "" [P ctx]
  (let [{{:keys [u vertices]} :shape c :pos} @P]
    (-> (mapv #(vec-add c (mat-vmult u %)) vertices) (polyDraw* ctx)) P))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setPosPoly "" [P pt & more] (assoc!! P :pos pt) P)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygon* "" []
  (Body (assoc (gx/Polygon [])
               :normals []
               :u (rotation2x2 0)
               :setAngle setPolyAngle) {:repos setPosPoly}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn PolygonBox "" [sz & [attrs]]
  (let [{:keys [width height]} sz
        P (polygon*)
        {:keys [shape]} @P
        hw (half* width)
        hh (half* height)]
    (assoc!! P
             :shape
             (assoc shape
                    :vertices [(Point2D (- hw) (- hh)) (Point2D hw (- hh))
                               (Point2D hw hh) (Point2D (- hw) hh)]
                    :normals [(vec2 0 -1) (vec2 1 0) (vec2 0 1) (vec2 -1 0)]))
    (setOrient! P 0)
    (pc/setBodyAttrs! P attrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcFaceNormals! "" [P]
  (let [{{:keys [vertices] :as S} :shape} @P
        EE (sqr* EPSILON)]
    ;;counter-clockwise so normals point out from edge to world
    (loop [i 0 SZ (n# vertices) out []]
      (if (>= i SZ)
        (do (assoc!! P :shape (assoc S :normals out)) P)
        (let [i2 (wrap?? i SZ)
              face (vec-sub (nth vertices i2)
                            (nth vertices i))]
          (assert (> (vec-lensq face) EE))
          (recur (+ 1 i)
                 SZ
                 (conj out (vec-unit (vec-normal face)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" [vertices & [attrs]]
  (let [P (polygon*)
        {:keys [shape]} @P]
    (->> (apply pc/sort?? vertices)
         (assoc shape :vertices)
         (assoc!! P :shape))
    (calcFaceNormals! P)
    (setOrient! P 0)
    (pc/setBodyAttrs! P attrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The extreme point along a direction within a polygon
(defn- findSupportPoint?? "" [S dir]
  (let [{:keys [vertices]} S]
    (loop [i 0 SZ (n# vertices) proj *neg-inf* bv nil]
      (if (>= i SZ)
        bv
        (let [v (nth vertices i)
              p' (vec-dot v dir)
              b? (> p' proj)]
          (recur (inc i) SZ (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyImpulseBody! "" [B impulse contactVector]
  (let [{:keys [vel im ii gvel]} @B]
    (assoc!! B
             :vel (vec-add vel (vec-scale impulse im))
             :gvel (+ gvel (* ii (vec-xss contactVector impulse)))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn manifold "" [& [a b]]
  (atom {:type :manifold
         :A (or a nil)
         :B (or b nil)
         ;;depth of penetration from collision
         :penetration 0
         ;;From A to B
         :normal (vec-zero 0)
         ;;Points of contact during collision
         :contacts []
         ;;Mixed restitution
         :bounce 0
         ;;Mixed dynamic friction
         :dynaF 0
         ;;Mixed static friction
         :statF 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- solveManifold? "" [M]
  (let [{:keys [A B]} @M
        {sa :shape} @A
        {sb :shape} @B]
    ((get (get @*dispatch* (:type sa)) (:type sb)) M A B)
    (not-empty (:contacts @M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initManifold! "" [M]
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
              ra (vec-sub c posA)
              rb (vec-sub c posB)
              rv (vec-sub (vec-sub (vec-add velB
                                            (v2-xss* gvelB rb)) velA)
                         (v2-xss* gvelA ra))]
          ;;Determine if we should perform a resting collision or not
          ;;The idea is if the only thing moving this object is gravity,
          ;;then the collision should be performed without any restitution
          (recur (+ 1 i)
                 SZ
                 (if (< (vec-lensq rv)
                        (+ (vec-lensq (vec-scale gravity frameSecs)) EPSILON)) 0 E))))) M))

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
                       ra (vec-sub c posA)
                       rb (vec-sub c posB)
                       ;;relative velocity
                       rv (vec-sub (vec-sub (vec-add velB
                                                     (v2-xss* gvelB rb)) velA)
                                  (v2-xss* gvelA ra))
                       contactVel (vec-dot rv normal)]
                   ;;do not resolve if velocities are separating
                   (if (pos? contactVel)
                     false
                     (let [raCrossN (vec-xss ra normal)
                           rbCrossN (vec-xss rb normal)
                           invMass (+ imA imB
                                      (* (sqr* raCrossN) iiA)
                                      (* (sqr* rbCrossN) iiB))
                           ;;calculate impulse scalar
                           j (-> (- (+ 1 bounce))
                                 (* contactVel) (/ invMass) (/ SZ))
                           impulse (vec-scale normal j)]
                       ;;Apply impulse
                       (applyImpulseBody! A (vec-neg impulse) ra)
                       (applyImpulseBody! B impulse rb)
                       ;;Friction impulse
                       (let [{gvelA :gvel velA :vel} @A
                             {gvelB :gvel velB :vel} @B
                             rv (vec-sub (vec-sub (vec-add velB
                                                           (v2-xss* gvelB rb)) velA)
                                        (v2-xss* gvelA ra))
                             T (->> (vec-dot rv normal)
                                    (vec-scale normal)
                                    (vec-sub rv) (vec-unit))
                             ;; j tangent magnitude
                             jT (-> (- (vec-dot rv T)) (/ invMass) (/ SZ))]
                         ;; Don't apply tiny friction impulses
                         (if (ec/fuzzyZero? jT)
                           false
                           ;;coulumb's law
                           (let [tangentImpulse
                                 (if (< (abs* jT) (* j statF))
                                   (vec-scale T jT)
                                   (vec-scale T (* dynaF (- j))))]
                             ;;Apply friction impulse
                             (applyImpulseBody! A (vec-neg tangentImpulse) ra)
                             (applyImpulseBody! B tangentImpulse rb) true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- applyImpulse! "" [M]
  ;;Early out and positional correct if both objects have infinite mass
  (let [{:keys [A B]} @M
        {imA :im velA :vel} @A
        {imB :im velB :vel} @B]
    (if (ec/fuzzyZero? (+ imA imB))
      ;;infinite mass Correction
      (do (assoc!! A :vel (vec-zero 2))
          (assoc!! B :vel (vec-zero 2)))
      (applyImpulseOnManifold! M)) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positionalCorrection! "" [M]
  (let [{:keys [A B normal penetration]} @M
        {posA :pos imA :im} @A
        {posB :pos imB :im} @B
        slop' 0.05 ;;Penetration allowance
        percent' 0.4 ;; Penetration percentage to correct
        jiggle (vec-scale normal
                          (* percent'
                             (/ (max (- penetration slop') 0) (+ imA imB))))]
    (assoc!! A :pos (vec-sub posA (vec-scale jiggle imA)))
    (assoc!! B :pos (vec-add posB (vec-scale jiggle imB))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyForce "" [B f]
  (swap! B (fn [{:keys [accel] :as root}]
             (assoc root :accel (vec-add accel f)))) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateForces! "" [B dt]
  (let [{:keys [im ii vel accel gvel torque]} @B
        dt2 (half* dt)
        {:keys [gravity]} @*gWorld*]
    (when-not (zero? im)
      (assoc!! B
               :gvel (+ gvel (* dt2 torque ii))
               :vel (vec-add vel (vec-scale (-> gravity
                                                (vec-add (vec-scale accel im))) dt2)))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateVelocity! "" [B dt]
  (let [{:keys [pos im vel gvel angle]} @B
        angle' (+ angle (* dt gvel))]
    (when-not (zero? im)
      (assoc!! B
               :angle angle'
               :pos (vec-add pos (vec-scale vel dt)))
      (setOrient! B angle')
      (integrateForces! B dt)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- step2 "" [manifolds algoIterCount]
  (let [{:keys [samples] dt :frameSecs} @*gWorld*]
    ;;integrate forces
    (ec/eachStore samples
                  (fn [b _] (integrateForces! b dt)))
    ;;initialize collisions
    (doseq [c manifolds] (initManifold! c))
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
                    (assoc!! b :accel (vec-zero 2) :torque 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToCircle "" [M B1 B2]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {radA :radius} A
        {radB :radius} B
        radius (+ radA radB)
        ;;calculate translational vector, which is normal
        normal (vec-sub posB posA)
        distSQ (vec-lensq normal)]
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
                 #(let [n (vec-scale normal (/ 1 dist))]
                    (assoc %
                           :penetration (- radius dist)
                           :normal n
                           :contacts [(vec-add (vec-scale n radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToPolygon* "" [M B1 B2 center separation faceNormal]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [normals vertices] bu :u} B
        {radA :radius} A
        ;;grab face's vertices
        v1 (nth vertices faceNormal)
        SZ (n# vertices)
        i2 (wrap?? faceNormal SZ)
        v2 (nth vertices i2)]
    ;;check to see if center is within polygon
    (if (< separation EPSILON)
      (swap! M
             #(let [n (vec-neg (mat-vmult bu
                                          (nth normals faceNormal)))]
                (assoc %
                       :normal n
                       :penetration radA
                       :contacts [(vec-add (vec-scale n radA) posA)])))
      ;;determine which voronoi region of the edge center of circle lies within
      (let [dot1 (vec-dot (vec-sub center v1) (vec-sub v2 v1))
            dot2 (vec-dot (vec-sub center v2) (vec-sub v1 v2))]
        (assoc!! M :penetration (- radA separation))
        (cond
          ;;Closest to v1
          (<= dot1 0)
          (when-not (> (vec-distsq center v1) (sqr* radA))
            (assoc!! M
                     :normal (vec-unit (mat-vmult bu (vec-sub v1 center)))
                     :contacts [(vec-add (mat-vmult bu v1) posB)]))
          ;;Closest to v2
          (<= dot2 0)
          (when-not (> (vec-distsq center v2) (sqr* radA))
            (assoc!! M
                     :normal (vec-unit (mat-vmult bu (vec-sub v2 center)))
                     :contacts [(vec-add (mat-vmult bu v2) posB)]))
          ;;Closest to face
          :else
          (let [n (nth normals faceNormal)
                n' (vec-neg (mat-vmult bu n))]
            (when-not (> (vec-dot (vec-sub center v1) n) radA)
              (assoc!! M
                       :normal n'
                       :contacts [(vec-add (vec-scale n' radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleToPolygon "" [M B1 B2]
  (let [_ (assoc!! M :contacts [])
        {posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [vertices normals] bu :u} B
        {radA :radius} A
        ;;Transform circle center to Polygon model space
        center (mat-vmult (mat-xpose bu)
                          (vec-sub posA posB))]
    ;;Find edge with minimum penetration
    ;;Exact concept as using support points in Polygon vs Polygon
    (loop [i 0 SZ (n# vertices)
           sep *neg-inf* n 0 break? false]
      (if (or break? (>= i SZ))
        (if-not break? (circleToPolygon* M B1 B2 center sep n))
        (let [s (vec-dot (nth normals i)
                         (vec-sub center (nth vertices i)))
              [s' n' t']
              (cond (> s radA) [sep n true]
                    (> s sep) [s i break?]
                    :else [sep n break?])] (recur (+ 1 i) SZ s' n' t'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygonToCircle "" [M B1 B2]
  (circleToPolygon M B2 B1)
  (assoc!! M :normal (vec-neg (:normal @M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findAxisLeastPenetration "" [A B]
  (let [{bA :body au :u :keys [vertices normals]} A
        {bB :body bu :u} B
        {posA :pos} @bA
        {posB :pos} @bB]
  (loop [i 0 SZ (n# vertices) bestD *neg-inf* bestIndex 0]
    (if (>= i SZ)
      [bestD bestIndex]
      (let [nw (mat-vmult au (nth normals i))
            ;;Transform face normal into B's model space
            buT (mat-xpose bu)
            n (mat-vmult buT nw)
            ;;Retrieve support point from B along -n
            s (findSupportPoint?? B (vec-neg n))
            ;;Retrieve vertex on face from A, transform into
            ;;B's model space
            v (mat-vmult buT
                         (vec-sub (vec-add (->> (nth vertices i)
                                                (mat-vmult au)) posA) posB))
            ;;Compute penetration distance (in B's model space)
            d (vec-dot n (vec-sub s v))
            b? (> d bestD)]
        (recur (+ 1 i)
               SZ
               (if b? d bestD)
               (if b? i bestIndex)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findIncidentFace?? "" [refPoly incPoly refIndex]
  (let [{bI :body iu :u :keys [normals vertices]} incPoly
        {posI :pos} @bI
        ;;Calculate normal in incident's frame of reference
        refNormal (->> (nth (:normals refPoly) refIndex)
                       (mat-vmult (:u refPoly)) ;; To world space
                       (mat-vmult (mat-xpose iu)))]  ;To incident's model space
    ;;Find most anti-normal face on incident polygon
    (loop [i 0 SZ (n# vertices)
           iFace 0 minDot *pos-inf*]
      (if (>= i SZ)
        ;;Assign face vertices for incidentFace
        [(vec-add (mat-vmult iu (nth vertices iFace)) posI)
         (vec-add (mat-vmult iu
                             (nth vertices (wrap?? iFace SZ))) posI)]
        ;loop
        (let [dot (vec-dot refNormal (nth normals i))
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
        d1 (- (vec-dot n face0) c)
        d2 (- (vec-dot n face1) c)
        sp 0
        ;;If negative (behind plane) clip
        sp (if (<= d1 0) (do (aset out sp face0) (inc sp)) sp)
        sp (if (<= d2 0) (do (aset out sp face1) (inc sp)) sp)
        ;;If the points are on different sides of the plane
        sp (if (< (* d1 d2) 0) ;;less than to ignore -0.0f
             (let [;;Push interesection point
                   alpha (/ d1 (- d1 d2))]
                 (aset out sp (vec-add face0 (vec-scale (vec-sub face1 face0) alpha)))
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
        (if (ma/biasGreater? penetrationA penetrationB)
          [A B faceA false]
          [B A faceB true])
        {bR :body} refPoly
        {bI :body} incPoly
        {posR :pos} @bR
        ;;World space incident face
        incidentFaces (findIncidentFace?? refPoly incPoly refIndex)
        {rverts :vertices ru :u} refPoly
        ;;Setup reference face vertices
        v1 (nth rverts refIndex)
        refIndex (wrap?? refIndex (n# rverts))
        v2 (nth rverts refIndex)
        ;;Transform vertices to world space
        v1 (vec-add (mat-vmult ru v1) posR)
        v2 (vec-add (mat-vmult ru v2) posR)
        ;;Calculate reference face side normal in world space
        sidePlaneNormal (vec-unit (vec-sub v2 v1))
        ;;Orthogonalize
        refFaceNormal (vec2 (_2 sidePlaneNormal) (- (_1 sidePlaneNormal)))
        ;;ax + by = c
        ;; c is distance from origin
        refC (vec-dot refFaceNormal v1)
        negSide (- (vec-dot sidePlaneNormal v1))
        posSide (vec-dot sidePlaneNormal v2)
        ;;Clip incident face to reference face side planes
        [sp incidentFaces]
        (clip?? (vec-neg sidePlaneNormal) negSide incidentFaces)
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
               (if flip? (vec-neg refFaceNormal) refFaceNormal))
      ;;Keep points behind reference face
      (let [sep (- (vec-dot refFaceNormal (_1 incidentFaces)) refC)]
        (if (<= sep 0)
          (assoc!! M
                   :contacts [(_1 incidentFaces)]
                   :penetration (- sep))
          (assoc!! M :penetration 0)))
      (let [sep (- (vec-dot refFaceNormal (_2 incidentFaces)) refC)]
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
  (let [_ (assoc!! M :contacts [])
        {A :shape} @B1
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
(defn runAlgo "" [algoIterCount posCorrection]
  (let [{:keys [frameSecs samples]} @*gWorld*]
    (loop [i 0 SZ (ec/countStore samples) ms []]
      (if (>= i SZ)
        (step2 ms algoIterCount)
        (let [B1 (ec/nthStore samples i)]
          (recur (+ 1 i)
                 SZ
                 (loop [j (+ i 1) ms' ms]
                   (if (>= j SZ)
                     ms'
                     (let [B2 (ec/nthStore samples j)
                           m (manifold B1 B2)]
                       (recur (+ 1 j)
                              (if (and (or (dynamic? B1)
                                           (dynamic? B2))
                                       (solveManifold? m))
                                (conj ms' m) ms')))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawBody "" [& args]
  (let [B (_1 args)
        {:keys [shape]} @B
        {:keys [type]} shape]
    (cond
      (= :polygon type) (apply drawPoly args)
      (= :circle type) (apply drawCircle args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (pc/initPhysics gravity
                  fps
                  world
                  (merge {:bodyDrawer drawBody :algoRunner runAlgo} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF





