;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.impulse

  (:require [czlab.mcfud.p2d.core :as pc :refer [gWorld]]
            [czlab.mcfud.afx.math :as m :refer [EPSILON vec2]]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.core :as c :refer [n# _1 _2
                                                POS-INF NEG-INF]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private dispatch-table (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-orient! "Rotate body." [B radians]
  (let [{:keys [shape]} @B]
    (c/assoc!! B
               :angle radians
               :shape ((:set-angle shape) shape radians)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-circle-angle "" [C radians] C)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-circle "" [C ctx]
  (let [{:keys [angle pos] {:keys [radius]} :shape} @C]
    (gx/draw-circle* pos radius angle ctx true) C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-pos-circle "" [C pt] (c/assoc!! C :pos pt) C)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle "" [r & [options]]
  (-> (g/circle r)
      (assoc :set-angle set-circle-angle)
      (pc/body {:repos set-pos-circle})
      (pc/set-body-attrs! options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-poly-angle
  "" [P radians] (assoc P :u (m/rotation2x2 radians)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-poly "" [P ctx]
  (let [{{:keys [u vertices]} :shape c :pos} @P]
    (-> (mapv #(m/vec-add c (m/mat-vmult u %)) vertices) (gx/draw-poly* ctx)) P))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-pos-poly "" [P pt] (c/assoc!! P :pos pt) P)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygon* "" []
  (pc/body (assoc (g/polygon [])
                  :normals []
                  :u (m/rotation2x2 0)
                  :set-angle set-poly-angle) {:repos set-pos-poly}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygon-box "" [sz & [attrs]]
  (let [{:keys [width height]} sz
        P (polygon*)
        {:keys [shape]} @P
        [hw hh] (c/mapfv / 2 width height)]
    (c/assoc!! P
               :shape
               (assoc shape
                      :vertices [(vec2 (- hw) (- hh)) (vec2 hw (- hh))
                                 (vec2 hw hh) (vec2 (- hw) hh)]
                      :normals [(vec2 0 -1) (vec2 1 0) (vec2 0 1) (vec2 -1 0)]))
    (set-orient! P 0)
    (pc/set-body-attrs! P attrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-face-normals! "" [P]
  (let [{{:keys [vertices] :as S} :shape} @P
        EE (c/sqr* EPSILON)]
    ;;counter-clockwise so normals point out from edge to world
    (loop [i 0 SZ (n# vertices) out []]
      (if (>= i SZ)
        (c/assoc!! P :shape (assoc S :normals out))
        (let [i2 (m/wrap?? i SZ)
              face (m/vec-sub (nth vertices i2)
                              (nth vertices i))]
          (assert (> (m/vec-lensq face) EE))
          (recur (+ 1 i)
                 SZ
                 (conj out (m/vec-unit (m/vec-normal face)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn polygon "" [vertices & [attrs]]
  (let [P (polygon*)
        {:keys [shape]} @P]
    (->> (apply pc/sort?? vertices)
         (assoc shape :vertices)
         (c/assoc!! P :shape))
    (calc-face-normals! P)
    (set-orient! P 0)
    (pc/set-body-attrs! P attrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The extreme point along a direction within a polygon
(defn- find-support-point?? "" [S dir]
  (let [{:keys [vertices]} S]
    (loop [i 0 SZ (n# vertices) proj NEG-INF bv nil]
      (if (>= i SZ)
        bv
        (let [v (nth vertices i)
              p' (m/vec-dot v dir)
              b? (> p' proj)]
          (recur (+ 1 i) SZ (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- apply-impulse-body! "" [B impulse contactVector]
  (let [{:keys [vel im ii gvel]} @B]
    (c/assoc!! B
               :vel (m/vec-add vel (m/vec-scale impulse im))
               :gvel (+ gvel (* ii (m/vec-xss contactVector impulse))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn manifold "" [& [a b]]
  (atom {:type :manifold
         :A (or a nil)
         :B (or b nil)
         ;;depth of penetration from collision
         :penetration 0
         ;;From A to B
         :normal (m/vz2)
         ;;Points of contact during collision
         :contacts []
         ;;Mixed restitution
         :bounce 0
         ;;Mixed dynamic friction
         :dynaF 0
         ;;Mixed static friction
         :statF 0}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- solve-manifold? "" [M]
  (let [{:keys [A B]} @M
        {{sa :type} :shape} @A
        {{sb :type} :shape} @B]
    (((@dispatch-table sa) sb) M A B) (not-empty (:contacts @M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init-manifold! "" [M]
  (let [{:keys [gravity frame-secs]} @gWorld
        {:keys [contacts A B]} @M
        {gvelA :gvel velA :vel posA :pos bounceA :bounce dfA :dynaF sfA :statF} @A
        {gvelB :gvel velB :vel posB :pos bounceB :bounce dfB :dynaF sfB :statF} @B]
    (c/assoc!! M
               :dynaF (c/sqrt* (* dfA dfB))
               :statF (c/sqrt* (* sfA sfB))
               :bounce (min bounceA bounceB))
    (loop [i 0 SZ (n# contacts) E 911]
      (if (or (zero? E)
              (>= i SZ))
        (if (zero? E) (c/assoc!! M :bounce 0))
        ;;calculate radii from COM to contact
        (let [c (nth contacts i)
              ra (m/vec-sub c posA)
              rb (m/vec-sub c posB)
              rv (m/vec-sub (m/vec-sub (m/vec-add velB
                                                  (m/v2-xss* gvelB rb)) velA)
                            (m/v2-xss* gvelA ra))]
          ;;Determine if we should perform a resting collision or not
          ;;The idea is if the only thing moving this object is gravity,
          ;;then the collision should be performed without any restitution
          (recur (+ 1 i)
                 SZ
                 (if (< (m/vec-lensq rv)
                        (+ (m/vec-lensq (m/vec-scale gravity frame-secs)) EPSILON)) 0 E))))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- apply-impulse-on-manifold! "" [M]
  (let [{:keys [A B bounce contacts normal statF dynaF]} @M]
    (loop [i 0 SZ (n# contacts) loop? true]
      (when (and loop? (< i SZ))
        (let [{imA :im iiA :ii gvelA :gvel velA :vel posA :pos} @A
              {imB :im iiB :ii gvelB :gvel velB :vel posB :pos} @B]
          (recur (+ 1 i)
                 SZ
                 (let [c (nth contacts i)
                       ;;calculate radii from COM to contact
                       ra (m/vec-sub c posA)
                       rb (m/vec-sub c posB)
                       ;;relative velocity
                       rv (m/vec-sub (m/vec-sub (m/vec-add velB
                                                           (m/v2-xss* gvelB rb)) velA)
                                  (m/v2-xss* gvelA ra))
                       contactVel (m/vec-dot rv normal)]
                   ;;do not resolve if velocities are separating
                   (if (pos? contactVel)
                     false
                     (let [raCrossN (m/vec-xss ra normal)
                           rbCrossN (m/vec-xss rb normal)
                           invMass (+ imA imB
                                      (* (c/sqr* raCrossN) iiA)
                                      (* (c/sqr* rbCrossN) iiB))
                           ;;calculate impulse scalar
                           j (-> (- (+ 1 bounce))
                                 (* contactVel) (/ invMass) (/ SZ))
                           impulse (m/vec-scale normal j)]
                       ;;Apply impulse
                       (apply-impulse-body! A (m/vec-neg impulse) ra)
                       (apply-impulse-body! B impulse rb)
                       ;;Friction impulse
                       (let [{gvelA :gvel velA :vel} @A
                             {gvelB :gvel velB :vel} @B
                             rv (m/vec-sub (m/vec-sub (m/vec-add velB
                                                                 (m/v2-xss* gvelB rb)) velA)
                                        (m/v2-xss* gvelA ra))
                             T (->> (m/vec-dot rv normal)
                                    (m/vec-scale normal)
                                    (m/vec-sub rv) (m/vec-unit))
                             ;; j tangent magnitude
                             jT (-> (- (m/vec-dot rv T)) (/ invMass) (/ SZ))]
                         ;; Don't apply tiny friction impulses
                         (if (m/fuzzy-zero? jT)
                           false
                           ;;coulumb's law
                           (let [tangentImpulse
                                 (if (< (c/abs* jT) (* j statF))
                                   (m/vec-scale T jT)
                                   (m/vec-scale T (* dynaF (- j))))]
                             ;;Apply friction impulse
                             (apply-impulse-body! A (m/vec-neg tangentImpulse) ra)
                             (apply-impulse-body! B tangentImpulse rb) true))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- apply-impulse! "" [M]
  ;;Early out and positional correct if both objects have infinite mass
  (let [{:keys [A B]} @M
        {imA :im velA :vel} @A
        {imB :im velB :vel} @B]
    (if (m/fuzzy-zero? (+ imA imB))
      ;;infinite mass Correction
      (do (c/assoc!! A :vel (m/vz2))
          (c/assoc!! B :vel (m/vz2)))
      (apply-impulse-on-manifold! M)) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positional-correction! "" [M]
  (let [{:keys [A B normal penetration]} @M
        {posA :pos imA :im} @A
        {posB :pos imB :im} @B
        slop' 0.05 ;;Penetration allowance
        percent' 0.4 ;; Penetration percentage to correct
        jiggle (m/vec-scale normal
                            (* percent'
                               (/ (max (- penetration slop') 0) (+ imA imB))))]
    (c/assoc!! A :pos (m/vec-sub posA (m/vec-scale jiggle imA)))
    (c/assoc!! B :pos (m/vec-add posB (m/vec-scale jiggle imB))) M))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn apply-force "" [B f]
  (swap! B (fn [{:keys [accel] :as root}]
             (assoc root :accel (m/vec-add accel f)))) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrate-forces! "" [B dt]
  (let [{:keys [im ii vel accel gvel torque]} @B
        dt2 (/ dt 2)
        {:keys [gravity]} @gWorld]
    (when-not (zero? im)
      (c/assoc!! B
                 :gvel (+ gvel (* dt2 torque ii))
                 :vel (m/vec-add vel (m/vec-scale (-> gravity
                                                      (m/vec-add (m/vec-scale accel im))) dt2)))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrate-velocity! "" [B dt]
  (let [{:keys [pos im vel gvel angle]} @B
        angle' (+ angle (* dt gvel))]
    (when-not (zero? im)
      (c/assoc!! B
                 :angle angle'
                 :pos (m/vec-add pos (m/vec-scale vel dt)))
      (set-orient! B angle')
      (integrate-forces! B dt)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- step2 "" [manifolds algoIterCount]
  (let [{:keys [samples] dt :frame-secs} @gWorld]
    ;;integrate forces
    (c/each-set samples
                (fn [b _] (integrate-forces! b dt)))
    ;;initialize collisions
    (doseq [c manifolds] (init-manifold! c))
    ;;solve collisions
    (dotimes [_ algoIterCount]
      (doseq [c manifolds] (apply-impulse! c)))
    ;;integrate velocities
    (c/each-set samples
                (fn [b _] (integrate-velocity! b dt)))
    ;;correct positions
    (doseq [c manifolds] (positional-correction! c))
    ;;clear all forces
    (c/each-set samples
                (fn [b _]
                  (c/assoc!! b :accel (m/vz2) :torque 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle->circle "" [M B1 B2]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {radA :radius} A
        {radB :radius} B
        radius (+ radA radB)
        ;;calculate translational vector, which is normal
        normal (m/vec-sub posB posA)
        distSQ (m/vec-lensq normal)]
    (cond
      (>= distSQ (c/sqr* radius))
      ;;Not in contact
      (c/assoc!! M :contacts [])
      :else
      (let [dist (c/sqrt* distSQ)]
        (if (zero? dist)
          (c/assoc!! M
                     :penetration radA
                     :normal (vec2 1 0) :contacts [posA])
          (swap! M
                 #(let [n (m/vec-scale normal (/ 1 dist))]
                    (assoc %
                           :penetration (- radius dist)
                           :normal n
                           :contacts [(m/vec-add (m/vec-scale n radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle->polygon* "" [M B1 B2 center separation faceNormal]
  (let [{posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [normals vertices] bu :u} B
        {radA :radius} A
        ;;grab face's vertices
        v1 (nth vertices faceNormal)
        SZ (n# vertices)
        i2 (m/wrap?? faceNormal SZ)
        v2 (nth vertices i2)]
    ;;check to see if center is within polygon
    (if (< separation EPSILON)
      (swap! M
             #(let [n (m/vec-neg (m/mat-vmult bu
                                              (nth normals faceNormal)))]
                (assoc %
                       :normal n
                       :penetration radA
                       :contacts [(m/vec-add (m/vec-scale n radA) posA)])))
      ;;determine which voronoi region of the edge center of circle lies within
      (let [dot1 (m/vec-dot (m/vec-sub center v1) (m/vec-sub v2 v1))
            dot2 (m/vec-dot (m/vec-sub center v2) (m/vec-sub v1 v2))]
        (c/assoc!! M :penetration (- radA separation))
        (cond
          ;;Closest to v1
          (<= dot1 0)
          (when-not (> (m/vec-distsq center v1) (c/sqr* radA))
            (c/assoc!! M
                       :normal (m/vec-unit (m/mat-vmult bu (m/vec-sub v1 center)))
                       :contacts [(m/vec-add (m/mat-vmult bu v1) posB)]))
          ;;Closest to v2
          (<= dot2 0)
          (when-not (> (m/vec-distsq center v2) (c/sqr* radA))
            (c/assoc!! M
                       :normal (m/vec-unit (m/mat-vmult bu (m/vec-sub v2 center)))
                       :contacts [(m/vec-add (m/mat-vmult bu v2) posB)]))
          ;;Closest to face
          :else
          (let [n (nth normals faceNormal)
                n' (m/vec-neg (m/mat-vmult bu n))]
            (when-not (> (m/vec-dot (m/vec-sub center v1) n) radA)
              (c/assoc!! M
                         :normal n'
                         :contacts [(m/vec-add (m/vec-scale n' radA) posA)]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle->polygon "" [M B1 B2]
  (let [_ (c/assoc!! M :contacts [])
        {posA :pos A :shape} @B1
        {posB :pos B :shape} @B2
        {:keys [vertices normals] bu :u} B
        {radA :radius} A
        ;;Transform circle center to Polygon model space
        center (m/mat-vmult (m/mat-xpose bu)
                            (m/vec-sub posA posB))]
    ;;Find edge with minimum penetration
    ;;Exact concept as using support points in Polygon vs Polygon
    (loop [i 0 SZ (n# vertices)
           sep NEG-INF n 0 break? false]
      (if (or break? (>= i SZ))
        (if-not break? (circle->polygon* M B1 B2 center sep n))
        (let [s (m/vec-dot (nth normals i)
                           (m/vec-sub center (nth vertices i)))
              [s' n' t']
              (cond (> s radA) [sep n true]
                    (> s sep) [s i break?]
                    :else [sep n break?])] (recur (+ 1 i) SZ s' n' t'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygon->circle "" [M B1 B2]
  (circle->polygon M B2 B1)
  (c/assoc!! M :normal (m/vec-neg (:normal @M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- find-axis-least-penetration "" [A B]
  (let [{bA :body au :u :keys [vertices normals]} A
        {bB :body bu :u} B
        {posA :pos} @bA
        {posB :pos} @bB]
  (loop [i 0 SZ (n# vertices) bestD NEG-INF bestIndex 0]
    (if (>= i SZ)
      [bestD bestIndex]
      (let [nw (m/mat-vmult au (nth normals i))
            ;;Transform face normal into B's model space
            buT (m/mat-xpose bu)
            n (m/mat-vmult buT nw)
            ;;Retrieve support point from B along -n
            s (find-support-point?? B (m/vec-neg n))
            ;;Retrieve vertex on face from A, transform into
            ;;B's model space
            v (m/mat-vmult buT
                           (m/vec-sub (m/vec-add (->> (nth vertices i)
                                                      (m/mat-vmult au)) posA) posB))
            ;;Compute penetration distance (in B's model space)
            d (m/vec-dot n (m/vec-sub s v))
            b? (> d bestD)]
        (recur (+ 1 i)
               SZ
               (if b? d bestD)
               (if b? i bestIndex)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- find-incident-face?? "" [refPoly incPoly refIndex]
  (let [{bI :body iu :u :keys [normals vertices]} incPoly
        {posI :pos} @bI
        ;;Calculate normal in incident's frame of reference
        refNormal (->> (nth (:normals refPoly) refIndex)
                       (m/mat-vmult (:u refPoly)) ;; To world space
                       (m/mat-vmult (m/mat-xpose iu)))]  ;To incident's model space
    ;;Find most anti-normal face on incident polygon
    (loop [i 0 SZ (n# vertices)
           iFace 0 minDot POS-INF]
      (if (>= i SZ)
        ;;Assign face vertices for incidentFace
        [(m/vec-add (m/mat-vmult iu (nth vertices iFace)) posI)
         (m/vec-add (m/mat-vmult iu
                                 (nth vertices (m/wrap?? iFace SZ))) posI)]
        ;loop
        (let [dot (m/vec-dot refNormal (nth normals i))
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
        d1 (- (m/vec-dot n face0) c)
        d2 (- (m/vec-dot n face1) c)
        sp 0
        ;;If negative (behind plane) clip
        sp (if (<= d1 0) (do (aset out sp face0) (+ 1 sp)) sp)
        sp (if (<= d2 0) (do (aset out sp face1) (+ 1 sp)) sp)
        ;;If the points are on different sides of the plane
        sp (if (< (* d1 d2) 0) ;;less than to ignore -0.0f
             (let [;;Push interesection point
                   alpha (/ d1 (- d1 d2))]
                 (aset out sp (m/vec-add face0
                                         (m/vec-scale (m/vec-sub face1
                                                                 face0) alpha)))
                 (+ 1 sp)) sp)]
  (assert (not= sp 3))
  ;;Assign our new converted values
  [sp [(aget out 0) (aget out 1)]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polygon->polygon*
  "" [M B1 B2 [penetrationA faceA] [penetrationB faceB]]
  (let [{A :shape} @B1
        {B :shape} @B2
        ;flip Always point from a to b
        [refPoly incPoly refIndex flip?]
        ;;Determine which shape contains reference face
        (if (m/bias-greater? penetrationA penetrationB)
          [A B faceA false]
          [B A faceB true])
        {bR :body} refPoly
        {bI :body} incPoly
        {posR :pos} @bR
        ;;World space incident face
        incidentFaces (find-incident-face?? refPoly incPoly refIndex)
        {rverts :vertices ru :u} refPoly
        ;;Setup reference face vertices
        v1 (nth rverts refIndex)
        refIndex (m/wrap?? refIndex (n# rverts))
        v2 (nth rverts refIndex)
        ;;Transform vertices to world space
        v1 (m/vec-add (m/mat-vmult ru v1) posR)
        v2 (m/vec-add (m/mat-vmult ru v2) posR)
        ;;Calculate reference face side normal in world space
        sidePlaneNormal (m/vec-unit (m/vec-sub v2 v1))
        ;;Orthogonalize
        refFaceNormal (vec2 (_2 sidePlaneNormal) (- (_1 sidePlaneNormal)))
        ;;ax + by = c
        ;; c is distance from origin
        refC (m/vec-dot refFaceNormal v1)
        negSide (- (m/vec-dot sidePlaneNormal v1))
        posSide (m/vec-dot sidePlaneNormal v2)
        ;;Clip incident face to reference face side planes
        [sp incidentFaces]
        (clip?? (m/vec-neg sidePlaneNormal) negSide incidentFaces)
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
      (c/assoc!! M
                 :normal
                 (if flip? (m/vec-neg refFaceNormal) refFaceNormal))
      ;;Keep points behind reference face
      (let [sep (- (m/vec-dot refFaceNormal (_1 incidentFaces)) refC)]
        (if (<= sep 0)
          (c/assoc!! M
                     :contacts [(_1 incidentFaces)]
                     :penetration (- sep))
          (c/assoc!! M :penetration 0)))
      (let [sep (- (m/vec-dot refFaceNormal (_2 incidentFaces)) refC)]
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
(defn- polygon->polygon "" [M B1 B2]
  (let [_ (c/assoc!! M :contacts [])
        {A :shape} @B1
        {B :shape} @B2
        ;;Check for a separating axis with A's face planes
        [penetrationA faceA] (find-axis-least-penetration A B)
        skipA? (>= penetrationA 0)
        ;;Check for a separating axis with B's face planes
        [penetrationB faceB] (if skipA?
                               [1 1] ;; hack to skip
                               (find-axis-least-penetration B A))
        skipB? (>= penetrationB 0)]
    (when-not (or skipA? skipB?)
      (polygon->polygon* M B1 B2
                         [penetrationA faceA] [penetrationB faceB]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(reset! dispatch-table
        {:circle {:circle circle->circle :polygon circle->polygon}
         :polygon {:circle polygon->circle :polygon polygon->polygon}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-algo "" [algoIterCount posCorrection]
  (let [{:keys [frame-secs samples]} @gWorld]
    (loop [i 0 SZ (c/count-set samples) ms []]
      (if (>= i SZ)
        (step2 ms algoIterCount)
        (let [B1 (c/nth-set samples i)]
          (recur (+ 1 i)
                 SZ
                 (loop [j (+ i 1) ms' ms]
                   (if (>= j SZ)
                     ms'
                     (let [B2 (c/nth-set samples j)
                           m (manifold B1 B2)]
                       (recur (+ 1 j)
                              (if (and (or (pc/dynamic? B1)
                                           (pc/dynamic? B2))
                                       (solve-manifold? m)) (conj ms' m) ms')))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-body "" [B & args]
  (let [{:keys [shape]} @B
        {:keys [type]} shape]
    (cond
      (= :polygon type) (apply draw-poly B args)
      (= :circle type) (apply draw-circle B args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init-physics
  "" [gravity fps world & [options]]
  (pc/init gravity
           fps
           world
           (merge {:body-drawer draw-body :algo-runner run-algo} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

