;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.physics2d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pythagSQ "" [x y] (+ (* x x) (* y y)))
(defn- pythag "" [x y] (js/Math.sqrt (pythagSQ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- inv! "" [x] (if (zero? x) 0 (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gravity* (vec2 0 -20))
(def VEC_ZERO (vec2 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-len "" [v] (pythag (:x v) (:y v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-add "" [v1 v2]
  (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sub "" [v1 v2]
  (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-scale
  "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dot "" [v1 v2]
  (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-cross "" [v1 v2]
  (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-rot "rotate counter-clockwise" [v1 center angle]
  (let [{cx :x cy :y} center
        cos (js/Math.cos angle)
        sin (js/Math.sin angle)
        x (- (:x v1) cx) y (- (:y v1) cy)]
    (vec2
      (+ cx (- (* x cos) (* y sin)))
      (+ cy (+ (* x sin) (* y cos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-norm "" [v1]
  (let [len (inv! (v2-len v1))]
    (vec2 (* len (:x v1)) (* len (:y v1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dist "" [v1 v2]
  (pythag (- (:x v1) (:x v2))
          (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionInfo "" []
  (atom {:depth 0 :normal VEC_ZERO :start VEC_ZERO :End VEC_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setCollisionInfo! "" [ci d n s]
  (swap! ci
         #(assoc %
                 :depth d
                 :normal n
                 :start s
                 :end (v2-add s (v2-scale n d)))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- changeCollisionDir "" [ci]
  (swap! ci
         (fn [{:keys [start end normal] :as root}]
           (assoc root
                  :start end :end start
                  :normal (v2-scale normal -1)))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- RigidShape "" [center mass friction restitution]
  (let [mass' (if (number? mass) mass 1)]
    (atom {:center center
           :inertia 0
           :sticky (if (number? friction) friction 0.8)
           :bounce (if (number? restitution) restitution 0.2)
           :velocity VEC_ZERO
           :invMass (inv! mass')
           :accel (if (zero? mass') VEC_ZERO *gravity*)
           :angle 0
           :angVel 0 ;; clockwise = negative
           :angAccel 0
           :bxRadius 0})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionTest "" [s1 s2 ci] ((:collisionTest s1) s1 s2 ci))
(defn updateInertia! "" [s] ((:updateInertia @s) s) s)
(defn move! "" [s p] ((:move @s) s p) s)
(defn rotate! "" [s v] ((:rotate @s) s v) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass! "" [s delta]
  (let [{:keys [invMass]} @s
        mass (+ (inv! invMass) delta)]
    (if (pos? mass)
      (swap! s #(assoc % :invMass (inv! mass) :accel *gravity*))
      (swap! s #(assoc %
                       :invMass 0
                       :vel VEC_ZERO
                       :accel VEC_ZERO
                       :angVel 0
                       :angAccel 0)))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateShape! "" [world s dt]
  (let [{:keys [top right bottom left height width samples]} @world]
    ;; v = v + a*t
    (swap! s (fn [{:keys [vel accel] :as root}]
               (assoc root :vel (v2-add vel (v2-scale accel dt)))))
    ;;s = x + v*t
    (move! s (v2-scale (:vel @s) dt))

    (swap! s (fn [{:keys [angVel angAccel] :as root}]
               (assoc root :angVel (+ angVel (* angAccel dt)))))
    (rotate! s (* (:angVel @s) dt))

    (let [{cx :x cy :y} (:center @s)]
      (when (or (< cx 0)
                (> cx width)
                (< cy 0)
                (> cy height))
        (let [pos (.indexOf samples s)]
          (if-not (neg? pos) (.splice samples pos 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boundTest? "true if overlapping" [s1 s2]
  (let [{c1 :center r1 :bxRadius} @s1
        {c2 :center r2 :bxRadius} @s2
        v1to2 (v2-sub c2 c1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- supportInfo "" [] (atom {:supportPt nil :supportPtDist 0}))
(def *tmpSupport* (supportInfo))
(defn- supportInfo* "" [d p] (atom {:supportPt p :supportPtDist d}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findSupportPoint "" [r1 dir ptOnEdge]
  (let [{:keys [vertices]} @r1
        len (count vertices)]
    (loop [supportPointDist -9999999 supportPoint nil i 0]
      (if (>= i len)
        (supportInfo* supportPointDist supportPoint)
        (let [v' (nth vertices i)
              ii (+ 1 i)
              proj (v2-dot (v2-sub v' ptOnEdge) dir)]
          ;;find the longest distance with certain edge
          ;;dir is -n direction, so the distance should be positive
          (if (and (pos? proj)
                   (> proj supportPointDist))
            (recur proj v' ii)
            (recur supportPointDist supportPoint ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findAxisLeastPenetration
  "Find the shortest axis that overlapping" [r1 r2 ci]
  (let [{:keys [vertices normals]} @r1
        len (count normals)
        [supportPoint bestDist bestIndex hasSupport?]
        (loop [suPt nil bestDist 999999
               bestIdx nil hasSupport? true i 0]
          (if-not (and hasSupport? (< i len))
            [suPt bestDist bestIdx hasSupport?]
            (let [n (nth normals i)
                  ii (+ i 1)
                  ;;use -n as direction and the vectex on edge i as point on edge
                  dir (v2-scale n -1)
                  ptOnEdge (nth vertices i)
                  ;;find the support on B, the point has longest distance with edge i
                  {:keys [supportPt supportPtDist]}
                  (findSupportPoint r2  dir ptOnEdge)
                  sp? (not (nil? supportPt))]
          ;;get the shortest support point depth
          (if (and sp? (< supportPtDist bestDist))
            (recur supportPt supportPtDist i sp? ii)
            (recur suPt bestDist bestIdx sp? ii)))))]
    (when hasSupport? ;;all four directions have support points
      (let [bn (nth normals bestIndex)
            bv (v2-scale bn bestDist)]
        (setCollisionInfo! ci bestDist bn (v2-add supportPoint bv)))) hasSupport?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollidedRectRect
  "Check for collision between RigidRectangle and RigidRectangle"
  [r1 r2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (collisionInfo)
        ci_2 (collisionInfo)
        p1? (findAxisLeastPenetration r1 r2 ci_1)
        p2? (if p1? (findAxisLeastPenetration r2 r1 ci_2))]
    (when p2?
      ;;if both of rectangles are overlapping,
      ;;choose the shorter normal as the normal
      (let [{d1 :depth n1 :normal s1 :start} @ci_1
            {d2 :depth n2 :normal s2 :start} @ci_2]
        (if (< d1 d2)
          (setCollisionInfo! ci d1 n1
                             (v2-sub s1 (v2-scale n1 d1)))
          (setCollisionInfo! ci d2
                             (v2-scale n2 -1) s2))))
    (and p1? p2?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleInsideRect "" [r1 cc1 ci nEdge bestDist]
  (let [{:keys [normals vertices]} @r1
        {:keys [radius center]} @cc1
        n (nth normals nEdge)
        rVec (v2-scale n (:radius @cc1))]
    (setCollisionInfo! ci
                       (- radius bestDist)
                       n
                       (v2-sub center rVec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findNFaceToCircle "" [r1 cc1 ci]
  (let [{:keys [vertices normals]} @r1
        {:keys [center]} @cc1
        len (count normals)]
    (loop [inside? true
           bestDist -99999 nEdge 0 i 0]
      (if (or (not inside?) (>= i len))
        {:inside? inside?
         :bestDist bestDist
         :nEdge nEdge}
        (let [v (v2-sub center (nth vertices i))
              ii (+ i 1)
              proj (v2-dot v (nth normals i))]
          (cond
            ;;the center of circle is outside of rectangle
            (pos? proj) (recur false proj i ii)
            (> proj bestDist) (recur inside? proj i ii)
            :else (recur inside? bestDist nEdge ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleOutsideRect "" [r1 cc1 ci nEdge bestDist]
  (let [{:keys [normals vertices]} @r1
        {:keys [radius center]} @cc1
        vn (nth vertices nEdge)
        en (nth normals nEdge)
        ;;V1 is from left vertex of face to center of circle
        ;;V2 is from left vertex of face to right vertex of face
        len (count normals)
        eX (mod (+ 1 nEdge) len)
        vX (nth vertices eX)
        V1 (v2-sub center vn)
        V2 (v2-sub vX vn)
        dot (v2-dot V1 V2)]
    ;;the circle is in corner region of vertex[nEdge]
    (if (neg? dot)
      (let [dis (v2-len V1)
            n (v2-norm V1)
            rVec (v2-scale n (- radius))]
        (if (> dis radius)
          false
          (do (setCollisionInfo! ci
                                 (- radius dis)
                                 n (v2-add center rVec)) true)))
      ;;;else
      ;the center of circle is in corner region of vertex[nEdge+1]
      ;v1 is from right vertex of face to center of circle
      ;v2 is from right vertex of face to left vertex of face
      (let [v1 (v2-sub center vX)
            v2 (v2-scale V2 -1)
            dot (v2-dot v1 v2)]
        (cond
          (neg? dot)
          (let [dis (v2-len v1)
                n (v2-norm v1)
                rVec (v2-scale n (- radius))]
            (if (> dis radius)
              false
              (do (setCollisionInfo! ci
                                     (- radius dis)
                                     n (v2-add center rVec)) true)))
          ;;the circle is in face region of face[nEdge]
          (< bestDist radius)
          (let [rVec (v2-scale en radius)]
            (do (setCollisionInfo! ci
                                   (- radius bestDist)
                                   en
                                   (v2-sub center rVec)) true))
          :else false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollidedRectCirc "" [r1 cc1 ci]
  (let [{:keys [inside? bestDist nEdge]}
        (findNFaceToCircle r1 cc1 ci)]
    (if inside?
      (circleInsideRect r1 cc1 ci nEdge bestDist)
      (circleOutsideRect r1 cc1 ci nEdge bestDist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (rectCollidedRectCirc s1 s2 ci) (rectCollidedRectRect s1 s2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rectangle stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createFaceNormals

  "0--Top;1--Right;2--Bottom;3--Left"
  [[v0 v1 v2 v3 :as vs]]

  [(v2-norm (v2-sub v1 v2)) (v2-norm (v2-sub v2 v3))
   (v2-norm (v2-sub v3 v0)) (v2-norm (v2-sub v0 v1))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectRotate "" [s angle']
  (let [{:keys [center angle vertices normals]} @s
        [v0 v1 v2 v3] vertices
        a2 (+ angle angle')
        vs [(v2-rot v0 center angle')
            (v2-rot v1 center angle')
            (v2-rot v2 center angle')
            (v2-rot v3 center angle')]]
    (swap! s
           #(assoc %
                   :angle a2
                   :vertices vs
                   :normals (createFaceNormals vs))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectMove "" [s p]
  (let [{:keys [center vertices]} @s
        [v0 v1 v2 v3] vertices]
    (swap! s #(assoc %
                     :center (v2-add center p)
                     :vertices [(v2-add v0 p)
                                (v2-add v1 p)
                                (v2-add v2 p)
                                (v2-add v3 p)])) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectUpdateInertia "" [s]
  (let [{:keys [width height invMass]} @s]
    (swap! s
           #(assoc %
                   :inertia (if (zero? invMass)
                              0
                              (inv! (/ (* (inv! invMass)
                                          (pythagSQ width height)) 12))))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [center width height mass friction restitution]
  (let [{:keys [x y]} center
        hw (* 0.5 width)
        hh (* 0.5 height)
        ;;0--TopLeft;1--TopRight;2--BottomRight;3--BottomLeft
        [v0 v1 v2 v3 :as vs]
        [(vec2 (- x hw) (- y hh))
         (vec2 (+ x hw) (- y hh))
         (vec2 (+ x hw) (+ y hh))
         (vec2 (- x hw) (+ y hh))]
        s (RigidShape center
                      mass
                      friction restitution)]
    (->>
      #(assoc %
              :updateInertia rectUpdateInertia
              :collisionTest rectCollisionTest
              :move rectMove
              :rotate rectRotate
              :type :rectangle
              :width width
              :height height
              :bxRadius (* 0.5 (pythag width height))
              :vertices vs
              :normals (createFaceNormals vs)) (swap! s))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;circle-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleMove "" [c p]
  (let [{:keys [startPt center]} @c]
    (swap! c
           #(assoc %
                   :startPt (v2-add startPt p)
                   :center (v2-add center p))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleRotate "rotate angle in counterclockwise" [c angle']
  (let [{:keys [angle startPt center]} @c]
    (swap! c
           #(assoc %
                   :angle (+ angle angle')
                   :startPt (v2-rot startPt center angle'))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleUpdateInertia "" [c]
  (let [{:keys [invMass radius]} @c
        n (if (zero? invMass) 0 (* (inv! invMass) radius radius))]
    ;;why 12?
    (swap! c #(assoc % :inertia (/ n 12))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollidedCircCirc "" [cc1 cc2 ci]
  (let [{c1 :center r1 :radius} @cc1
        {c2 :center r2 :radius} @cc2
        v1to2  (v2-sub c2 c1)
        rSum  (+ r1 r2)
        dist  (v2-len v1to2)]
    (cond
      (> dist rSum) ;;no overlap
      false
      (zero? dist) ;;centers overlap
      (do (setCollisionInfo! ci
                         rSum
                         (vec2 0 -1)
                         (if (> r1 r2)
                           (v2-add c1 (vec2 0 r1)) (v2-add c2 (vec2 0 r2)))) true)
      :else ;overlap
      (let [rC2 (-> (v2-norm (v2-scale v1to2 -1)) (v2-scale r2))]
        (setCollisionInfo! ci
                           (- rSum dist) (v2-norm v1to2) (v2-add c2 rC2)) true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (circleCollidedCircCirc s1 s2 ci) (rectCollidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [center radius mass friction restitution]
  (let [{:keys [x y]} center
        s (RigidShape center mass friction restitution)]
    (swap! s
           #(merge %
                   {:collisionTest circleCollisionTest
                    :updateInertia circleUpdateInertia
                    :rotate  circleRotate
                    :move circleMove
                    :type :circle
                    :radius radius
                    :bxRadius radius
                    ;;the start point of line in circle
                    :startPt (vec2 x (- y radius))}))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;percentage of separation to project objects
(def *correction-rate* 0.8)
(def *relaxCount* 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- correctPos! "" [s1 s2 ci]
  (let [{:keys [depth normal]} @ci
        {m1 :invMass} @s1
        {m2 :invMass} @s2
        n (* (/ depth (+ m1 m2)) *correction-rate*)
        jiggle (v2-scale normal n)]
    (move! s1 (v2-scale jiggle (- m1)))
    (move! s2 (v2-scale jiggle m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision*
  "compute and apply response impulses for each object"
  [s1 s2 r1 r2 ci rVelocity rVelocityInNormal]
  (let [{e1 :inertia b1 :bounce f1 :sticky m1 :invMass} @s1
        {e2 :inertia b2 :bounce f2 :sticky m2 :invMass} @s2
        {:keys [normal]} @ci
        bounce' (js/Math.min b1 b2)
        sticky' (js/Math.min f1 f2)
        ;;R cross N
        r1xN (v2-cross r1 normal)
        r2xN (v2-cross r2 normal)
        ;;Calc impulse scalar
        ;;the formula of jN can be found in http://www.myphysicslab.com/collision.html
        jN (/ (* (- (+ 1 bounce')) rVelocityInNormal)
              (+ m1 m2 (* r1xN r1xN e1) (* r2xN r2xN e2)))
        ;;impulse is in direction of normal ( from s1 to s2)
        ;;impulse = F dt = m * ?v , ?v = impulse / m
        impulse (v2-scale normal jN)]
    (swap! s1
           (fn [{:keys [angVel vel] :as root}]
             (assoc root
                    :angVel (- angVel (* r1xN jN e1))
                    :vel (v2-sub vel (v2-scale impulse m1)))))
    (swap! s2
           (fn [{:keys [angVel vel] :as root}]
             (assoc root
                    :angVel (+ angVel (* r2xN jN e2))
                    :vel (v2-add vel (v2-scale impulse m2)))))
    ;;rVelocity.dot(tangent) should less than 0
    (let [tangent (->>
                    (v2-dot rVelocity normal)
                    (v2-scale normal)
                    (v2-sub rVelocity)
                    (v2-norm )
                    (v2-scale -1))
          r1xT (v2-cross r1 tangent)
          r2xT (v2-cross r2 tangent)
          jT' (/ (* (- (1 + bounce')) (v2-dot rVelocity tangent) sticky')
                 (+ m1 m2 (* r1xT r1xT e1) (* r2xT r2xT e2)))
          ;;friction should less than force in normal direction
          jT (if (> jT' jN) jN jT')
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (v2-scale tangent jT)]
      (swap! s1
             (fn [{:keys [angVel vel] :as root}]
               (assoc root
                      :angVel (- angVel (* r1xT jT e1))
                      :vel (v2-sub vel (v2-scale impulse m1)))))

      (swap! s2
             (fn [{:keys [angVel vel] :as root}]
               (assoc root
                      :angVel (+ angVel (* r2xT jT e2))
                      :vel (v2-add vel (v2-scale impulse m2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [s1 s2 ci]
  (when-not (and (zero? (:invMass @s1))
                 (zero? (:invMass @s2)))
    (correctPos! s1 s2 ci)
    ;;the direction of collisionInfo is always from s1 to s2
    ;;but the Mass is inversed, so start scale with s2 and end scale with s1
    (let
      [{:keys [normal start end]} @ci
       {m1 :invMass c1 :center vs1 :vel av1 :angVel} @s1
       {m2 :invMass c2 :center vs2 :vel av2 :angVel} @s2
       start' (v2-scale start (/ m2 (+ m1 m2)))
       end' (v2-scale end (/ m1 (+ m1 m2)))
       p (v2-add start' end')
       r1 (v2-sub p c1)
       r2 (v2-sub p c2)
       ;;newV = V + mAngularVelocity cross R
       v1 (v2-add vs1 (vec2 (- (* av1 (:y r1))) (* av1 (:x r1))))
       v2 (v2-add vs2 (vec2 (- (* av2 (:y r2))) (* av2 (:x r2))))
       rVelocity (v2-sub v2 v1)
       rVelocityInNormal (v2-dot rVelocity normal)]
      ;;if objects moving apart ignore
      (if-not (pos? rVelocityInNormal)
        (resolveCollision* s1 s2 r1 r2 ci rVelocity rVelocityInNormal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collision* "" [samples]
  (let [len (count samples)
        ci (collisionInfo)]
    (loop [i 0]
      (when-not (>= i len)
        (loop [j (+ 1 i)]
          (when-not (>= j len)
            (let [si (nth samples i)
                  sj (nth samples j)]
              (when (and (boundTest? si sj)
                         (collisionTest si sj ci))
                ;;make sure the normal is always from object[i] to object[j]
                (if (neg? (v2-dot (:normal @ci)
                                  (v2-sub (:center @sj)
                                          (:center @si))))
                  (changeCollisionDir ci))
                (resolveCollision si sj ci)))
            (recur (+ 1 j))))
        (recur (+ 1 i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision "" [world]
  (let [{:keys [samples]} @world]
    (dotimes [_ *relaxCount*] (collision* samples))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




