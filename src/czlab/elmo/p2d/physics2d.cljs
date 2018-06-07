;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Length
  "" [{:keys [x y] :as v2}] (js/Math.sqrt (* x x) (* y y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Add "" [v1 v2]
  (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Sub "" [v1 v2]
  (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Scale
  "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Dot "" [v1 v2]
  (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Cross "" [v1 v2]
  (- (* (:x v1)(:y v2)) (* (:y v1)(:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Rotate
  "Rotate in radian" [v center angle]
  (let [x (- (:x v) (:x center))
        y (- (:y v) (:y center))
        r0 (- (* x (js/Math.cos angle))
              (* y (js/Math.sin angle)))
        r1 (+ (* x (js/Math.sin angle))
              (* y (js/Math.cos angle)))]
    (vec2 (+ r0 (:x center))
          (+ r1 (:y center)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Normalize "" [v]
  (let [vl (vec2Length v)
        len (if (pos? vl)(/ 1 vl) vl)]
    (vec2 (* len (:x v)) (* len (:y v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Distance "" [v1 v2]
  (let [x (- (:x v1)(:x v2))
        y (- (:y v1)(:y v2))]
    (js/Math.sqrt (+ (* x x)(* y y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionInfo "" []
  (atom {:depth 0
         :normal (vec2 0 0) :start (vec2 0 0) :end (vec2 0 0)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionDepth! "" [c d]
  (swap! c #(assoc % :depth d)) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionNormal! "" [c v2]
  (swap! c #(assoc % :normal v2)) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionInfo! "" [c depth normal start]
  (swap! c
         #(assoc %
                 :depth depth
                 :normal normal
                 :start start
                 :end (vec2Add start
                               (vec2Scale normal depth)))) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn changeCollisionDir! "" [c]
  (let [{:keys [start normal end]} @c]
    (swap! c
           #(assoc %
                   :start end
                   :end start
                   :normal (vec2Scale normal -1))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;physics
(def *positional-correction-flag* true)
(def relaxation-count 15)
;;percentage of separation to project objects
(def pos-correction-rate 0.8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positionalCorrection "" [s1 s2 ci]
  (let [s1InvMass s1.mInvMass
        s2InvMass s2.mInvMass
        {:keys [depth normal]} @ci
        n (* (/ depth (+ s1InvMass s2InvMass)) pos-correction-rate)
        correctionAmount (vec2Scale normal n)]
    (shapeMove s1 (vec2Scale correctionAmount (- s1InvMass)))
    (shapeMove s2 (vec2Scale correctionAmount s2InvMass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [s1 s2 ci]
  (if-not (and (zero? s1.mInvMass)
               (zero? s2.mInvMass))
    (resolveCollision* s1 s2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision* "" [s1 s2 ci]
  (if *positional-correction-flag* (positionalCorrection s1 s2 ci))
  (let [{:keys [normal start end]} @ci
        ;;the dir of ci is always s1 -> s2
        ;;but the mass is inversed, so scale start with s2 and scale end with s1
        start' (vec2Scale start
                          (/ s2.mInvMass (+ s1.mInvMass s2.mInvMass)))
        end' (vec2Scale end
                        (/ s1.mInvMass (+ s1.mInvMass s2.mInvMass)))
        p (vec2Add start' end')
        r1 (vec2Sub p s1.mCenter)
        r2 (vec2Sub p s2.mCenter)
        v1 (vec2Add s1.mVelocity
                    (vec2 (* (- 1) s1.mAngularVelocity (:y r1))
                          (* s1.mAngularVelocity (:x r1))))
        v2 (vec2Add s2.mVelocity
                    (vec2 (* (- 1) s2.mAngularVelocity (:y r2))
                          (* s2.mAngularVelocity (:x r2))))
        relativeVelocity (vec2Sub v2 v1)
        ;;relative velocity in normal direction
        rVelocityInNormal (vec2Dot relativeVelocity normal)]
    ;;if objects moving apart ignore
    (if-not (pos? rVelocityInNormal)
      (resolveCollision** s1 s2 r1 r2 ci relativeVelocity rVelocityInNormal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision** "" [s1 s2 r1 r2 ci
                              relativeVelocity
                              rVelocityInNormal]
  ;;compute and apply response impulses for each object
  (let [newRestituion (js/Math.min s1.mRestitution s2.mRestitution)
        newFriction (js/Math.min s1.mFriction s2.mFriction)
        {:keys [normal]} @ci
        ;;R cross N
        R1crossN (vec2Cross r1 normal)
        R2crossN (vec2Cross r2 normal)
        ;;Calc impulse scalar
        ;;the formula of jN can be found in
        ;;http://www.myphysicslab.com/collision.html
        jN (* (- (+ 1 newRestituion)) rVelocityInNormal)
        jN (/ jN
              (+ s1.mInvMass s2.mInvMass
                 (* R1crossN R1crossN s1.mInertia)
                 (* R2crossN R2crossN s2.mInertia)))
        ;;impulse is in direction of normal (from s1 to s2)
        ;;impulse = F dt = m * ?v
        ;;?v = impulse / m
        impulse (vec2Scale normal jN)]
    (swap! s1
           #(assoc %
                   :velocity
                   (vec2Sub s1.mVelocity (vec2Scale impulse s1.mInvMass))
                   :angularVelocity
                   (- s1.mAngularVelocity
                      (* R1crossN jN s1.mInertia))))
    (swap! s2
           #(assoc %
                   :velocity
                   (vec2Add s2.mVelocity (vec2Scale impulse s2.mInvMass))
                   :angularVelocity
                   (+ s2.mAngularVelocity
                      (* R2crossN jN s2.mInertia))))
    (let [tangent (vec2Sub relativeVelocity
                           (vec2Scale normal
                                      (vec2Dot relativeVelocity normal)))
          tangent (vec2Scale (vec2Normalize tangent) -1)
          R1crossT (vec2Cross r1 tangent)
          R2crossT (vec2Cross r2 tangent)
          jT (* (- (+ 1 newRestituion))
                (* (vec2Dot relativeVelocity tangent) newFriction))
          jT (/ jT
                (+ s1.mInvMass
                   s2.mInvMass
                   (+ (* R1crossT R1crossT s1.mInertia)
                      (* R2crossT R2crossT s2.mInertia))))
          ;;friction should less than force in normal direction
          jT (if (> jT jN) jN jT)
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (vec2Scale tangent jT)]
      (swap! s1
             #(assoc %
                     :velocity
                     (vec2Sub s1.mVelocity
                              (vec2Scale impulse s1.mInvMass))
                     :angularVelocity
                     (- s1.mAngularVelocity
                        (* R1crossT jT s1.mInertia))))
      (swap! s2
             #(assoc %
                     :velocity
                     (vec2Add s2.mVelocity
                              (vec2Scale impulse s2.mInvMass))
                     :angularVelocity
                     (+ s2.mAngularVelocity
                        (* R2crossT jT s2.mInertia)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawCollisionInfo "" [ci ctx] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision "" [objects]
  (let [ci (collisionInfo)
        len (count objects)]
    (dotimes [k (range relaxation-count)]
      (dotimes [i (range len)]
        (loop [j (+ 1 i)]
          (when (< j len)
            (when (boundTest (nth objects i)
                             (nth objects j))
              (when (collisionTest (nth objects i)
                                   (nth objects j ci))
                ;;make sure the normal is always
                ;;from object[i] to object[j]
                (if (neg? (vec2Dot (:normal @ci)
                                     (vec2Sub (:center (nth objects j))
                                              (:center (nth objects i)))))
                  (changeCollisionDir! ci))
                (resolveCollision (nth objects i)
                                  (nth objects j) ci)))
            (recur (inc j))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateInertia  "" [s] ((:updateInertia @s) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move  "" [s p] ((:move @s) s p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate  "" [s angle] ((:rotate @s) angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gravity* (vec2 0 20))
(defn rigidShape "" [center mass friction restitution]
  (let [mass' (if (number? mass) mass 1)
        s {:center center
           :inertia 0
           :friction (if (number? friction) friction 0.8)
           :bounce (if (number? restitution) restitution 0.2)
           :vel (vec2 0 0)
           :invMass (if-not (zero? mass') (/ 1 mass') mass')
           :accel (if-not (zero? mass') *gravity* (vec2 0 0))
           :angle 0
           ;;negetive-- clockwise
           :angVel 0
           :angAccel 0
           :boundRadius 0}
        ret (atom s)]
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass "" [s delta]
  (let [{:keys [invMass]} @s
        mass (+ delta (if-not (zero? invMass) (/ 1 invMass) 0))]
    (if (pos? mass)
      (swap! s
             #(assoc %
                     :invMass (/ 1 mass)
                     :accel *gravity*))
      (swap! s
             #(assoc %
                     :invMass 0
                     :vel (vec2 0 0)
                     :accel (vec2 0 0)
                     :angVel 0
                     :angAccel 0)))
    (updateInertia s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update "" [s]
  (when *movement*
    (let [dt *updateIntervalInSeconds*
          {:keys [accel vel]} @s]
      (swap! s
             #(assoc %
                     :vel (vec2Add vel (vec2Scale accel dt))))
      (move s (vec2Scale (:vel @s) dt))
      (swap! s
             #(assoc %
                     :angVel
                     (+ (:angVel @s)
                        (* (:angAccel @s) dt))))
      (rotate s (* dt (:angVel @s)))))
  (let [width *width*
        height *height*
        {:keys [center]} @s]
    (when (or (< (:x center) 0)
              (> (:x center) width)
              (< (:y center) 0)
              (> (:y center) height))
      (let [i (.indexOf AllObjects s)]
        (if (>= i 0)
          (.splice AllObjects i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn boundTest "" [s1 s2]
  (let [vFrom1to2 (vec2Sub (:center @s2) (:center @s1))
        rSum  (+ (:boundRadius @s1) (:boundRadius @s2))
        dist (vec2Length vFrom1to2)]
    (not (> dist rSum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- moveCircle "" [s p]
  (let [{:keys [startPt center]} @s]
    (swap! s
           #(assoc %
                   :startPt (vec2Add startPt p)
                   :center (vec2Add center p))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawCircle "" [s])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rotCircle
  "rotate angle in counterclockwise"
  [s rotation]
  (let [{:keys [angle startPt]} @s]
    (swap! s
           #(assoc %
                   :angle (+ angle rotation)
                   :startPt (vec2Rotate startPt center rotation))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateInertiaCircle "" [s]
  (let [{:keys [radius invMass]} @s]
    (swap! s
           #(assoc %
                   :inertia
                   (if (zero? invMass)
                     0
                     ;; Inertia=mass * radius^2
                     ;; 12 is a constant value that can be changed
                     (/ (* (/ 1 invMass)
                           (* radius radius)) 12)))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedCircCirc "" [cc1 cc2 ci]
  (let [{c2 :center r2 :radius} @cc2
        {c1 :center r1 :radius} @cc1
        vFrom1to2 (vec2Sub c2 c1)
        rSum (+ r1 r2)
        dist (vec2Length vFrom1to2)]
    (cond
      (> dist (js/Math.sqrt (* rSum rSum))) ;overlapping
      false
      (not (zero? dist)) ;;overlapping but not same position
      (let [normalFrom2to1 (vec2Normalize (vec2Scale vFrom1to2 -1))
            radiusC2 (vec2Scale normalFrom2to1 r2)]
        (setCollisionInfo! ci
                           (- rSum dist)
                           (vec2Normalize vFrom1to2)
                           (vec2Add c2 r2)) true)
      :else ;;same pos
      (do (if (> r1 r2)
            (setCollisionInfo! ci
                               rSum
                               (vec2 0 -1) (vec2Add c1 (vec2 0 r1)))
            (setCollisionInfo! ci
                               rSum
                               (vec2 0 -1)
                               (vec2Add c2 (vec2 0 r2)))) true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collideTestCircle "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedCircCirc s1 s2 ci)
    (collidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circle "" [center radius mass friction restitution]
  (let [s (rigidShape center mass friction restitution)]
    (swap! s
           #(assoc %
                   :type :circle
                   :radius radius
                   :rotate rotCircle
                   :move moveCircle
                   :collisionTest collideTestCircle
                   :updateInertia updateInertiaCircle
                   :boundRadius radius
                   ;;start point of line in circle
                   :startPt (vec2 (:x center)
                                  (- (:y center) radius))))
    (updateInertia s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rotRect "" [s rotation]
  (swap! s #(assoc % :angle (+ rotation (:angle @s))))
  (let [{:keys [vertices center angle]} @s
        vs (transient [])
        _ (doseq [v vertices]
            (conj! vs (vec2Rotate v center angle)))
        vs' (persistent! vs)
        fs (transient [])]
    (conj! fs (vec2Normalize (vec2Sub (nth vs' 1) (nth vs' 2))))
    (conj! fs (vec2Normalize (vec2Sub (nth vs' 2) (nth vs' 3))))
    (conj! fs (vec2Normalize (vec2Sub (nth vs' 3)(nth vs' 0))))
    (conj! fs (vec2Normalize (vec2Sub (nth vs' 0)(nth vs' 1))))
    (swap! s
           #(assoc %
                   :vertices vs'
                   :normals (persistent! fs))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- moveRect "" [s p]
  (let [{:keys [vertices center]} @s
        vs (transient [])]
    (doseq [v vertices]
      (conj! vs (vec2Add v p)))
    (swap! s
           #(assoc %
                   :vertices (persistent! vs)
                   :center (vec2Add center p))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawRect "" [s] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateInertiaRect "" [s]
  (let [{:keys [invMass width height]} @s]
    (swap! s
           #(assoc %
                   :inertia
                   (if (zero? invMass)
                     0
                     ;;inertia=mass*width^2+height^2
                     (/ 1 (/ (* (/ 1 invMass)
                                (* width width)
                                (* height height)) 12))))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectangle "" [center width height mass friction restitution]
  (let [s (rigidShape center mass friction restitution)
        {:keys [x y] center}
        hw (/ width 2)
        hh (/ height 2)
        ;;0--TopLeft 1--TopRight 2--BottomRight 3--BottomLeft
        vs [(vec2 (- x hw) (- y hh))
            (vec2 (+ x hw) (- y hh))
            (vec2 (+ x hw) (+ y hh))
            (vec2 (- x hw) (+ y hh))]
        ;;0--Top;1--Right;2--Bottom;3--Left
        ;;normal of face towards outside of rectangle
        nn [(vec2Normalize (vec2Sub (nth vs 1)(nth vs 2)))
            (vec2Normalize (vec2Sub (nth vs 2)(nth vs 3)))
            (vec2Normalize (vec2Sub (nth vs 3)(nth vs 0)))
            (vec2Normalize (vec2Sub (nth vs 0)(nth vs 1)))]]
    (swap! s
           #(assoc %
                   :type :rectangle
                   :move moveRect
                   :rotate rotRect
                   :updateInertia updateInertiaRect
                   :vertices vs
                   :normals nn
                   :width width
                   :height height
                   :boundRadius (/ (js/Math.sqrt (+ (* width width)
                                                    (* height height))) 2)))
    (updateInertia s)
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EOF

(defn- collideTestRect "" [r1 r2 ci]
  (if (= (:type @r2) :circle)
    (collidedRectCirc r1 r2 ci)
    (collidedRectRect r1 r2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- supportStruct
  "" [] {:supportPoint nil :supportPointDist 0})

(def tmpSupport (atom (supportStruct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findSupportPoint "" [r1 dir ptOnEdge]
  (let [{:keys [vertices]} @r1]
    ;;the longest project length
    (swap! tmpSupport
           #(assoc %
                   :supportPointDist -9999999 :supportPoint nil))
    ;;check each vector of other object
    (doseq [v vertices
            :let [vToEdge (vec2Sub v ptOnEdge)
                  projection (vec2Dot vToEdge dir)]]
      ;;find the longest distance with certain edge
      ;;dir is -n direction, so the distance should be positive
      (when (and (pos? projection)
                 (> projection (:supportPointDist @tmpSupport)))
        (swap! tmpSupport
               #(assoc %
                       :supportPoint v
                       :supportPointDist projection))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findAxisLeastPenetration "" [r1 r2 ci]
  (let [{:keys [vertices normals]} @r1
        flen (count normals)
        [supportPoint bestDistance bestIndex hasSupport?]
        (loop [supportPoint js/undefined
               bestDistance 999999
               bestIndex nil hasSupport? true i 0]
          (if (or (not hasSupport?)
                  (>= i flen))
            [supportPoint bestDistance bestIndex hasSupport?]
            ;;retrieve a face normal from A
            (let [n (nth normals i)
                  ;;use -n as direction and the vectex on edge i as point on edge
                  dir (vec2Scale n -1)
                  ptOnEdge (nth vertices i)
                  ;;find the support on B
                  ;;the point has longest distance with edge i
                  _ (findSupportPoint r2 dir ptOnEdge)
                  hasSupport?' (some? (:supportPoint @tmpSupport))]
              ;;get the shortest support point depth
              (if (and hasSupport?'
                       (< (:supportPointDist @tmpSupport) bestDistance))
                (recur (:supportPoint @tmpSupport)
                       (:supportPointDist @tmpSupport)
                       i hasSupport?' (inc i))
                (recur supportPoint
                       bestDistance
                       bestIndex hasSupport?' (inc i))))))]
    (when hasSupport?
      ;;all four directions have support point
      (let [bestVec (vec2Scale (nth normals bestIndex) bestDistance)]
        (setCollisionInfo! ci
                           bestDistance
                           (nth normals bestIndex)
                           (vec2Add supportPoint bestVec))))
    hasSupport?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def collisionInfoR1 (collisionInfo))
(def collisionInfoR2 (collisionInfo))
(defn- collidedRectRect "" [r1 r2 ci]
  ;;find Axis of Separation for both rectangle
  (let [status1 (findAxisLeastPenetration r1 r2 collisionInfoR1)
        status2
        (when status1
          (let [status2 (findAxisLeastPenetration r2 r1 collisionInfoR2)
                {d1 :depth n1 :normal s1 :start} @collisionInfoR1
                {d2 :depth n2 :normal s2 :start} @collisionInfoR2]
            (when status2
              ;;if both of rectangles are overlapping, choose the shorter normal as the normal
              (if (< d1 d2)
                (setCollisionInfo! ci
                                   d1
                                   n1
                                   (vec2Sub s1 (vec2Scale n1 d1)))
                (setCollisionInfo! ci
                                   d2
                                   (vec2Scale n2 -1) s2)))))]
    (and status1 status2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedRectCirc "" [r1 c2 ci]
    var inside = true;
    var bestDistance = -99999;
    var nearestEdge = 0;
    var i, v;
    var circ2Pos, projection;
    for (i = 0; i < 4; i++) {
        //find the nearest face for center of circle
        circ2Pos = otherCir.mCenter;
        v = circ2Pos.subtract(this.mVertex[i]);
        projection = v.dot(this.mFaceNormal[i]);
        if (projection > 0) {
            //if the center of circle is outside of rectangle
            bestDistance = projection;
            nearestEdge = i;
            inside = false;
            break;
        }
        if (projection > bestDistance) {
            bestDistance = projection;
            nearestEdge = i;
        }
    }
    var dis, normal, radiusVec;
    if (!inside) {
        //the center of circle is outside of rectangle

        //v1 is from left vertex of face to center of circle
        //v2 is from left vertex of face to right vertex of face
        var v1 = circ2Pos.subtract(this.mVertex[nearestEdge]);
        var v2 = this.mVertex[(nearestEdge + 1) % 4].subtract(this.mVertex[nearestEdge]);

        var dot = v1.dot(v2);

        if (dot < 0) {
            //the center of circle is in corner region of mVertex[nearestEdge]
            dis = v1.length();
            //compare the distance with radium to decide collision
            if (dis > otherCir.mRadius) {
                return false;
            }

            normal = v1.normalize();
            radiusVec = normal.scale(-otherCir.mRadius);
            collisionInfo.setInfo(otherCir.mRadius - dis, normal, circ2Pos.add(radiusVec));
        } else {
            //the center of circle is in corner region of mVertex[nearestEdge+1]

            //v1 is from right vertex of face to center of circle
            //v2 is from right vertex of face to left vertex of face
            v1 = circ2Pos.subtract(this.mVertex[(nearestEdge + 1) % 4]);
            v2 = v2.scale(-1);
            dot = v1.dot(v2);
            if (dot < 0) {
                dis = v1.length();
                //compare the distance with radium to decide collision
                if (dis > otherCir.mRadius) {
                    return false;
                }
                normal = v1.normalize();
                radiusVec = normal.scale(-otherCir.mRadius);
                collisionInfo.setInfo(otherCir.mRadius - dis, normal, circ2Pos.add(radiusVec));
            } else {
                //the center of circle is in face region of face[nearestEdge]
                if (bestDistance < otherCir.mRadius) {
                    radiusVec = this.mFaceNormal[nearestEdge].scale(otherCir.mRadius);
                    collisionInfo.setInfo(otherCir.mRadius - bestDistance, this.mFaceNormal[nearestEdge], circ2Pos.subtract(radiusVec));
                } else {
                    return false;
                }
            }
        }
    } else {
        //the center of circle is inside of rectangle
        radiusVec = this.mFaceNormal[nearestEdge].scale(otherCir.mRadius);
        collisionInfo.setInfo(otherCir.mRadius - bestDistance, this.mFaceNormal[nearestEdge], circ2Pos.subtract(radiusVec));
    }
    return true;
};


