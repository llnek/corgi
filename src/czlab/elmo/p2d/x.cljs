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
(def VEC_ZERO (vec 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- inv! "" [x] (if (zero? x) 0 (/ 1 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rigidBody "" [center mass friction restitution]
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
           :bcRadius 0})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defn updateBody! "" [s dt]
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
  (let [{c1 :center r1 :bcRadius} @s1
        {c2 :center r2 :bcRadius} @s2
        v1to2 (v2-sub c2 c1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (rectCollidedRectCirc s1 s2 ci) (rectCollidedRectRect s1 s2 ci)))

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

/**
 * Check for collision between RigidRectangle and RigidRectangle
 * @param {Rectangle} r1 Rectangle object to check for collision status
 * @param {Rectangle} r2 Rectangle object to check for collision status against
 * @param {CollisionInfo} collisionInfo Collision info of collision
 * @returns {Boolean} true if collision occurs
 * @memberOf Rectangle
 */
var collisionInfoR1 = new CollisionInfo();
var collisionInfoR2 = new CollisionInfo();
Rectangle.prototype.collidedRectRect = function (r1, r2, collisionInfo) {

    var status1 = false;
    var status2 = false;

    //find Axis of Separation for both rectangle
    status1 = r1.findAxisLeastPenetration(r2, collisionInfoR1);

    if (status1) {
        status2 = r2.findAxisLeastPenetration(r1, collisionInfoR2);
        if (status2) {
            //if both of rectangles are overlapping, choose the shorter normal as the normal
            if (collisionInfoR1.getDepth() < collisionInfoR2.getDepth()) {
                var depthVec = collisionInfoR1.getNormal().scale(collisionInfoR1.getDepth());
                collisionInfo.setInfo(collisionInfoR1.getDepth(), collisionInfoR1.getNormal(), collisionInfoR1.mStart.subtract(depthVec));
            } else {
                collisionInfo.setInfo(collisionInfoR2.getDepth(), collisionInfoR2.getNormal().scale(-1), collisionInfoR2.mStart);
            }
        }
    }
    return status1 && status2;
};

/**
 * Check for collision between Rectangle and Circle
 * @param {Circle} otherCir circle to check for collision status against
 * @param {CollisionInfo} collisionInfo Collision info of collision
 * @returns {Boolean} true if collision occurs
 * @memberOf Rectangle
 */
Rectangle.prototype.collidedRectCirc = function (otherCir, collisionInfo) {

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
      (let [rC2 (-> (v2-normal (v2-scale v1to2 -1)) (v2-scale r2))]
        (setCollisionInfo! ci
                           (- rSum dist) (v2-normal v1to2) (v2-add c2 rC2)) true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (circleCollidedCircCirc s1 s2 ci) (collidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circleBody "" [center radius mass friction restitution]
  (let [{:keys [x y]} center
        s (rigidBody center mass friction restitution)]
    (swap! s
           #(merge %
                   {:collisionTest circleCollisionTest
                    :updateInertia circleUpdateInertia
                    :rotate  circleRotate
                    :move circleMove
                    :type :circle
                    :radius radius
                    :bcRadius radius
                    ;;the start point of line in circle
                    :startPt (vec2 x (- y radius))}))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EOF

