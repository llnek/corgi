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
(defn- circleCollidedCircCirc "" [c1 c2 ci]
    var vFrom1to2 = c2.mCenter.subtract(c1.mCenter);
    var rSum = c1.mRadius + c2.mRadius;
    var dist = vFrom1to2.length();
    if (dist > Math.sqrt(rSum * rSum)) {
        //not overlapping
        return false;
    }
    if (dist !== 0) {
        // overlapping bu not same position
        var normalFrom2to1 = vFrom1to2.scale(-1).normalize();
        var radiusC2 = normalFrom2to1.scale(c2.mRadius);
        collisionInfo.setInfo(rSum - dist, vFrom1to2.normalize(), c2.mCenter.add(radiusC2));
    } else {
        //same position
        if (c1.mRadius > c2.mRadius) {
            collisionInfo.setInfo(rSum, new Vec2(0, -1), c1.mCenter.add(new Vec2(0, c1.mRadius)));
        } else {
            collisionInfo.setInfo(rSum, new Vec2(0, -1), c2.mCenter.add(new Vec2(0, c2.mRadius)));
        }
    }
    return true;
};

Circle.prototype.collisionTest = function (otherShape, collisionInfo) {
    var status = false;
    if (otherShape.mType === "Circle") {
        status = this.collidedCircCirc(this, otherShape, collisionInfo);
    } else {
        status = otherShape.collidedRectCirc(this, collisionInfo);
    }
    return status;
};

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn circleBody "" [center radius mass friction restitution]
  (let [{:keys [x y]} center
        s (rigidBody center mass friction restitution)]
    (swap! s
           #(merge %
                   {:updateInertia circleUpdateInertia
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

