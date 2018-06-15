;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.physics2d

  (:require [oops.core :refer [oget oset!
                               ocall oapply
                               ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- inv! "" [x] (if (zero? x) 0 (/ 1 x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def VEC_ZERO (vec2 0 0))
(def *shapeNum* (atom 0))
(defn- nextShapeNum "" [] (swap! *shapeNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gWorld*
  (atom {:samples (array)
         :context nil
         :cur 0
         :canvas nil
         :top 0
         :right 799
         :bottom 449
         :left 0
         :width 800
         :height 450
         :gravity (vec2 0 20)}))
(def mPositionalCorrection? true)
(def mPreviousTime (system-time))
(def mMovement? true)
(def mCurrentTime 0)
(def mElapsedTime 0)
(def mLagTime 0)
(def kFPS 60)
(def kFrameSecs (inv! kFPS))
(def kMPF (* 1000 kFrameSecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pythagSQ "" [x y] (+ (* x x) (* y y)))
(defn- pythag "" [x y] (js/Math.sqrt (pythagSQ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-len "" [v] (pythag (:x v) (:y v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-add "" [v1 v2] (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sub "" [v1 v2] (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-scale "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dot "" [v1 v2] (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-cross "" [v1 v2] (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-rot "rotate counter-clockwise" [v1 center angle]
  (let [{cx :x cy :y} center
        cos (js/Math.cos angle)
        sin (js/Math.sin angle)
        x (- (:x v1) cx) y (- (:y v1) cy)]
    (vec2 (+ cx (- (* x cos) (* y sin)))
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
  (atom {:depth 0 :normal VEC_ZERO :start VEC_ZERO :end VEC_ZERO}))

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
(defn- RigidShape "" [center & [mass friction restitution]]
  (let [{:keys [gravity samples]} @*gWorld*
        mass' (if (number? mass) mass 1)
        ret (atom {:invMass (inv! mass')
                   :center center
                   :oid (nextShapeNum)
                   :inertia 0
                   :vel VEC_ZERO
                   :angle 0
                   :angVel 0 ;; clockwise = negative
                   :angAccel 0
                   :bxRadius 0
                   :accel (if (zero? mass') VEC_ZERO gravity)
                   :sticky (if (number? friction) friction 0.8)
                   :bounce (if (number? restitution) restitution 0.2)})]
    (.push samples ret)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionTest?? "" [s1 s2 ci] ((:collisionTest @s1) s1 s2 ci))
(defn updateInertia! "" [s] ((:updateInertia @s) s) s)
(defn move! "" [s p] ((:move @s) s p) s)
(defn rotate! "" [s v] ((:rotate @s) s v) s)
(defn draw "" [s c] ((:draw @s) s c) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass! "" [s delta]
  (let [{:keys [gravity]} @*gWorld*
        {:keys [invMass]} @s
        mass (+ (inv! invMass) delta)]
    (if (pos? mass)
      (swap! s #(assoc % :invMass (inv! mass) :accel gravity))
      (swap! s #(assoc % :invMass 0 :vel VEC_ZERO :accel VEC_ZERO :angVel 0 :angAccel 0)))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateShape! "" [s dt]
  (let [{:keys [top right bottom left height width samples]} @*gWorld*]
    (when mMovement?
      ;; v = v + a*t
      (swap! s (fn [{:keys [vel accel] :as root}]
                 (assoc root :vel (v2-add vel (v2-scale accel dt)))))
      ;;s = x + v*t
      (move! s (v2-scale (:vel @s) dt))

      (swap! s (fn [{:keys [angVel angAccel] :as root}]
                 (assoc root :angVel (+ angVel (* angAccel dt)))))
      (rotate! s (* (:angVel @s) dt)))
    (let [{cx :x cy :y} (:center @s)]
      (when (or (< cx 0)
                (> cx width)
                (< cy 0)
                (> cy height))
        (let [pos (.indexOf samples s)]
          (if-not (neg? pos) (.splice samples pos 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [s1 s2]
  (let [{c1 :center r1 :bxRadius} @s1
        {c2 :center r2 :bxRadius} @s2
        v1to2 (v2-sub c2 c1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findSupportPoint "" [r1 dir ptOnEdge]
  (let [{:keys [vertices]} @r1
        len (count vertices)]
    (loop [spDist -9999999 sp nil i 0]
      (if (>= i len)
        [spDist sp]
        (let [v' (nth vertices i)
              ii (+ i 1)
              proj (v2-dot (v2-sub v' ptOnEdge) dir)]
          ;;find the longest distance with certain edge
          ;;dir is -n direction, so the distance should be positive
          (if (and (pos? proj)
                   (> proj spDist))
            (recur proj v' ii)
            (recur spDist sp ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hasAxisLeastPenetration?
  "Find the shortest axis that's overlapping" [r1 r2 ci]
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
                  [supportPtDist supportPt]
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
  "Check for collision between 2 rectangles"
  [r1 r2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (collisionInfo)
        ci_2 (collisionInfo)
        p1? (hasAxisLeastPenetration? r1 r2 ci_1)
        p2? (if p1? (hasAxisLeastPenetration? r2 r1 ci_2))]
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
(defn- circleInsideRect? "" [r1 cc1 ci nEdge bestDist]
  (let [{:keys [normals vertices]} @r1
        {:keys [radius center]} @cc1
        n (nth normals nEdge)
        rVec (v2-scale n radius)]
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
        {:inside? inside? :bestDist bestDist :nEdge nEdge}
        (let [v (v2-sub center (nth vertices i))
              ii (+ i 1)
              proj (v2-dot v (nth normals i))]
          (cond
            ;;the center of circle is outside of rectangle
            (pos? proj) (recur false proj i ii)
            (> proj bestDist) (recur inside? proj i ii)
            :else (recur inside? bestDist nEdge ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleOutsideRect? "" [r1 cc1 ci nEdge bestDist]
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
      (circleInsideRect? r1 cc1 ci nEdge bestDist)
      (circleOutsideRect? r1 cc1 ci nEdge bestDist))))

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
(defn- rectDraw "" [r1 context]
  (let [{:keys [vertices width height angle oid]} @r1
        {:keys [x y]} (nth vertices 0)]
    ;(js/console.log (str "draw rect: " oid))
    (ocall! context "save")
    (ocall! context "translate" x y)
    (ocall! context "rotate" angle)
    (ocall! context "strokeRect" 0 0 width height)
    (ocall! context "restore")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [center width height & [mass friction restitution]]
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
              :draw rectDraw
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
(defn- circleDraw "" [c1 context]
  (let [{:keys [center radius startPt oid]} @c1
        {:keys [x y]} center
        {sx :x sy :y} startPt]
    ;(js/console.log (str "draw circle: " oid))
    (ocall! context "beginPath")
    (ocall! context "arc" x y radius 0 (* 2 js/Math.PI) true)
    (ocall! context "moveTo" sx sy)
    (ocall! context "lineTo" x y)
    (ocall! context "closePath")
    (ocall! context "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [center radius & [mass friction restitution]]
  (let [{:keys [x y]} center
        s (RigidShape center mass friction restitution)]
    (swap! s
           #(merge %
                   {:collisionTest circleCollisionTest
                    :updateInertia circleUpdateInertia
                    :rotate  circleRotate
                    :draw circleDraw
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
(defn- drawCollisionInfo "" [ci context]
  (let [{:keys [start end]} @ci
        {sx :x sy :y} start
        {ex :x ey :y} end]
    (ocall! context "beginPath")
    (ocall! context "moveTo" sx sy)
    (ocall! context "lineTo" ex ey)
    (ocall context "closePath")
    (oset! context "!strokeStyle" "orange")
    (ocall! context "stroke")))

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
    (let [tangent (-> (->> (v2-dot rVelocity normal)
                           (v2-scale normal)
                           (v2-sub rVelocity)
                           (v2-norm ))
                      (v2-scale -1))
          r1xT (v2-cross r1 tangent)
          r2xT (v2-cross r2 tangent)
          jT' (/ (* (- (+ 1 bounce')) (v2-dot rVelocity tangent) sticky')
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
    (if mPositionalCorrection? (correctPos! s1 s2 ci))
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
(defn- checkCollision* "" []
  (let [{:keys [samples context]} @*gWorld*
        len (count samples)
        ci (collisionInfo)]
    (loop [i 0]
      (when-not (>= i len)
        (loop [j (+ 1 i)]
          (when-not (>= j len)
            (let [si (nth samples i)
                  sj (nth samples j)]
              (when (and (overlap? si sj)
                         (collisionTest?? si sj ci))
                ;;make sure the normal is always from object[i] to object[j]
                (if (neg? (v2-dot (:normal @ci)
                                  (v2-sub (:center @sj)
                                          (:center @si))))
                  (changeCollisionDir ci))
                (drawCollisionInfo ci context)
                (resolveCollision si sj ci)))
            (recur (+ 1 j))))
        (recur (+ 1 i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn checkCollision "" []
  (dotimes [_ *relaxCount*] (checkCollision*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alterShapeAttr! "" [s attr & more]
  (let [{:keys [angVel vel]} @s
        p1 (first more)
        v (cond (= :angVel attr) (+ angVel p1)
                (= :vel attr) (v2-add vel p1)
                (= :bounce attr) p1
                (= :sticky attr) p1)]
    (if (some? v)
      (swap! s #(assoc % attr v))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(defn ^:export userControl "" [evt]
  (let [key (or (oget evt "?keyCode")
                (oget evt "?which"))
        {:keys [cur samples]} @*gWorld*
        s (nth samples cur)
        len (count samples)
        offset (- key 48)]
    (cond
      (and (>= key 48)
           (<= key 57))
      (if (< (- offset 48) len)
        (swap! *gWorld* #(assoc % :cur offset)))
      (= key 38) ;up arrow
      (if (pos? cur)
        (swap! *gWorld* #(assoc % :cur (dec cur))))
      (= key 40) ;down arrow
      (if (< cur (- len 1))
        (swap! *gWorld* #(assoc % :cur (inc cur))))
      (= key 87) ;;W
      (move! s (vec2 0 -10))
      (= key 83) ;;S
      (move! s (vec2 0 10))
      (= key 65) ;;A
      (move! s (vec2 -10 0))
      (= key 68) ;;D
      (move! s (vec2 10 0))
      (= key 81) ;;Q
      (rotate! s -0.1)
      (= key 69) ;;E
      (rotate! s 0.1)
      (= key 73) ;;I
      (alterShapeAttr! s :vel (vec2 0 -1))
      (= key 75) ;;k
      (alterShapeAttr! s :vel (vec2 0 1))
      (= key 74) ;;j
      (alterShapeAttr! s :vel (vec2 -1 0))
      (= key 76) ;;l
      (alterShapeAttr! s :vel (vec2 1 0))
      (= key 85) ;;U
      (alterShapeAttr! s :angVel -0.1)
      (= key 79) ;O
      (alterShapeAttr! s :angVel 0.1)
      (= key 90) ;Z
      (updateMass! s -1)
      (= key 88) ;;X
      (updateMass! s 1)
      (= key 67) ;C
      (alterShapeAttr! s :sticky -0.01)
      (= key 86) ;V
      (alterShapeAttr! s :sticky 0.01)
      (= key 66) ;B
      (alterShapeAttr! s :bounce -0.01)
      (= key 78) ;N
      (alterShapeAttr! s :bounce 0.01)
      (= key 77) ;M
      (set! mPositionalCorrection? (not mPositionalCorrection?))
      (= key 188) ; ;
      (set! mMovement? (not mMovement?))
      (= key 70);f
      (let [{{:keys [x y]} :center} @s
            r1 (Rectangle (vec2 x y)
                          (+ 10 (rand 30))
                          (+ 10 (rand 30)) (rand 30) (rand) (rand))]
        (alterShapeAttr! r1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 71) ;;g
      (let [{{:keys [x y]} :center} @s
            c1 (Circle (vec2 x y)
                       (+ 20 (rand 10)) (rand 30) (rand) (rand))]
        (alterShapeAttr! c1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 72);H
      (doseq [s samples
              :let [{:keys [invMass]} @s]]
        (if (pos? invMass)
          (alterShapeAttr! s :vel (vec2 (- (rand 500) 250)
                                        (- (rand 500) 250))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateUIEcho "" []
  (let [{:keys [uiEcho cur samples]} @*gWorld*
        obj (nth samples cur)
        {:keys [sticky bounce invMass angVel vel angle center]} @obj]
    (->> (str "<p><b>Selected Object:</b>:</p>"
              "<ul style=\"margin:-10px\">"
              "<li>Id: " cur "</li>"
              "<li>Center: " (:x center) "," (:y center) "</li>"
              "<li>Angle: " angle "</li>"
              "<li>Velocity: " (:x vel) "," (:y vel) "</li>"
              "<li>AngluarVelocity: " angVel "</li>"
              "<li>Mass: " (inv! invMass) "</li>"
              "<li>Friction: " sticky "</li>"
              "<li>Restitution: " bounce "</li>"
              "<li>Positional Correction: " mPositionalCorrection? "</li>"
              "<li>Movement: " mMovement? "</li>"
              "</ul> <hr>"
              "<p><b>Control</b>: of selected object</p>"
              "<ul style=\"margin:-10px\">"
              "<li><b>Num</b> or <b>Up/Down Arrow</b>: Select Object</li>"
              "<li><b>WASD</b> + <b>QE</b>: Position [Move + Rotate]</li>"
              "<li><b>IJKL</b> + <b>UO</b>: Velocities [Linear + Angular]</li>"
              "<li><b>Z/X</b>: Mass [Decrease/Increase]</li>"
              "<li><b>C/V</b>: Frictrion [Decrease/Increase]</li>"
              "<li><b>B/N</b>: Restitution [Decrease/Increase]</li>"
              "<li><b>M</b>: Positional Correction [On/Off]</li>"
              "<li><b>,</b>: Movement [On/Off]</li>"
              "</ul> <hr>"
              "<b>F/G</b>: Spawn [Rectangle/Circle] at selected object"
              "<p><b>H</b>: Excite all objects</p>"
              "<p><b>R</b>: Reset System</p>"
              "<hr>")
         (oset! uiEcho "!innerHTML" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawGame "" []
  (let [{:keys [cur samples
                width height context]} @*gWorld*
        len (count samples)]
    (ocall! context "clearRect" 0 0 width height)
    (loop [i 0]
      (when (< i len)
        (oset! context
               "!strokeStyle"
               (if (= i cur) "red" "blue"))
        (draw (nth samples i) context)
        (recur (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update! "" []
  (let [{:keys [context samples]} @*gWorld*]
    (doseq [s samples] (updateShape! s kFrameSecs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runGameLoop "" []
  (js/requestAnimationFrame #(runGameLoop))
  ;;compute how much time has elapsed since we last runGameLoop was executed
  (set! mCurrentTime (system-time))
  (set! mElapsedTime (- mCurrentTime mPreviousTime))
  (set! mPreviousTime mCurrentTime)
  (set! mLagTime (+ mLagTime mElapsedTime))
  (updateUIEcho)
  (drawGame)
  ;;Make sure we update the game the appropriate number of times.
  ;;Update only every Milliseconds per frame.
  ;;If lag larger then update frames, update until caught up.
  (while (>= mLagTime kMPF)
    ;(js/console.log "entered update loop")
    (set! mLagTime (- mLagTime kMPF))
    (checkCollision)
    (update! ))
  ;(js/console.log "end game loop")
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ^:export MyGame "" []
  (let [html (js/document.getElementById "uiEchoString")
        canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @*gWorld*
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! *gWorld* #(assoc %
                                  :uiEcho html
                                  :canvas canvas :context context))
        r1 (Rectangle (vec2 500 200) 400 20 0 0.3 0)
        r2 (Rectangle (vec2 200 400) 400 20 0 1 0.5)
        r3 (Rectangle (vec2 100 200) 200 20 0)
        r4 (Rectangle (vec2 10 360) 20 100 0 0 1)]
    (rotate! r1 2.8)
    (dotimes [i 10]
      (-> (Rectangle (vec2 (rand width)
                           (rand (/ height 2)))
                     (+ 10 (rand 50)) (+ 10 (rand 50)) (rand 30) (rand) (rand))
          (alterShapeAttr! :vel
                           (vec2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (Circle (vec2 (rand width)
                        (rand (/ height 2)))
                  (+ 10 (rand 20)) (rand 30) (rand) (rand))
          (alterShapeAttr! (vec2 (- (rand 60) 30)
                                 (- (rand 60) 30)))))
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


(set! js/userControl userControl)
(set! js/MyGame MyGame)


