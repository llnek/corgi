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

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [pythag pythagSQ TWO-PI PI vec2 VEC_ZERO _cocos2dx?
                            v2-len v2-add v2-sub v2-dot Point2D Size2D
                            v2-negate v2-scale v2-cross v2-rot v2-norm v2-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *shapeNum* (atom 0))
(defn- nextShapeNum "" [] (swap! *shapeNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *gWorld* (atom {:samples (ec/createStore 10)
                               :context nil :cur 0 :canvas nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info
  "" [] (atom {:depth 0 :normal VEC_ZERO :start VEC_ZERO :end VEC_ZERO}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- chgci! "" [ci d n s]
  (swap! ci
         #(assoc %
                 :depth d :normal n
                 :start s :end (v2-add s (v2-scale n d)))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- revCIDir! "" [ci]
  (swap! ci
         (fn [{:keys [start end normal] :as root}]
           (assoc root
                  :start end :end start :normal (v2-negate normal)))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rigidize! "" [s & [mass friction restitution]]
  (let [{:keys [gravity samples]} @*gWorld*
        options (get @*gWorld* (:type @s))
        {:keys [draw]} options
        mass' (if (number? mass) mass 1)]
    (if (fn? draw) (swap! s #(assoc % :draw draw)))
    (swap! s
           #(assoc %
                   :invMass (invert mass')
                   :oid (nextShapeNum)
                   :inertia 0
                   :vel VEC_ZERO
                   :valid? true
                   :angle 0
                   :angVel 0 ;; clockwise = negative
                   :angAccel 0
                   :bxRadius 0
                   :accel (if (zero? mass') VEC_ZERO gravity)
                   :sticky (if (number? friction) friction 0.8)
                   :bounce (if (number? restitution) restitution 0.2)))
    (ec/addToStore! samples s) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionTest?? "" [s1 s2 ci] ((:collisionTest @s1) s1 s2 ci))
(defn updateInertia! "" [s] ((:updateInertia @s) s) s)
(defn draw "" [s & more] (apply gx/drawShape (concat [s] more)))
(defn move! "" [s p] ((:move @s) s p) s)
(defn rotate! "" [s v] ((:rotate @s) s v) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass! "" [s delta]
  (let [{:keys [gravity]} @*gWorld*
        {:keys [invMass]} @s
        m (+ (invert invMass) delta)]
    (swap! s
           #(merge %
                   (if (pos? m)
                     {:invMass (invert m) :accel gravity}
                     {:invMass 0 :vel VEC_ZERO
                      :accel VEC_ZERO :angVel 0 :angAccel 0})))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- validateShape "" [s]
  (let [{{:keys [x y]} :pos} @s
        {:keys [top right bottom left]} @*gWorld*]
    (if (or (< x left)
            (> x right)
            (if _cocos2dx?
              (or (> y top) (< y bottom))
              (or (< y top) (> y bottom))))
      (swap! s #(assoc % :valid? false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [height width samples validator]} @*gWorld*]
    (when true
      ;;update vel += a*t
      (swap! s (fn [{:keys [vel accel] :as root}]
                 (assoc root :vel (v2-add vel (v2-scale accel dt)))))
      ;;move object += v*dt
      (move! s (v2-scale (:vel @s) dt))
      ;;update angular vel
      (swap! s (fn [{:keys [angVel angAccel] :as root}]
                 (assoc root :angVel (+ angVel (* angAccel dt)))))
      ;rotate object
      (rotate! s (* (:angVel @s) dt)))
    (if (fn? validator) (validator s) (validateShape s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [s1 s2]
  (let [{p1 :pos r1 :bxRadius} @s1
        {p2 :pos r2 :bxRadius} @s2
        v1to2 (v2-sub p2 p1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findSPoint?? "" [r1 dir ptOnEdge]
  (let [{:keys [vertices]}
        @r1
        len (count vertices)]
    (loop [spDist js/Number.NEGATIVE_INFINITY sp nil i 0]
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
        [bestDist supportPoint bestIndex hasSupport?]
        (loop [bestDist js/Number.POSITIVE_INFINITY
               suPt nil bestIdx nil hasSupport? true i 0]
          (if-not (and (< i len)
                       hasSupport?)
            [bestDist suPt bestIdx hasSupport?]
            (let [n (nth normals i)
                  ii (+ i 1)
                  dir (v2-negate n)
                  ptOnEdge (nth vertices i)
                  [dist pt]
                  (findSPoint?? r2  dir ptOnEdge)
                  sp? (not (nil? pt))]
              (if (and sp? (< dist bestDist))
                (recur dist pt i sp? ii)
                (recur bestDist suPt bestIdx sp? ii)))))]
    (when hasSupport? ;;all four directions have support points
      (let [bn (nth normals bestIndex)
            bv (v2-scale bn bestDist)]
        (chgci! ci bestDist bn (v2-add supportPoint bv)))) hasSupport?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- testRectRect?
  "Check for collision between 2 rectangles"
  [r1 r2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (ci-info)
        ci_2 (ci-info)
        p1? (hasAxisLeastPenetration? r1 r2 ci_1)
        p2? (if p1? (hasAxisLeastPenetration? r2 r1 ci_2))]
    (when p2?
      ;;if both of rectangles are overlapping,
      ;;choose the shorter normal as the normal
      (let [{d1 :depth n1 :normal s1 :start} @ci_1
            {d2 :depth n2 :normal s2 :start} @ci_2]
        (if (< d1 d2)
          (chgci! ci d1 n1 (v2-sub s1 (v2-scale n1 d1)))
          (chgci! ci d2 (v2-negate n2) s2))))
    (and p1? p2?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleInsideRect? "" [r1 cc1 ci nEdge bestDist]
  (let [{:keys [normals vertices]} @r1
        {:keys [radius pos]} @cc1
        n (nth normals nEdge)
        rVec (v2-scale n radius)]
    (chgci! ci (- radius bestDist) n (v2-sub pos rVec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nfaceToCircle?? "" [r1 cc1 ci]
  (let [{:keys [vertices normals]} @r1
        {center :pos} @cc1
        len (count normals)]
    (loop [inside? true
           bestDist js/Number.NEGATIVE_INFINITY nEdge 0 i 0]
      (if (or (not inside?) (>= i len))
        [inside? bestDist nEdge]
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
  (let [{center :pos :keys [radius]} @cc1
        {:keys [normals vertices]} @r1
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
          (do (chgci! ci (- radius dis) n (v2-add center rVec)) true)))
      ;;;else
      ;the center of circle is in corner region of vertex[nEdge+1]
      ;v1 is from right vertex of face to center of circle
      ;v2 is from right vertex of face to left vertex of face
      (let [v1 (v2-sub center vX)
            v2 (v2-negate V2)
            dot (v2-dot v1 v2)]
        (cond
          (neg? dot)
          (let [dis (v2-len v1)
                n (v2-norm v1)
                rVec (v2-scale n (- radius))]
            (if (> dis radius)
              false
              (do (chgci! ci (- radius dis) n (v2-add center rVec)) true)))
          ;;the circle is in face region of face[nEdge]
          (< bestDist radius)
          (let [rVec (v2-scale en radius)]
            (do (chgci! ci (- radius bestDist) en (v2-sub center rVec)) true))
          :else false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedRectCirc "" [r1 cc1 ci]
  (let [[inside? bestDist nEdge] (nfaceToCircle?? r1 cc1 ci)]
    (if inside?
      (circleInsideRect? r1 cc1 ci nEdge bestDist)
      (circleOutsideRect? r1 cc1 ci nEdge bestDist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedRectCirc s1 s2 ci) (testRectRect? s1 s2 ci)))

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
  (let [{center :pos :keys [angle vertices normals]} @s
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
  (let [{center :pos :keys [vertices]} @s
        [v0 v1 v2 v3] vertices]
    (swap! s #(assoc %
                     :pos (v2-add center p)
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
                              (invert (/ (* (invert invMass)
                                          (pythagSQ width height)) 12))))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz & [mass friction restitution]]
  (let [s (-> (gx/Rectangle pt sz)
              (rigidize! mass friction restitution))
        {:keys [width height width_2 height_2] {:keys [x y]} :pos} @s
        bottom (if _cocos2dx? (- y height_2) (+ y height_2))
        top (if _cocos2dx? (+ y height_2) (- y height_2))
        right (+ x width_2)
        left (- x width_2)
        [v0 v1 v2 v3 :as vs]
        [(vec2 left top) (vec2 right top)
         (vec2 right bottom) (vec2 left bottom)]]
    (->>
      #(assoc %
              :updateInertia rectUpdateInertia
              :collisionTest rectCollisionTest
              :move rectMove
              :rotate rectRotate
              :bxRadius (/ (pythag width height) 2)
              :vertices vs
              :normals (createFaceNormals vs)) (swap! s))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;circle-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleMove "" [c p]
  (let [{center :pos :keys [startPt]} @c]
    (swap! c
           #(assoc %
                   :pos (v2-add center p)
                   :startPt (v2-add startPt p))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleRotate "rotate angle in counterclockwise" [c angle']
  (let [{center :pos :keys [angle startPt]} @c]
    (swap! c
           #(assoc %
                   :angle (+ angle angle')
                   :startPt (v2-rot startPt center angle'))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleUpdateInertia "" [c]
  (let [{:keys [invMass radius]} @c
        n (if (zero? invMass) 0 (* (invert invMass) radius radius))]
    ;;why 12?
    (swap! c #(assoc % :inertia (/ n 12))) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedCircCirc "" [cc1 cc2 ci]
  (let [{c1 :pos r1 :radius} @cc1
        {c2 :pos r2 :radius} @cc2
        v1to2  (v2-sub c2 c1)
        rSum  (+ r1 r2)
        dist  (v2-len v1to2)]
    (cond
      (> dist rSum) ;;no overlap
      false
      (zero? dist) ;;centers overlap
      (do (chgci! ci rSum
                  (vec2 0 -1)
                  (if (> r1 r2)
                    (v2-add c1 (vec2 0 r1)) (v2-add c2 (vec2 0 r2)))) true)
      :else ;overlap
      (let [rC2 (-> (v2-norm (v2-negate v1to2)) (v2-scale r2))]
        (chgci! ci (- rSum dist) (v2-norm v1to2) (v2-add c2 rC2)) true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedCircCirc s1 s2 ci) (collidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius & [mass friction restitution]]
  (let [s (-> (gx/Circle pt radius)
              (rigidize! mass friction restitution))
        {{:keys [x y]} :pos} @s
        top (if _cocos2dx? (+ y radius) (- y radius))]
    (swap! s
           #(merge %
                   {:collisionTest circleCollisionTest
                    :updateInertia circleUpdateInertia
                    :rotate  circleRotate
                    :move circleMove
                    :bxRadius radius
                    :startPt (vec2 x top)}))
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
    (let [tangent (->> (v2-dot rVelocity normal)
                       (v2-scale normal)
                       (v2-sub rVelocity)
                       (v2-norm)
                       (v2-negate))
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
    (correctPos! s1 s2 ci)
    ;;the direction of collisionInfo is always from s1 to s2
    ;;but the Mass is inversed, so start scale with s2 and end scale with s1
    (let
      [{:keys [normal start end]} @ci
       {m1 :invMass c1 :pos vs1 :vel av1 :angVel} @s1
       {m2 :invMass c2 :pos vs2 :vel av2 :angVel} @s2
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
        len (ec/countStore samples)
        ci (ci-info)]
    (loop [i 0]
      (when-not (>= i len)
        (loop [j (+ 1 i)]
          (when-not (>= j len)
            (let [si (ec/nthStore samples i)
                  sj (ec/nthStore samples j)]
              (when (and (:valid? @si)
                         (:valid? @sj)
                         (overlap? si sj)
                         (collisionTest?? si sj ci))
                ;;make sure the normal is always from object[i] to object[j]
                (if (neg? (v2-dot (:normal @ci)
                                  (v2-sub (:pos @sj)
                                          (:pos @si)))) (revCIDir! ci))
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
        len (ec/countStore samples)
        s (ec/nthStore samples cur)
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
      (= key 70);f
      (let [{:keys [pos]} @s
            r1 (Rectangle pos
                          (Size2D (+ 10 (rand 30))
                                  (+ 10 (rand 30))) (rand 30) (rand) (rand))]
        (alterShapeAttr! r1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 71) ;;g
      (let [{:keys [pos]} @s
            c1 (Circle pos
                       (+ 20 (rand 10)) (rand 30) (rand) (rand))]
        (alterShapeAttr! c1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 72);H
      (ec/eachStore samples
                    (fn [s i]
                      (if (pos? (:invMass @s))
                        (alterShapeAttr! s
                                         :vel (vec2 (- (rand 500) 250)
                                                    (- (rand 500) 250)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateUIEcho "" []
  (let [{:keys [uiEcho cur samples]} @*gWorld*
        obj (ec/nthStore samples cur)
        {:keys [sticky bounce invMass angVel vel angle pos]} @obj]
    (->> (str "<p><b>Selected Object:</b>:</p>"
              "<ul style=\"margin:-10px\">"
              "<li>Id: " cur "</li>"
              "<li>Center: " (:x pos) "," (:y pos) "</li>"
              "<li>Angle: " angle "</li>"
              "<li>Velocity: " (:x vel) "," (:y vel) "</li>"
              "<li>AngluarVelocity: " angVel "</li>"
              "<li>Mass: " (invert invMass) "</li>"
              "<li>Friction: " sticky "</li>"
              "<li>Restitution: " bounce "</li>"
              "</ul> <hr>"
              "<p><b>Control</b>: of selected object</p>"
              "<ul style=\"margin:-10px\">"
              "<li><b>Num</b> or <b>Up/Down Arrow</b>: Select Object</li>"
              "<li><b>WASD</b> + <b>QE</b>: Position [Move + Rotate]</li>"
              "<li><b>IJKL</b> + <b>UO</b>: Velocities [Linear + Angular]</li>"
              "<li><b>Z/X</b>: Mass [Decrease/Increase]</li>"
              "<li><b>C/V</b>: Frictrion [Decrease/Increase]</li>"
              "<li><b>B/N</b>: Restitution [Decrease/Increase]</li>"
              "</ul> <hr>"
              "<b>F/G</b>: Spawn [Rectangle/Circle] at selected object"
              "<p><b>H</b>: Excite all objects</p>"
              "<p><b>R</b>: Reset System</p>"
              "<hr>")
         (oset! uiEcho "!innerHTML" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawGame "" []
  (let [{:keys [cur samples width height context]} @*gWorld*]
    (ocall! context "clearRect" 0 0 width height)
    (ec/eachStore samples
                  (fn [s i]
                    (when (:valid? @s)
                      (oset! context
                             "!strokeStyle" (if (= i cur) "red" "blue"))
                      (draw s context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update! "" []
  (let [{:keys [context samples frameSecs]} @*gWorld*
        bin #js []]
    (ec/eachStore samples
                  (fn [s i]
                    (if-not (:valid? @s)
                      (.push bin s)
                      (updateShape! s frameSecs))))
    (when (pos? (count bin))
      (doseq [b bin] (ec/delFromStore! samples b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private prevMillis (system-time))
(def ^:private nowMillis 0)
(def ^:private lagMillis 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" [dt]
  (set! prevMillis (system-time))
  (set! lagMillis (+ lagMillis dt))
  ;;Make sure we update the game the appropriate number of times.
  ;;Update only every Milliseconds per frame.
  ;;If lag larger then update frames, update until caught up.
  (let [{:keys [frameMillis]} @*gWorld*]
    (while (>= lagMillis frameMillis)
      (set! lagMillis (- lagMillis frameMillis))
      (checkCollision)
      (update! ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runGameLoop "" []
  (js/requestAnimationFrame #(runGameLoop))
  (updateUIEcho)
  (drawGame)
  (step (- (system-time) prevMillis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (let [{:keys [cc2dx? validator]} options
        {:keys [top right bottom left]} world
        options' (dissoc options cc2dx? validator)]
    (if (true? cc2dx?) (set! _cocos2dx? true))
    (swap! *gWorld*
           (fn [root]
             (-> (merge root options')
                 (assoc :validator (if (fn? validator) validator validateShape)
                        :arena world
                        :FPS fps
                        :width (+ 1 (- right left))
                        :height (+ 1 (if _cocos2dx? (- top bottom) (- bottom top)))
                        :gravity (vec2 0 gravity)
                        :frameSecs (invert fps)
                        :frameMillis (* 1000 (invert fps)))))) *gWorld*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- myGame "" []
  (initPhysics 20 60 {:left 0 :right 799 :top 0 :bottom 449})
  (let [html (js/document.getElementById "uiEchoString")
        canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @*gWorld*
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! *gWorld* #(assoc %
                                  :uiEcho html
                                  :canvas canvas :context context))
        r1 (Rectangle (Point2D 500 200) (Size2D 400 20) 0 0.3 0)
        r2 (Rectangle (Point2D 200 400) (Size2D 400 20) 0 1 0.5)
        r3 (Rectangle (Point2D 100 200) (Size2D 200 20) 0)
        r4 (Rectangle (Point2D 10 360) (Size2D 20 100) 0 0 1)]
    (rotate! r1 2.8)
    (dotimes [i 4]
      (-> (Rectangle (Point2D (rand width)
                              (rand (/ height 2)))
                     (Size2D (+ 10 (rand 50))
                             (+ 10 (rand 50))) (rand 30) (rand) (rand))
          (alterShapeAttr! :vel
                           (vec2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (Circle (Point2D (rand width)
                           (rand (/ height 2)))
                  (+ 10 (rand 20)) (rand 30) (rand) (rand))
          (alterShapeAttr! (vec2 (- (rand 60) 30)
                                 (- (rand 60) 30)))))
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(set! js/userControl userControl)
(set! js/MyGame myGame)


