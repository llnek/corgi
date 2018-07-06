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

  (:require [czlab.elmo.afx.core :as ec :refer [num?? invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [_cocos2dx? *pos-inf* *neg-inf*
                            pythag pythagSQ TWO-PI PI
                            Point2D vec2 VEC2_ZERO Edge
                            v2-len v2-add v2-sub v2-dot
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
  "" [] (atom {:depth 0 :normal VEC2_ZERO :start VEC2_ZERO :end VEC2_ZERO}))

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
(defn rigidize! "" [s & [mass friction restitution]]
  (let [{:keys [gravity samples]} @*gWorld*
        options (get @*gWorld* (:type @s))
        {:keys [draw]} options
        mass' (if (number? mass) mass 1)]
    (if (fn? draw) (swap! s #(assoc % :draw draw)))
    (swap! s
           #(assoc %
                   :invMass (invert mass')
                   :mass mass'
                   :oid (nextShapeNum)
                   :inertia 0
                   :vel VEC2_ZERO
                   :valid? true
                   :angle 0
                   :angVel 0 ;; clockwise = negative
                   :angAccel 0
                   :bxRadius 0
                   :accel (if (zero? mass') VEC2_ZERO gravity)
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
                     {:invMass 0 :vel VEC2_ZERO
                      :accel VEC2_ZERO :angVel 0 :angAccel 0})))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- validateShape "" [s]
  (let [{{:keys [x y]} :pos} @s
        {{:keys [top right bottom left]} :arena} @*gWorld*]
    (if (or (< x left)
            (> x right)
            (if _cocos2dx?
              (or (> y top) (< y bottom))
              (or (< y top) (> y bottom))))
      (swap! s #(assoc % :valid? false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [height width samples validator]} @*gWorld*
        {:keys [oid]} @s]
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
    ;;;;;;
    (validator s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [s1 s2]
  (let [{p1 :pos r1 :bxRadius} @s1
        {p2 :pos r2 :bxRadius} @s2
        v1to2 (v2-sub p2 p1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findSPoint?? "" [r1 dir ptOnEdge]
  (let [{:keys [edges]} @r1
        len (count edges)]
    (loop [spDist *neg-inf* sp nil i 0]
      (if (>= i len)
        [spDist sp]
        (let [e' (nth edges i)
              {v' :v1} @e'
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
  (let [{:keys [edges normals]} @r1
        len (count normals)
        [bestDist supportPoint bestIndex hasSupport?]
        (loop [bestDist *pos-inf*
               suPt nil bestIdx nil hasSupport? true i 0]
          (if-not (and (< i len)
                       hasSupport?)
            [bestDist suPt bestIdx hasSupport?]
            (let [n (nth normals i)
                  ii (+ i 1)
                  dir (v2-negate n)
                  e' (nth edges i)
                  {ptOnEdge :v1} @e'
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
  (let [{:keys [radius pos]} @cc1
        {:keys [normals]} @r1
        n (nth normals nEdge)
        rVec (v2-scale n radius)]
    (chgci! ci (- radius bestDist) n (v2-sub pos rVec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nfaceToCircle?? "" [r1 cc1 ci]
  (let [{:keys [edges normals]} @r1
        {center :pos} @cc1
        len (count normals)]
    (loop [inside? true
           bestDist *neg-inf* nEdge 0 i 0]
      (if (or (not inside?) (>= i len))
        [inside? bestDist nEdge]
        (let [e' (nth edges i)
              v (v2-sub center (:v1 @e'))
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
        {:keys [normals edges]} @r1
        vn (:v1 (deref (nth edges nEdge)))
        en (nth normals nEdge)
        ;;V1 is from left vertex of face to center of circle
        ;;V2 is from left vertex of face to right vertex of face
        len (count normals)
        eX (mod (+ 1 nEdge) len)
        vX (:v1 (deref (nth edges eX)))
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
  (let [{center :pos :keys [angle edges normals]} @s
        [v0 v1 v2 v3]
        (map (fn [e] (:v1 @e)) edges)
        a2 (+ angle angle')
        [p0 p1 p2 p3 :as vs]
        [(v2-rot v0 center angle')
         (v2-rot v1 center angle')
         (v2-rot v2 center angle')
         (v2-rot v3 center angle')]]
    (swap! s
           #(assoc %
                   :angle a2
                   :edges
                   [(Edge p0 p1)
                    (Edge p1 p2)
                    (Edge p2 p3)
                    (Edge p3 p0)]
                   :normals (createFaceNormals vs))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectMove "" [s p]
  (let [{center :pos :keys [edges]} @s
        [v0 v1 v2 v3]
        (map (fn [e] (:v1 @e)) edges)
        [p0 p1 p2 p3]
        [(v2-add v0 p)
         (v2-add v1 p)
         (v2-add v2 p)
         (v2-add v3 p)]]
    (swap! s #(assoc %
                     :pos (v2-add center p)
                     :edges [(Edge p0 p1)
                             (Edge p1 p2)
                             (Edge p2 p3)
                             (Edge p3 p0)])) s))

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
        [(Point2D left top) (Point2D right top)
         (Point2D right bottom) (Point2D left bottom)]]
    (->>
      #(assoc %
              :updateInertia rectUpdateInertia
              :collisionTest rectCollisionTest
              :move rectMove
              :rotate rectRotate
              :bxRadius (/ (pythag width height) 2)
              :edges [(Edge v0 v1)(Edge v1 v2)
                      (Edge v2 v3)(Edge v3 v0)]
              :normals (createFaceNormals vs)) (swap! s))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [edges & [mass]] (atom {:edges edges :invMass (invert mass)}))

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
(defn- correctPos! "" [posCorrection s1 s2 ci]
  (let [{:keys [depth normal]} @ci
        {m1 :invMass} @s1
        {m2 :invMass} @s2
        n (* posCorrection
             (/ depth (+ m1 m2)))
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
(defn- resolveCollision "" [posCorrection s1 s2 ci]
  (when-not (and (zero? (:invMass @s1))
                 (zero? (:invMass @s2)))
    (correctPos! posCorrection s1 s2 ci)
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
(defn- checkCollision* "" [posCorrection]
  (let [{:keys [samples context]} @*gWorld*
        len (ec/countStore samples)
        ci (ci-info)]
    (dotimes [i len]
      (loop [j (inc i)]
        (when-not (>= j len)
          (let [si (ec/nthStore samples i)
                sj (ec/nthStore samples j)]
            (when (and (:valid? @si)
                       (:valid? @sj)
                       (overlap? si sj)
                       (collisionTest?? si sj ci))
              ;;make sure the normal is always si -> sj
              (if (neg? (v2-dot (:normal @ci)
                                (v2-sub (:pos @sj)
                                        (:pos @si)))) (revCIDir! ci))
              (resolveCollision posCorrection si sj ci)))
          (recur (+ 1 j)))))))

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
(defn runAlgo "" [algoIterCount posCorrection]
  (dotimes [_ algoIterCount] (checkCollision* posCorrection))
  (let [{:keys [context
                samples
                frameSecs]} @*gWorld* bin #js []]
    (ec/eachStore samples
                  (fn [s i]
                    (if-not (:valid? @s)
                      (.push bin s)
                      (updateShape! s frameSecs))))
    (when (pos? (count bin))
      (doseq [b bin] (ec/delFromStore! samples b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private prevMillis (system-time))
(def ^:private lagMillis 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" [dt & [algoIterCount posCorrection]]
  (set! prevMillis (system-time))
  (set! lagMillis (+ lagMillis dt))
  ;;Make sure we update the game the appropriate number of times.
  ;;Update only every Milliseconds per frame.
  ;;If lag larger then update frames, update until caught up.
  (let [{:keys [algoRunner frameMillis]} @*gWorld*
        iterCnt (num?? algoIterCount 10)
        posCorrect (num?? posCorrection 0.8)
        R (if (fn? algoRunner) algoRunner runAlgo)]
    (while (>= lagMillis frameMillis)
      (set! lagMillis
            (- lagMillis frameMillis)) (R iterCnt posCorrect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step*
  "" [& [algoIterCount posCorrection]]
  (step (- (system-time) prevMillis) algoIterCount posCorrection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (let [{:keys [cc2dx? validator]} options
        {:keys [top right bottom left]} world
        options' (dissoc options :cc2dx? :validator)]
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
;;EOF


