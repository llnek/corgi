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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do->true]])

  (:require [czlab.elmo.afx.core :as ec :refer [n# num?? invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [_cocos2dx? *pos-inf* *neg-inf*
                            pythag pythagSQ TWO-PI PI
                            Point2D vec2 V2_ZERO Edge
                            v2-len v2-add v2-sub v2-dot
                            v2-neg v2-scale v2-xss v2-rot v2-norm v2-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *shapeNum* (atom 0))
(defn- nextShapeNum "" [] (swap! *shapeNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *gWorld* (atom {:samples (ec/createStore 10)
                               :context nil :canvas nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info
  "" [] (atom {:depth 0 :normal V2_ZERO :start V2_ZERO :end V2_ZERO}))

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
                  :start end :end start :normal (v2-neg normal)))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setFixed! "" [obj] (swap! obj #(assoc % :dynamic? false)) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dynamic? "" [obj] (true? (:dynamic? @obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rigidBody! "" [obj & [mass friction bounce]]
  (let [opts (get @*gWorld* (:type @obj))
        mass' (if (number? mass) mass 1)
        {:keys [gravity samples]} @*gWorld*]
    (swap! obj
           #(assoc %
                   :invMass (invert mass')
                   :oid (nextShapeNum)
                   :vel V2_ZERO
                   :dynamic? true
                   :valid? true
                   :mass mass'
                   :inertia 0
                   :angle 0
                   :gvel 0 ;; clockwise = negative
                   :gaccel 0
                   :bxRadius 0
                   :accel (if (zero? mass') V2_ZERO gravity)
                   :sticky (if (number? friction) friction 0.8)
                   :bounce (if (number? bounce) bounce 0.2)))
    (if (map? opts) (swap! obj #(merge % opts)))
    (ec/addToStore! samples obj) obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionTest?? "" [s1 s2 ci] ((:collisionTest @s1) s1 s2 ci))
(defn updateInertia! "" [s] ((:updateInertia @s) s) s)
(defn draw "" [s & more] (apply gx/drawShape (concat [s] more)))
(defn move! "" [s p] ((:move @s) s p) s)
(defn rotate! "" [s v] ((:rotate @s) s v) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass! "" [s delta]
  (let [{:keys [gravity]} @*gWorld*
        {:keys [mass]} @s
        m (+ mass delta)]
    (swap! s
           #(merge %
                   (if (pos? m)
                     {:invMass (invert m) :mass m :accel gravity}
                     {:invMass 0 :mass 0 :vel V2_ZERO
                      :accel V2_ZERO :gvel 0 :gaccel 0})))
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
      (swap! s (fn [{:keys [gvel gaccel] :as root}]
                 (assoc root :gvel (+ gvel (* gaccel dt)))))
      ;rotate object
      (rotate! s (* (:gvel @s) dt)))
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
(defn- spoint?? "" [r1Pt n r2]
  (let [{:keys [vertices]} @r2
        len (n# vertices)
        dir (v2-neg n)] ;easier to deal with +ve values
    (loop [dist *neg-inf* sp nil i 0]
      (if (>= i len)
        [(some? sp) dist sp]
        (let [v' (nth vertices i)
              ii (+ i 1)
              proj (v2-dot (v2-sub v' r1Pt) dir)]
          ;;find the longest +ve distance with edge
          (if (and (pos? proj)
                   (> proj dist))
            (recur proj v' ii) (recur dist sp ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findPenetration??
  "Find the shortest axis that's overlapping" [r1 r2]
  (let [{:keys [vertices normals]} @r1
        len (n# normals)]
    ;;all vertices have corresponding support points?
    (loop [depth *pos-inf*
           vert nil n' nil support? true i 0]
      (if-not (and support? (< i len))
        (if support?
          (chgci! (ci-info) depth n'
                  (v2-add vert (v2-scale n' depth))))
        (let [v' (nth vertices i)
              ii (+ i 1)
              dir (nth normals i)
              [ok? dist pt] (spoint?? v' dir r2)]
          (if (and ok? (< dist depth))
            (recur dist pt (nth normals i) true ii)
            (recur depth vert n' ok? ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- testRectRect?
  "Check for collision between 2 rectangles"
  [r1 r2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (findPenetration?? r1 r2)
        ci_2 (if (some? ci_1) (findPenetration?? r2 r1))]
    ;;if both are overlapping, choose the shorter normal
    (when (some? ci_2)
      (let [{d1 :depth n1 :normal s1 :start} @ci_1
            {d2 :depth n2 :normal s2 :start} @ci_2]
        (if (< d1 d2)
          (chgci! ci d1 n1 (v2-sub s1 (v2-scale n1 d1)))
          (chgci! ci d2 (v2-neg n2) s2))))
    (and (some? ci_1)(some? ci_2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleInsideRect? "" [r1 cc1 ci nEdge depth]
  (let [{:keys [radius pos]} @cc1
        {:keys [normals]} @r1
        n (nth normals nEdge)
        rVec (v2-scale n radius)]
    (chgci! ci (- radius depth) n (v2-sub pos rVec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nfaceToCircle?? "" [r1 cc1]
  (let [{:keys [vertices normals]} @r1
        {center :pos} @cc1
        len (n# normals)]
    (loop [inside? true
           depth *neg-inf* nEdge 0 i 0]
      (if (or (not inside?) (>= i len))
        [inside? depth nEdge]
        (let [e' (nth vertices i)
              v (v2-sub center e')
              ii (+ i 1)
              proj (v2-dot v (nth normals i))]
          (cond
            ;;the center of circle is outside of rectangle
            (pos? proj) (recur false proj i ii)
            (> proj depth) (recur inside? proj i ii)
            :else (recur inside? depth nEdge ii)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleOutsideRect? "" [r1 cc1 ci nEdge depth]
  (let [{center :pos :keys [radius]} @cc1
        {:keys [normals vertices]} @r1
        vn (nth vertices nEdge)
        en (nth normals nEdge)
        ;;V1 is from left vertex of face to center of circle
        ;;V2 is from left vertex of face to right vertex of face
        len (n# normals)
        eX (gx/wrap?? nEdge len)
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
            v2 (v2-neg V2)
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
          (< depth radius)
          (let [rVec (v2-scale en radius)]
            (do (chgci! ci (- radius depth) en (v2-sub center rVec)) true))
          :else false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedRectCirc "" [r1 cc1 ci]
  (let [[inside? depth nEdge] (nfaceToCircle?? r1 cc1)]
    (if inside?
      (circleInsideRect? r1 cc1 ci nEdge depth)
      (circleOutsideRect? r1 cc1 ci nEdge depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedRectCirc s1 s2 ci) (testRectRect? s1 s2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rectangle stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createFaceNormals

  "0--Top;1--Right;2--Bottom;3--Left"
  [vertices]

  (let [[v0 v1 v2 v3] vertices]
    [(v2-norm (v2-sub v1 v2)) (v2-norm (v2-sub v2 v3))
     (v2-norm (v2-sub v3 v0)) (v2-norm (v2-sub v0 v1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectRotate "" [s angle']
  (let [{center :pos :keys [angle vertices normals]} @s
        [v0 v1 v2 v3] vertices
        a2 (+ angle angle')
        vs'
        [(v2-rot v0 center angle')
         (v2-rot v1 center angle')
         (v2-rot v2 center angle')
         (v2-rot v3 center angle')]]
    (swap! s
           #(assoc %
                   :angle a2
                   :vertices vs'
                   :normals (createFaceNormals vs'))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectMove "" [s p]
  (let [{center :pos :keys [vertices]} @s
        [v0 v1 v2 v3] vertices
        vs'
        [(v2-add v0 p)
         (v2-add v1 p)
         (v2-add v2 p)
         (v2-add v3 p)]]
    (swap! s #(assoc %
                     :pos (v2-add center p)
                     :vertices vs')) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectUpdateInertia "" [s]
  (let [{:keys [width height mass invMass]} @s]
    (swap! s
           #(assoc %
                   :inertia (if (zero? mass)
                              0
                              (invert (/ (* mass
                                            (pythagSQ width height)) 12))))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz & [mass friction bounce]]
  (let [s (-> (gx/Rectangle pt sz)
              (rigidBody! mass friction bounce))
        {:keys [vertices width height]} @s]
    (->>
      #(assoc %
              :updateInertia rectUpdateInertia
              :collisionTest rectCollisionTest
              :move rectMove
              :rotate rectRotate
              :bxRadius (/ (pythag width height) 2)
              :normals (createFaceNormals vertices)) (swap! s))
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
  (let [{:keys [mass radius]} @c
        n (if (zero? mass) 0 (* mass radius radius))]
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
      (do->true (chgci! ci rSum (vec2 0 -1)
                        (if (> r1 r2)
                          (v2-add c1 (vec2 0 r1)) (v2-add c2 (vec2 0 r2)))))
      :else ;overlap
      (do->true
        (let [rC2 (-> (v2-norm (v2-neg v1to2)) (v2-scale r2))]
          (chgci! ci (- rSum dist) (v2-norm v1to2) (v2-add c2 rC2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedCircCirc s1 s2 ci) (collidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius & [mass friction bounce]]
  (let [s (-> (gx/Circle pt radius)
              (rigidBody! mass friction bounce))
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
        bounce' (min b1 b2)
        sticky' (min f1 f2)
        ;;R cross N
        r1xN (v2-xss r1 normal)
        r2xN (v2-xss r2 normal)
        ;;Calc impulse scalar
        ;;the formula of jN can be found in http://www.myphysicslab.com/collision.html
        jN (/ (* (- (+ 1 bounce')) rVelocityInNormal)
              (+ m1 m2 (* r1xN r1xN e1) (* r2xN r2xN e2)))
        ;;impulse is in direction of normal ( from s1 to s2)
        ;;impulse = F dt = m * ?v , ?v = impulse / m
        impulse (v2-scale normal jN)]
    (swap! s1
           (fn [{:keys [gvel vel] :as root}]
             (assoc root
                    :gvel (- gvel (* r1xN jN e1))
                    :vel (v2-sub vel (v2-scale impulse m1)))))
    (swap! s2
           (fn [{:keys [gvel vel] :as root}]
             (assoc root
                    :gvel (+ gvel (* r2xN jN e2))
                    :vel (v2-add vel (v2-scale impulse m2)))))
    ;;rVelocity.dot(tangent) should less than 0
    (let [tangent (->> (v2-dot rVelocity normal)
                       (v2-scale normal)
                       (v2-sub rVelocity)
                       (v2-norm)
                       (v2-neg))
          r1xT (v2-xss r1 tangent)
          r2xT (v2-xss r2 tangent)
          jT' (/ (* (- (+ 1 bounce')) (v2-dot rVelocity tangent) sticky')
                 (+ m1 m2 (* r1xT r1xT e1) (* r2xT r2xT e2)))
          ;;friction should less than force in normal direction
          jT (if (> jT' jN) jN jT')
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (v2-scale tangent jT)]
      (swap! s1
             (fn [{:keys [gvel vel] :as root}]
               (assoc root
                      :gvel (- gvel (* r1xT jT e1))
                      :vel (v2-sub vel (v2-scale impulse m1)))))
      (swap! s2
             (fn [{:keys [gvel vel] :as root}]
               (assoc root
                      :gvel (+ gvel (* r2xT jT e2))
                      :vel (v2-add vel (v2-scale impulse m2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [posCorrection s1 s2 ci]
  (when-not (and (zero? (:mass @s1))
                 (zero? (:mass @s2)))
    (correctPos! posCorrection s1 s2 ci)
    ;;the direction of collisionInfo is always from s1 to s2
    ;;but the Mass is inversed, so start scale with s2 and end scale with s1
    (let
      [{:keys [normal start end]} @ci
       {m1 :invMass c1 :pos vs1 :vel av1 :gvel} @s1
       {m2 :invMass c2 :pos vs2 :vel av2 :gvel} @s2
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
  (let [{:keys [gvel vel]} @s
        p1 (first more)
        v (cond (= :gvel attr) (+ gvel p1)
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
    (when (pos? (n# bin))
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


