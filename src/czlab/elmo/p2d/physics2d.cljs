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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 do->true assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [n# num?? invert]]
            [czlab.elmo.p2d.core
             :as pc :refer [*gWorld* rigidBody!
                            static? updateInertia! draw move! rotate!]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [_cocos2dx? *pos-inf* *neg-inf*
                            pythag pythagSQ TWO-PI PI
                            Point2D vec2 V2_ZERO
                            v2-len v2-add v2-sub v2-dot
                            v2-neg v2-scale v2-xss v2-rot v2-norm v2-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionTest?? "" [s1 s2 ci] ((:collisionTest @s1) s1 s2 ci))


(def ^:private MI_SPREAD 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [&[d n s e]]
  (atom {:depth (num?? d 0)
         :normal (or n V2_ZERO)
         :start (or s V2_ZERO) :end (or e V2_ZERO)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- chgci! "" [ci d n s]
  (assoc!! ci
           :depth d :normal n
           :start s :end (v2-add s (v2-scale n d))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- revCIDir! "" [ci]
  (let [{:keys [start end normal]} @ci]
    (assoc!! ci
             :start end :end start :normal (v2-neg normal)) ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- validateShape "" [s]
  (let [{{:keys [x y]} :pos} @s
        {{:keys [top right bottom left]} :arena} @*gWorld*]
    (if (or (< x left)
            (> x right)
            (if _cocos2dx?
              (or (> y top) (< y bottom))
              (or (< y top) (> y bottom))))
      (assoc!! s :valid? false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateShape! "" [s dt]
  (let [{:keys [height width samples validator]} @*gWorld*
        {:keys [oid vel accel gvel gaccel]} @s
        gv' (+ gvel (* gaccel dt))
        v' (v2-add vel (v2-scale accel dt))]
    (when true
      ;;update vel += a*t
      ;;move object += v*dt
      ;;update angular vel
      ;rotate object
      (assoc!! s :gvel gv')
      (assoc!! s :vel v')
      (move! s (v2-scale v' dt))
      (rotate! s (* gv' dt)))
    ;;;;;;
    (if (fn? validator) (validator s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [s1 s2]
  (let [{p1 :pos r1 :bxRadius} @s1
        {p2 :pos r2 :bxRadius} @s2
        v1to2 (v2-sub p2 p1)] (not (> (v2-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- spoint?? "" [r1Pt n r2]
  (let [{:keys [vertices]} @r2]
    ;easier to deal with +ve values
    (loop [i 0 SZ (n# vertices)
           dir (v2-neg n) dist *neg-inf* sp nil]
      (if (>= i SZ)
        [(some? sp) dist sp]
        (let [v' (nth vertices i)
              proj (v2-dot (v2-sub v' r1Pt) dir)
              t? (and (pos? proj) (> proj dist))]
          ;;find the longest +ve distance with edge
          (recur (+ 1 i) SZ dir (if t? proj dist) (if t? v' sp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findPenetration??
  "Find the shortest axis that's overlapping" [r1 r2]
  (let [{:keys [vertices normals]} @r1]
    ;;all vertices have corresponding support points?
    (loop [i 0 SZ (n# normals)
           depth *pos-inf*
           vert nil n' nil support? true]
      (if-not (and support? (< i SZ))
        (if support?
          (chgci! (ci-info) depth n'
                  (v2-add vert (v2-scale n' depth))))
        (let [v' (nth vertices i)
              dir (nth normals i)
              [ok? dist pt]
              (spoint?? v' dir r2)
              t? (and ok? (< dist depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? dist depth) (if t? pt vert) (if t? dir n') ok?))))))

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
  (let [{center :pos} @cc1
        {:keys [vertices normals]} @r1]
    (loop [i 0 SZ (n# normals)
           depth *neg-inf* nEdge 0 inside? true]
      (if (or (not inside?) (>= i SZ))
        [inside? depth nEdge]
        (let [proj (-> (->> (nth vertices i)
                            (v2-sub center))
                       (v2-dot (nth normals i)))
              t? (or (pos? proj) (> proj depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? proj depth)
                 (if t? i nEdge)
                 ;;center is outside of rectangle?
                 (if (pos? proj) false inside?)))))))

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
        (if-not (> dis radius)
          (do->true (chgci! ci (- radius dis) n (v2-add center rVec)))))
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
            (if-not (> dis radius)
              (do->true (chgci! ci (- radius dis) n (v2-add center rVec)))))
          ;;the circle is in face region of face[nEdge]
          (< depth radius)
          (let [rVec (v2-scale en radius)]
            (do->true (chgci! ci (- radius depth) en (v2-sub center rVec))))
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
        vs' [(v2-rot v0 center angle')
             (v2-rot v1 center angle')
             (v2-rot v2 center angle')
             (v2-rot v3 center angle')]]
    (assoc!! s
             :angle (+ angle angle')
             :vertices vs'
             :normals (createFaceNormals vs')) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectMove "" [s p]
  (let [{center :pos :keys [vertices]} @s
        [v0 v1 v2 v3] vertices
        vs' [(v2-add v0 p)
             (v2-add v1 p)
             (v2-add v2 p)
             (v2-add v3 p)]]
    (assoc!! s :vertices vs' :pos (v2-add center p)) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectUpdateInertia "" [s]
  (let [{:keys [width height mass invMass]} @s]
    (assoc!! s
             :inertia (if (zero? mass)
                        0
                        (invert (/ (* mass
                                      (pythagSQ width height)) MI_SPREAD)))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz & [mass friction bounce]]
  (let [s (-> (pc/Rectangle pt sz)
              (rigidBody! mass friction bounce))
        {:keys [vertices width height]} @s]
    (assoc!! s
             :updateInertia rectUpdateInertia
             :collisionTest rectCollisionTest
             :move rectMove
             :rotate rectRotate
             :bxRadius (/ (pythag width height) 2)
             :normals (createFaceNormals vertices))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;circle-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleMove "" [c p]
  (let [{center :pos :keys [startPt]} @c]
    (assoc!! c
             :pos (v2-add center p)
             :startPt (v2-add startPt p)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleRotate "rotate angle in counterclockwise" [c angle']
  (let [{center :pos :keys [angle startPt]} @c]
    (assoc!! c
             :angle (+ angle angle')
             :startPt (v2-rot startPt center angle')) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleUpdateInertia "" [c]
  (let [{:keys [mass radius]} @c
        n (if (zero? mass) 0 (* mass radius radius))]
    (assoc!! c :inertia (/ n MI_SPREAD)) c))

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
        (let [rC2 (-> (v2-neg v1to2)
                      (v2-norm) (v2-scale r2))]
          (chgci! ci (- rSum dist) (v2-norm v1to2) (v2-add c2 rC2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [s1 s2 ci]
  (if (= (:type @s2) :circle)
    (collidedCircCirc s1 s2 ci) (collidedRectCirc s2 s1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius & [mass friction bounce]]
  (let [s (-> (pc/Circle pt radius)
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
    (let [{:keys [gvel vel]} @s1]
      (assoc!! s1
               :gvel (- gvel (* r1xN jN e1))
               :vel (v2-sub vel (v2-scale impulse m1))))
    (let [{:keys [gvel vel]} @s2]
      (assoc!! s2
               :gvel (+ gvel (* r2xN jN e2))
               :vel (v2-add vel (v2-scale impulse m2))))
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
      (let [{:keys [gvel vel]} @s1]
        (assoc!! s1
                 :gvel (- gvel (* r1xT jT e1))
                 :vel (v2-sub vel (v2-scale impulse m1))))
      (let [{:keys [gvel vel]} @s2]
        (assoc!! s2
                 :gvel (+ gvel (* r2xT jT e2))
                 :vel (v2-add vel (v2-scale impulse m2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [posCorrection s1 s2 ci]
  (when-not (and (static? s1) (static? s2))
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
      (loop [j (+ 1 i)]
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
        p1 (_1 more)
        v (cond (= :gvel attr) (+ gvel p1)
                (= :vel attr) (v2-add vel p1)
                (= :bounce attr) p1
                (= :sticky attr) p1)]
    (if (some? v)
      (swap! s #(assoc % attr v))) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runAlgo "" [algoIterCount posCorrection]
  (dotimes [_ algoIterCount] (checkCollision* posCorrection))
  (let [{:keys [samples frameSecs]} @*gWorld*]
    (ec/eachStore samples (fn [s _] (updateShape! s frameSecs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (pc/initPhysics gravity
                  fps
                  world
                  (merge options {:algoRunner runAlgo})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


