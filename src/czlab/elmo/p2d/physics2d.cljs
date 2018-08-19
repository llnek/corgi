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

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [_1 _2 do->true assoc!!]])

  (:require [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]
            [czlab.elmo.afx.core
             :as ec :refer [*pos-inf* *neg-inf* n# num?? invert]]
            [czlab.elmo.p2d.core
             :as pc :refer [*gWorld* Body
                            static? draw move! rotate!]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [Point2D _cocos2dx?]]
            [czlab.elmo.afx.math
             :as ma :refer [pythag pythagSQ TWO-PI PI wrap??
                            vec2 vec-zero
                            vec-normal vec-len vec-add vec-sub vec-dot
                            vec-neg vec-scale vec-xss vec-rot vec-unit vec-dist]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collisionTest?? "" [s1 s2 ci] ((:collisionTest @s1) s1 s2 ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ci-info "" [& [d n s e]]
  (atom {:depth (num?? d 0)
         :normal (or n (vec-zero 2))
         :start (or s (vec-zero 2)) :end (or e (vec-zero 2))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- chgci! "" [ci d n s]
  (assoc!! ci
           :depth d :normal n
           :start s :end (vec-add s (vec-scale n d))) ci)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- revCIDir! "" [ci]
  (let [{:keys [start end normal]} @ci]
    (assoc!! ci
             :start end :end start :normal (vec-neg normal)) ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- validateBody "" [B]
  (let [{[x y] :pos} @B
        {{:keys [top right bottom left]} :arena} @*gWorld*]
    (if (or (< x left)
            (> x right)
            (if _cocos2dx?
              (or (> y top) (< y bottom))
              (or (< y top) (> y bottom)))) (assoc!! B :valid? false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateBody! "" [B dt]
  (let [{:keys [height width samples validator]} @*gWorld*
        {:keys [oid vel accel gvel torque]} @B
        gv' (+ gvel (* torque dt))
        v' (vec-add vel (vec-scale accel dt))]
    (when true
      ;;update vel += a*t
      ;;move object += v*dt
      ;;update angular vel
      ;rotate object
      (assoc!! B :gvel gv')
      (assoc!! B :vel v')
      (move! B (vec-scale v' dt))
      (rotate! B (* gv' dt)))
    ;;;;;;
    (if (fn? validator) (validator B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overlap? "" [B1 B2]
  (let [{p1 :pos r1 :bxRadius} @B1
        {p2 :pos r2 :bxRadius} @B2
        v1to2 (vec-sub p2 p1)] (not (> (vec-len v1to2) (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rect-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- spoint?? "" [r1Pt n B2]
  (let [{{:keys [vertices]} :shape} @B2]
    ;easier to deal with +ve values
    (loop [i 0 SZ (n# vertices)
           dir (vec-neg n) dist *neg-inf* sp nil]
      (if (>= i SZ)
        [(some? sp) dist sp]
        (let [v' (nth vertices i)
              proj (vec-dot (vec-sub v' r1Pt) dir)
              t? (and (pos? proj) (> proj dist))]
          ;;find the longest +ve distance with edge
          (recur (+ 1 i) SZ dir (if t? proj dist) (if t? v' sp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findPenetration??
  "Find the shortest axis that's overlapping" [B1 B2]
  (let [{{:keys [normals vertices]} :shape} @B1]
    ;;all vertices have corresponding support points?
    (loop [i 0 SZ (n# normals)
           depth *pos-inf*
           vert nil n' nil support? true]
      (if-not (and support? (< i SZ))
        (if support?
          (chgci! (ci-info) depth n'
                  (vec-add vert (vec-scale n' depth))))
        (let [v' (nth vertices i)
              dir (nth normals i)
              [ok? dist pt]
              (spoint?? v' dir B2)
              t? (and ok? (< dist depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? dist depth) (if t? pt vert) (if t? dir n') ok?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- testRectRect?
  "Check for collision between 2 rectangles"
  [B1 B2 ci]
  ;;find Axis of Separation for both rectangle
  (let [ci_1 (findPenetration?? B1 B2)
        ci_2 (if (some? ci_1) (findPenetration?? B2 B1))]
    ;;if both are overlapping, choose the shorter normal
    (when (some? ci_2)
      (let [{d1 :depth n1 :normal s1 :start} @ci_1
            {d2 :depth n2 :normal s2 :start} @ci_2]
        (if (< d1 d2)
          (chgci! ci d1 n1 (vec-sub s1 (vec-scale n1 d1)))
          (chgci! ci d2 (vec-neg n2) s2))))
    (and (some? ci_1)(some? ci_2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleInsideRect? "" [B1 C1 ci nEdge depth]
  (let [{:keys [pos] {:keys [radius]} :shape} @C1
        {{:keys [normals]} :shape} @B1
        n (nth normals nEdge)
        rVec (vec-scale n radius)]
    (chgci! ci (- radius depth) n (vec-sub pos rVec)) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nfaceToCircle?? "" [B1 C1]
  (let [{center :pos} @C1
        {{:keys [normals vertices]} :shape} @B1]
    (loop [i 0 SZ (n# normals)
           depth *neg-inf* nEdge 0 inside? true]
      (if (or (not inside?) (>= i SZ))
        [inside? depth nEdge]
        (let [proj (-> (->> (nth vertices i)
                            (vec-sub center))
                       (vec-dot (nth normals i)))
              t? (or (pos? proj) (> proj depth))]
          (recur (+ 1 i)
                 SZ
                 (if t? proj depth)
                 (if t? i nEdge)
                 ;;center is outside of rectangle?
                 (if (pos? proj) false inside?)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleOutsideRect? "" [B1 C1 ci nEdge depth]
  (let [{center :pos {:keys [radius]} :shape} @C1
        {{:keys [normals vertices]} :shape} @B1
        vn (nth vertices nEdge)
        en (nth normals nEdge)
        ;;V1 is from left vertex of face to center of circle
        ;;V2 is from left vertex of face to right vertex of face
        len (n# normals)
        eX (wrap?? nEdge len)
        vX (nth vertices eX)
        V1 (vec-sub center vn)
        V2 (vec-sub vX vn)
        dot (vec-dot V1 V2)]
    ;;the circle is in corner region of vertex[nEdge]
    (if (neg? dot)
      (let [dis (vec-len V1)
            n (vec-unit V1)
            rVec (vec-scale n (- radius))]
        (if-not (> dis radius)
          (do->true (chgci! ci (- radius dis) n (vec-add center rVec)))))
      ;;;else
      ;the center of circle is in corner region of vertex[nEdge+1]
      ;v1 is from right vertex of face to center of circle
      ;v2 is from right vertex of face to left vertex of face
      (let [v1 (vec-sub center vX)
            v2 (vec-neg V2)
            dot (vec-dot v1 v2)]
        (cond
          (neg? dot)
          (let [dis (vec-len v1)
                n (vec-unit v1)
                rVec (vec-scale n (- radius))]
            (if-not (> dis radius)
              (do->true (chgci! ci (- radius dis) n (vec-add center rVec)))))
          ;;the circle is in face region of face[nEdge]
          (< depth radius)
          (let [rVec (vec-scale en radius)]
            (do->true (chgci! ci (- radius depth) en (vec-sub center rVec))))
          :else false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedRectCirc "" [B1 C1 ci]
  (let [[inside? depth nEdge] (nfaceToCircle?? B1 C1)]
    (if inside?
      (circleInsideRect? B1 C1 ci nEdge depth)
      (circleOutsideRect? B1 C1 ci nEdge depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectCollisionTest "" [B1 B2 ci]
  (if (= (:type (:shape @B2)) :circle)
    (collidedRectCirc B1 B2 ci) (testRectRect? B1 B2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rectangle stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- createFaceNormals

  "0--Top;1--Right;2--Bottom;3--Left"
  [vertices]

  (let [[v0 v1 v2 v3] vertices]
    [(vec-unit (vec-sub v1 v2)) (vec-unit (vec-sub v2 v3))
     (vec-unit (vec-sub v3 v0)) (vec-unit (vec-sub v0 v1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectRotate "" [B angle']
  (let [{{:keys [vertices] :as S} :shape
         :keys [angle pos]} @B
        vs' (gx/rotRectVertices vertices pos angle')]
    (assoc!! B
             :angle (+ angle angle')
             :shape
             (assoc S
                    :vertices vs'
                    :normals (createFaceNormals vs'))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectMove "" [B & [x y]]
  (let [{:keys [pos]
         {:keys [vertices] :as S} :shape} @B
        [v0 v1 v2 v3] vertices
        p (gx/toPoint2D x y)
        vs' [(vec-add v0 p)
             (vec-add v1 p)
             (vec-add v2 p)
             (vec-add v3 p)]]
    (assoc!! B
             :pos (vec-add pos p)
             :shape (assoc S :vertices vs')) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectRepos "" [R pt angle]
  (let [{{:keys [width height] :as S} :shape} @R
        vs (gx/calcRectVertices pt width height)]
    (assoc!! R
             :pos pt
             :shape
             (assoc S
                    :vertices vs
                    :normals (createFaceNormals vs))) R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "" [R ctx]
  (let [{:keys [shape]} @R] ((:draw shape) shape ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private RRR
  {:collisionTest rectCollisionTest
   :repos rectRepos
   :move rectMove
   :rotate rectRotate})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [sz & [options]]
  (let [{:keys [width height]} sz
        B (Body (assoc (pc/config?? (gx/Rectangle sz)) :normals [])
                (assoc RRR :bxRadius (/ (pythag width height) 2)))]
    (pc/setBodyAttrs! B options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;circle-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleMove "" [C  & [x y]]
  (let [{:keys [pos]} @C
        p (gx/toVec2 x y)]
    (assoc!! C :pos (vec-add pos p)) C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleRotate "rotate angle in counterclockwise" [C angle']
  (let [{:keys [angle]} @C]
    (assoc!! C :angle (+ angle angle')) C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collidedCircCirc "" [CC1 CC2 ci]
  (let [{c1 :pos {r1 :radius} :shape} @CC1
        {c2 :pos {r2 :radius} :shape} @CC2
        v1to2  (vec-sub c2 c1)
        rSum  (+ r1 r2)
        dist  (vec-len v1to2)]
    (cond
      (> dist rSum) ;;no overlap
      false
      (zero? dist) ;;centers overlap
      (do->true (chgci! ci rSum (vec2 0 -1)
                        (if (> r1 r2)
                          (vec-add c1 (vec2 0 r1)) (vec-add c2 (vec2 0 r2)))))
      :else ;overlap
      (do->true
        (let [rC2 (-> (vec-neg v1to2)
                      (vec-unit) (vec-scale r2))]
          (chgci! ci (- rSum dist) (vec-unit v1to2) (vec-add c2 rC2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleCollisionTest "" [B1 B2 ci]
  (if (= (:type (:shape @B2)) :circle)
    (collidedCircCirc B1 B2 ci) (collidedRectCirc B2 B1 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleRepos "" [C pt] (assoc!! C :pos pt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [C ctx]
  (let [{:keys [shape pos angle]} @C]
    ((:draw shape) shape ctx pos angle true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private CCC
  {:collisionTest circleCollisionTest
   :repos circleRepos
   :rotate  circleRotate
   :move circleMove})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [radius & [options]]
  (-> (Body (pc/config??
              (gx/Circle radius))
            (assoc CCC :bxRadius radius)) (pc/setBodyAttrs! options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- correctPos! "" [posCorrection B1 B2 ci]
  (let [{:keys [depth normal]} @ci
        {m1 :im} @B1
        {m2 :im} @B2
        n (* posCorrection
             (/ depth (+ m1 m2)))
        jiggle (vec-scale normal n)]
    (move! B1 (vec-scale jiggle (- m1)))
    (move! B2 (vec-scale jiggle m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision*
  "compute and apply response impulses for each object"
  [B1 B2 r1 r2 ci rVelocity rVelocityInNormal]
  (let [{e1 :ii b1 :bounce f1 :statF m1 :im} @B1
        {e2 :ii b2 :bounce f2 :statF m2 :im} @B2
        {:keys [normal]} @ci
        bounce' (min b1 b2)
        sticky' (min f1 f2)
        ;;R cross N
        r1xN (vec-xss r1 normal)
        r2xN (vec-xss r2 normal)
        ;;Calc impulse scalar
        ;;the formula of jN can be found in http://www.myphysicslab.com/collision.html
        jN (/ (* (- (+ 1 bounce')) rVelocityInNormal)
              (+ m1 m2 (* r1xN r1xN e1) (* r2xN r2xN e2)))
        ;;impulse is in direction of normal ( from s1 to s2)
        ;;impulse = F dt = m * ?v , ?v = impulse / m
        impulse (vec-scale normal jN)]
    (let [{:keys [gvel vel]} @B1]
      (assoc!! B1
               :gvel (- gvel (* r1xN jN e1))
               :vel (vec-sub vel (vec-scale impulse m1))))
    (let [{:keys [gvel vel]} @B2]
      (assoc!! B2
               :gvel (+ gvel (* r2xN jN e2))
               :vel (vec-add vel (vec-scale impulse m2))))
    ;;rVelocity.dot(tangent) should less than 0
    (let [tangent (->> (vec-dot rVelocity normal)
                       (vec-scale normal)
                       (vec-sub rVelocity)
                       (vec-unit)
                       (vec-neg))
          r1xT (vec-xss r1 tangent)
          r2xT (vec-xss r2 tangent)
          jT' (/ (* (- (+ 1 bounce')) (vec-dot rVelocity tangent) sticky')
                 (+ m1 m2 (* r1xT r1xT e1) (* r2xT r2xT e2)))
          ;;friction should less than force in normal direction
          jT (if (> jT' jN) jN jT')
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (vec-scale tangent jT)]
      (let [{:keys [gvel vel]} @B1]
        (assoc!! B1
                 :gvel (- gvel (* r1xT jT e1))
                 :vel (vec-sub vel (vec-scale impulse m1))))
      (let [{:keys [gvel vel]} @B2]
        (assoc!! B2
                 :gvel (+ gvel (* r2xT jT e2))
                 :vel (vec-add vel (vec-scale impulse m2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [posCorrection B1 B2 ci]
  (when-not (and (static? B1) (static? B2))
    (correctPos! posCorrection B1 B2 ci)
    ;;the direction of collisionInfo is always from s1 to s2
    ;;but the Mass is inversed, so start scale with s2 and end scale with s1
    (let
      [{:keys [normal start end]} @ci
       {m1 :im c1 :pos vs1 :vel av1 :gvel} @B1
       {m2 :im c2 :pos vs2 :vel av2 :gvel} @B2
       start' (vec-scale start (/ m2 (+ m1 m2)))
       end' (vec-scale end (/ m1 (+ m1 m2)))
       p (vec-add start' end')
       r1 (vec-sub p c1)
       r2 (vec-sub p c2)
       ;;newV = V + mAngularVelocity cross R
       v1 (vec-add vs1 (vec2 (- (* av1 (:y r1))) (* av1 (:x r1))))
       v2 (vec-add vs2 (vec2 (- (* av2 (:y r2))) (* av2 (:x r2))))
       rVelocity (vec-sub v2 v1)
       rVelocityInNormal (vec-dot rVelocity normal)]
      ;;if objects moving apart ignore
      (if-not (pos? rVelocityInNormal)
        (resolveCollision* B1 B2 r1 r2 ci rVelocity rVelocityInNormal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkCollision* "" [posCorrection]
  (let [{:keys [samples context]} @*gWorld*
        len (ec/countStore samples)]
    (dotimes [i len]
      (loop [j (+ 1 i)]
        (when-not (>= j len)
          (let [si (ec/nthStore samples i)
                ci (ci-info)
                sj (ec/nthStore samples j)]
            (when (and (:valid? @si)
                       (:valid? @sj)
                       (overlap? si sj)
                       (collisionTest?? si sj ci))
              ;;make sure the normal is always si -> sj
              (if (neg? (vec-dot (:normal @ci)
                                 (vec-sub (:pos @sj)
                                          (:pos @si)))) (revCIDir! ci))
              (resolveCollision posCorrection si sj ci)))
          (recur (+ 1 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn alterBodyAttr! "" [B attr & more]
  (let [{:keys [gvel vel]} @B
        p1 (_1 more)
        v (cond (= :gvel attr) (+ gvel p1)
                (= :vel attr) (vec-add vel p1)
                (= :bounce attr) p1
                (= :statF attr) p1)]
    (if (some? v)
      (swap! B #(assoc % attr v))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runAlgo "" [algoIterCount posCorrection]
  (dotimes [_ algoIterCount] (checkCollision* posCorrection))
  (let [{:keys [samples frameSecs]} @*gWorld*]
    (ec/eachStore samples (fn [b _] (updateBody! b frameSecs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawBody "" [& args]
  (let [B (_1 args)
        {:keys [shape]} @B
        {:keys [type]} shape]
    (cond
      (= :rectangle type) (apply rectDraw args)
      (= :circle type) (apply circleDraw args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn initPhysics "" [gravity fps world & [options]]
  (pc/initPhysics gravity
                  fps
                  world
                  (merge {:bodyDrawer drawBody
                          :algoRunner runAlgo} options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


