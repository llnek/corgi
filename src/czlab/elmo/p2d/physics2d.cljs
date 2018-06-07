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
(defn- inv! "" [x] (if-not (zero? x) (/ 1 x) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [x y] {:x x :y y})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gravity* (vec2 0 20))
(def *v2-zero* (vec2 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2LenSQ
  "" [{:keys [x y] :as v2}] (+ (* x x) (* y y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Len "" [v2] (js/Math.sqrt (vec2LenSQ v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Add
  "" [v1 v2] (vec2 (+ (:x v1) (:x v2)) (+ (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Sub
  "" [v1 v2] (vec2 (- (:x v1) (:x v2)) (- (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Scale
  "" [v n] (vec2 (* n (:x v)) (* n (:y v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Dot
  "" [v1 v2] (+ (* (:x v1) (:x v2)) (* (:y v1) (:y v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Cross
  "" [v1 v2] (- (* (:x v1)(:y v2)) (* (:y v1)(:x v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Rotate
  "Rotate in radian" [v center angle]
  (let [{cx :x cy :y} center
        {:keys [x y]} v
        xx (- x cx)
        yy (- y cy)
        r0 (- (* xx (js/Math.cos angle))
              (* yy (js/Math.sin angle)))
        r1 (+ (* xx (js/Math.sin angle))
              (* yy (js/Math.cos angle)))]
    (vec2 (+ r0 cx) (+ r1 cy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Normal "" [v]
  (let [{:keys [x y]} v
        n (inv! (vec2Len v))] (vec2 (* n x) (* n y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2Dist "" [v1 v2]
  (let [x (- (:x v1)(:x v2))
        y (- (:y v1)(:y v2))]
    (js/Math.sqrt (+ (* x x) (* y y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionInfo "" []
  (atom {:depth 0
         :normal *v2-zero*  :start *v2-zero*  :end *v2-zero* }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionDepth!
  "" [c d] (swap! c #(assoc % :depth d)) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionNormal!
  "" [c n] (swap! c #(assoc % :normal n)) c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setCollisionInfo! "" [c depth normal start]
  (swap! c
         #(assoc %
                 :normal normal
                 :depth depth
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
(defn updateInertia!  "" [s] ((:updateInertia @s) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move!  "" [s p] ((:move @s) s p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate!  "" [s angle] ((:rotate @s) angle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collisionTest?? "" [s other ci] ((:collisionTest s) s other ci))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *positional-correction-flag* true)
(def relaxation-count 15)
(def relaxation-range (range relaxation-count))
;;percentage of separation to project objects
(def pos-correction-rate 0.8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- positionalCorrection "" [s1 s2 ci]
  (let [{m1 :invMass} @s1
        {m2 :invMass} @s2
        {:keys [depth normal]} @ci
        correction (->> (+ m1 m2)
                        (/ depth)
                        (* pos-correction-rate) (vec2Scale normal))]
    (move! s1 (vec2Scale correction (- m1)))
    (move! s2 (vec2Scale correction m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision** "" [s1 s2 r1 r2
                              relativeVelocity rVelocityInNormal ci]
  ;;compute and apply response impulses for each object
  (let [{b1 :bounce f1 :sticky m1 :invMass in1 :inertia} @s1
        {b2 :bounce f2 :sticky m2 :invMass in2 :inertia} @s2
        {:keys [normal]} @ci
        bounce' (js/Math.min b1 b2)
        sticky' (js/Math.min f1 f2)
        r1xn (vec2Cross r1 normal)
        r2xn (vec2Cross r2 normal)
        ;;find impulse, the formula of jN www.myphysicslab.com/collision.html
        jN (/ (* (- (+ 1 bounce')) rVelocityInNormal)
              (+ m1 m2 (* r1xn r1xn in1) (* r2xn r2xn in2)))
        ;;impulse is in direction of normal (from s1 to s2)
        ;;impulse = F dt = m * ?v
        impulse (vec2Scale normal jN)]
    (swap! s1
           (fn [root]
             (assoc root
                    :vel (vec2Sub (:vel root)
                                  (vec2Scale impulse (:invMass root)))
                    :angVel (- (:angVel root) (* r1xn jN (:inertia root))))))
    (swap! s2
           (fn [root]
             (assoc root
                    :vel (vec2Add (:vel root)
                                  (vec2Scale impulse (:invMass root)))
                    :angVel (+ (:angVel root) (* r2xn jN (:inertia root))))))
    (let [tangent' (vec2Sub relativeVelocity
                            (vec2Scale normal
                                       (vec2Dot relativeVelocity normal)))
          tangent (vec2Scale (vec2Normal tangent') -1)
          {in1 :inertia m1 :invMass} @s1
          {in2 :inertia m2 :invMass} @s2
          r1xt (vec2Cross r1 tangent)
          r2xt (vec2Cross r2 tangent)
          jT' (/ (* (- (+ 1 bounce'))
                    (* (vec2Dot relativeVelocity tangent) sticky'))
                 (+ m1 m2 (+ (* r1xt r1xt in1) (* r2xt r2xt in2))))
          ;;friction should less than force in normal direction
          jT (if (> jT' jN) jN jT')
          ;;impulse is from s1 to s2 (in opposite direction of velocity)
          impulse (vec2Scale tangent jT)]
      (swap! s1
             (fn [root]
               (assoc root
                      :vel (vec2Sub (:vel root)
                                    (vec2Scale impulse (:invMass root)))
                      :angVel (- (:angVel root) (* r1xt jT (:inertia root))))))
      (swap! s2
             (fn [root]
               (assoc root
                      :vel (vec2Add (:vel root)
                                    (vec2Scale impulse (:invMass root)))
                      :angVel (+ (:angVel root) (* r2xt jT (:inertia root)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision* "" [s1 s2 ci]
  (if *positional-correction-flag* (positionalCorrection s1 s2 ci))
  (let [{:keys [normal start end]} @ci
        {m1 :invMass c1 :center vv1 :vel av1 :angVel} @s1
        {m2 :invMass c2 :center vv2 :vel av2 :angVel} @s2
        ;;the dir of ci is always s1 -> s2
        ;;but the mass is inversed, so scale start with s2 and scale end with s1
        start' (vec2Scale start (/ m2 (+ m1 m2)))
        end' (vec2Scale end (/ m1 (+ m1 m2)))
        p (vec2Add start' end')
        r1 (vec2Sub p c1)
        r2 (vec2Sub p c2)
        v1 (vec2Add vv1 (vec2 (* -1 av1 (:y r1)) (* av1 (:x r1))))
        v2 (vec2Add vv2 (vec2 (* -1 av2 (:y r2)) (* av2 (:x r2))))
        relativeVelocity (vec2Sub v2 v1)
        rVelocityInNormal (vec2Dot relativeVelocity normal)]
    ;;ignore if objects are moving apart
    (if-not (pos? rVelocityInNormal)
      (resolveCollision** s1 s2 r1 r2 relativeVelocity rVelocityInNormal ci))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolveCollision "" [s1 s2 ci]
  (if-not (and (zero? (:invMass @s1))
               (zero? (:invMass @s2))) (resolveCollision* s1 s2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boundTest "" [s1 s2]
  (let [{c1 :center r1 :boundRadius} @s1
        {c2 :center r2 :boundRadius} @s2
        v1to2 (vec2Sub c2 c1)
        rSum  (+ r1 r2)
        dist (vec2Len v1to2)] (not (> dist rSum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collision?? "" [samples]
  (let [ci (collisionInfo)
        len (count samples)]
    (dotimes [k relaxation-range]
      (dotimes [i (range len)
                :let [si (nth samples i)]]
        (loop [j (+ 1 i)]
          (when (< j len)
            (let [sj (nth samples j)]
              (when (and (boundTest si sj)
                         (collisionTest?? si sj ci))
                ;;make sure the normal is always from object[i] to object[j]
                (if (neg? (vec2Dot (:normal @ci)
                                   (vec2Sub (:center sj) (:center si))))
                  (changeCollisionDir! ci))
                (resolveCollision si sj ci))
              (recur (inc j)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rigidBody "" [center mass friction restitution]
  (let [mass' (if (number? mass) mass 1)]
    (atom {:accel (if (zero? mass') *v2-zero* *gravity*)
           :angAccel 0
           :invMass (inv! mass')
           :inertia 0
           :vel *v2-zero*
           :angVel 0
           :boundRadius 0
           :center center
           :angle 0 ;;negetive-- clockwise
           :sticky (if (number? friction) friction 0.8)
           :bounce (if (number? restitution) restitution 0.2)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass "" [s delta]
  (let [{:keys [invMass]} @s
        mass (+ delta (inv! invMass))]
    (if (pos? mass)
      (swap! s #(assoc % :invMass (/ 1 mass) :accel *gravity*))
      (swap! s #(assoc %
                       :accel *v2-zero*
                       :vel *v2-zero*
                       :invMass 0
                       :angVel 0
                       :angAccel 0)))
    (updateInertia! s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update "" [world s dt]
  (when *movement*
    (swap! s
           (fn [root]
             (assoc root
                    :vel (vec2Add (:vel root) (vec2Scale (:accel root) dt)))))
    (move! s (vec2Scale (:vel @s) dt))
    (swap! s
           (fn [root]
             (assoc root
                    :angVel (+ (:angVel root) (* (:angAccel root) dt)))))
    (rotate! s (* dt (:angVel @s))))
  (let [{:keys [width height samples]} @world
        {:keys [x y]} (:center @s)]
    (when (or (< x 0)
              (> x width)
              (< y 0)
              (> y height))
      (let [i (.indexOf samples s)]
        (if-not (neg? i)
          (.splice samples i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- moveCircle "" [s p]
  (let [{:keys [startPt center]} @s]
    (swap! s
           #(assoc %
                   :startPt (vec2Add startPt p)
                   :center (vec2Add center p))) s))

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
        dist (vec2Len vFrom1to2)]
    (cond
      (> dist (js/Math.sqrt (* rSum rSum))) ;overlapping
      false
      (not (zero? dist)) ;;overlapping but not same position
      (let [normalFrom2to1 (vec2Normal (vec2Scale vFrom1to2 -1))
            radiusC2 (vec2Scale normalFrom2to1 r2)]
        (setCollisionInfo! ci
                           (- rSum dist)
                           (vec2Normal vFrom1to2)
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
  (let [s (rigidBody center mass friction restitution)]
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
    (conj! fs (vec2Normal (vec2Sub (nth vs' 1) (nth vs' 2))))
    (conj! fs (vec2Normal (vec2Sub (nth vs' 2) (nth vs' 3))))
    (conj! fs (vec2Normal (vec2Sub (nth vs' 3)(nth vs' 0))))
    (conj! fs (vec2Normal (vec2Sub (nth vs' 0)(nth vs' 1))))
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
(defn- collidedRectCirc "" [r1 cc2 ci]
  (let [{:keys [vertices normals]} @r1
        {circ2Pos :center circ2Rad :radius} @cc2
        ;;find the nearest face for center of circle
        [bestDistance nearestEdge inside?]
        (loop [bestDistance -99999
               nearestEdge 0
               inside? true break? false i 0]
          (if (or break? (>= i 4))
            [bestDistance nearestEdge inside?]
            (let [v  (vec2Sub circ2Pos (nth vertices i))
                  projection  (vec2Dot v (nth normals i))]
            (if (pos? projection)
              ;;if the center of circle is outside of rectangle
              (recur projection i false true i)
              (if (> projection bestDistance)
                (recur projection i inside? break? (inc i))
                (recur bestDistance nearestEdge inside? break? (inc i)))))))]
    (cond
      ;;the center of circle is outside of rectangle
      ;;v1 is from left vertex of face to center of circle
      ;;v2 is from left vertex of face to right vertex of face
      (not inside?)
      (let [v1  (vec2Sub circ2Pos (nth vertices nearestEdge))
            v2  (vec2Sub (nth vertices (rem (inc nearestEdge) 4)) (nth vertices nearestEdge))
            dot (vec2Dot v1 v2)]
        (if (neg? dot)
          (let [;;the center of circle is in corner region of mVertex[nearestEdge]
                dis (vec2Len v1)]
            ;;compare the distance with radium to decide collision
            (if (> dis circ2Rad)
              false
              (let [normal (vec2Normal v1)
                    radiusVec (vec2Scale normal (- circ2Rad))]
                (setCollisionInfo! ci
                                   (- circ2Rad dis)
                                   normal
                                   (vec2Add circ2Pos radiusVec)) true)))
          (let [;;the center of circle is in corner region of mVertex[nearestEdge+1]
                ;;v1 is from right vertex of face to center of circle
                ;;v2 is from right vertex of face to left vertex of face
                v1  (vec2Sub circ2Pos (nth vertices (rem (inc nearestEdge) 4)))
                v2  (vec2Scale v2 -1)
                dot (vec2Dot v1 v2)]
            (if (neg? dot)
              (let [dis (vec2Len v1)]
                ;;compare the distance with radium to decide collision
                (if (> dis circ2Rad)
                  false
                  (let [normal (vec2Normal v1)
                        radiusVec (vec2Scale normal (- circ2Rad))]
                    (setCollisionInfo! ci
                                       (- circ2Rad dis)
                                       normal
                                       (vec2Add circ2Pos radiusVec)) true)))
              (let []
                ;;the center of circle is in face region of face[nearestEdge]
                (if (< bestDistance circ2Rad)
                  (let [radiusVec (vec2Scale (nth normals nearestEdge) circ2Rad)]
                    (setCollisionInfo! ci
                                       (- circ2Rad bestDistance)
                                       (nth normals nearestEdge)
                                       (vec2Sub circ2Pos radiusVec)) true)
                  false))))))
      :else
      (let [;;the center of circle is inside of rectangle
            radiusVec (vec2Scale (nth normals nearestEdge) circ2Rad)]
        (setCollisionInfo! ci
                           (- circ2Rad bestDistance)
                           (nth normals nearestEdge)
                           (vec2Sub circ2Pos radiusVec)) true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- collideTestRect "" [r1 r2 ci]
  (if (= (:type @r2) :circle)
    (collidedRectCirc r1 r2 ci)
    (collidedRectRect r1 r2 ci)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rectangle "" [center width height mass friction restitution]
  (let [s (rigidBody center mass friction restitution)
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
        nn [(vec2Normal (vec2Sub (nth vs 1)(nth vs 2)))
            (vec2Normal (vec2Sub (nth vs 2)(nth vs 3)))
            (vec2Normal (vec2Sub (nth vs 3)(nth vs 0)))
            (vec2Normal (vec2Sub (nth vs 0)(nth vs 1)))]]
    (swap! s
           #(assoc %
                   :type :rectangle
                   :move moveRect
                   :rotate rotRect
                   :collisionTest collideTestRect
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
