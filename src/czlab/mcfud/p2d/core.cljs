;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.core

  (:require [czlab.mcfud.afx.core
             :as c :refer [sqr* n# POS-INF NEG-INF
                           num?? _1 _2 _E cc+ cc+1 do-with]]
            [czlab.mcfud.afx.gfx2d
             :as g :refer [_cocos2dx?]]
            [czlab.mcfud.afx.math :as m :refer [PI vec2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rect
  "Defines a rectangle, origin is left+bottom, and area."
  [x y width height]
  {:x x :y y :width width :height height})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-equals-rect?
  "If rects are equal?"
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (and (== x1 x2) (== y1 y2) (== w1 w2) (== t1 t2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-contains-rect?
  "If R contains r?"
  [{x1 :x y1 :y w1 :width t1 :height :as R}
   {x2 :x y2 :y w2 :width t2 :height :as r}]
  (not (or (>= x1 x2)
           (>= y1 y2)
           (<= (+ x1 w1) (+ x2 w2))
           (<= (+ y1 t1) (+ y2 t2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-maxX
  "Right side of rect."
  [{:keys [x width]}] (+ x width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-midX
  "Mid point of rect on the x-axis."
  [{:keys [x width]}] (+ x (* .5 width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-minX "Get left side of rect." [r] (:x r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-maxY
  "Top of the rect."
  [{:keys [y height]}] (+ y height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-midY
  "Mid point of rect on the y-axis."
  [{:keys [y height]}] (+ y (* .5 height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-get-minY "Bottom of rect." [r] (:y r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pt-in-rect?
  "If point lies inside rect."
  [[px py :as P] rect]
  (and (>= px (rect-get-minX rect))
       (<= px (rect-get-maxX rect))
       (>= py (rect-get-minY rect))
       (<= py (rect-get-maxY rect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-intersects-rect?
  "If two rects intersect?"
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (not (or (< (+ x1 w1) x2)
           (< (+ x2 w2) x1)
           (< (+ y1 t1) y2)
           (< (+ y2 t2) y1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-unions-rect
  "Find the union of two rects."
  [{x1 :x y1 :y w1 :width t1 :height :as R1}
   {x2 :x y2 :y w2 :width t2 :height :as R2}]
  (let [x (min x1 x2)
        y (min y1 y2)]
    (Rect x y
          (- (max (+ x1 w1) (+ x2 w2)) x)
          (- (max (+ y1 t1) (+ y2 t2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect-intersects-rect
  "Find the intersection of two rects."
  [{x1 :x y1 :y w1 :width t1 :height :as rect1}
   {x2 :x y2 :y w2 :width t2 :height :as rect2}]
  (let [x (max x1 x2)
        y (max y1 y2)]
    (Rect x y
          (- (min (rect-get-maxX rect1) (rect-get-maxX rect2)) x)
          (- (min (rect-get-maxY rect1) (rect-get-maxY rect2)) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn out-of-bound?
  "If entity is outside of B?"
  [r B] (not (rect-contains-rect? B r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect?
  "If object is a rect?"
  [obj]
  (and (map? obj)
       (contains? obj :width)
       (contains? obj :height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rect->box4
  "Get the 4 sides of a rect."
  [{:keys [x y width height] :as R}]
  {:top (+ y height) :right (+ x width) :bottom y :left x})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities."
  [a b]
  (rect-intersects-rect? a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private body-num (atom 0))
(defn- next-body-num [] (swap! body-num inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def GWorld (atom {:context nil :canvas nil
                   :samples (c/new-memset)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- dref [s k] (get (cond (c/atom? s) @s
                             (map? s) s :else nil) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pos!
  "Set new position."
  [s & more]
  (apply (dref s :re-pos) (cc+1 s more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn angle!
  "Set new angle."
  [s & more]
  (apply (dref s :set-angle) (cc+1 s more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw
  "Draw object."
  [s & more]
  (apply (dref s :draw) (cc+1 s more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move!
  "Move object."
  [s p] ((dref s :move) s p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate!
  "Rotate object."
  [s v] ((dref s :rotate) s v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn config??
  ""
  [s]
  (let [{:keys [type]} s
        m (get GWorld type)] (merge s m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body
  "A 2d physics body."
  [shape & [options]]
  (do-with [B (atom (merge {:draw (:bodyDrawer GWorld)
                            :oid (next-body-num)
                            :valid? true
                            :type :body
                            :accel (m/vec-zero 2)
                            :vel (m/vec-zero 2)
                            :bxRadius 0
                            :ii 0 :im 0
                            :i 0 :m 0
                            :gvel 0
                            :torque 0
                            :angle 0
                            :statF 0.5 ; 0.8
                            :dynaF 0.3
                            :bounce 0.2} (or options {})))]
    (c/assoc!! B
               :shape (assoc shape :body B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addBody
  "Add a new body to the world."
  [B & [at]]
  (let [pt (or at (m/vec-zero 2))
        {:keys [shape angle]} @B
        {:keys [samples]} @GWorld]
    (pos! B pt angle)
    (c/add->set! samples B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-min-max
  "Find limits (top,bottom) of shape."
  [S]
  (let [{:keys [vertices]} S]
    (loop [i 0 SZ (n# vertices)
           xmin POS-INF ymin POS-INF
           xmax NEG-INF ymax NEG-INF]
      (if (>= i SZ)
        [(vec2 xmin ymin) (vec2 xmax ymax)]
        (let [[x y] (nth vertices i)]
          (recur (+ 1 i)
                 SZ
                 (min xmin x) (min ymin y)
                 (max xmax x) (max ymax y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- find-right-most??
  "Loop through all vertices find the rightmost vertex."
  [vertices]
  (loop [i 1 SZ (n# vertices)
         right 0 cx (_1 (_1 vertices))]
    (if (>= i SZ)
      right
      (let [x (_1 (nth vertices i))
            [r x'] (if (> x cx)
                     [i x]
                     (if (and (= x cx)
                              (< (_2 (nth vertices i))
                                 (_2 (nth vertices right))))
                       [i cx] [right cx]))] (recur (+ 1 i) SZ r x')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort??
  "Sort out vertices, right-most, then counter-clockwise."
  [v1 v2 v3 & more]
  (let [vertices (cc+ [v1 v2 v3] more)
        rightMost (find-right-most?? vertices)]
    (loop [hull [rightMost]
           curIndex rightMost brk? false]
      (if brk?
        (loop [i 0 SZ (n# hull) out []]
          (if (>= i SZ)
            out
            (recur (+ 1 i)
                   SZ
                   (conj out (nth vertices (nth hull i))))))
        (let [nextIndex
              (loop [i 1 SZ (n# vertices) pos 0]
                (if (>= i SZ)
                  pos
                  (recur (+ 1 i)
                         SZ
                         (if (= pos curIndex)
                           i
                           (let [v' (nth vertices (_E hull))
                                 e1 (m/vec-sub (nth vertices pos) v')
                                 e2 (m/vec-sub (nth vertices i) v')
                                 c (m/vec-xss e1 e2)]
                             (if (or (neg? c)
                                     (and (zero? c)
                                          (> (m/vec-lensq e2)
                                             (m/vec-lensq e1)))) i pos))))))
              q? (= nextIndex rightMost)]
          (recur (if q? hull (conj hull nextIndex)) nextIndex q?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-circle-mass [C density]
  (let [{{:keys [radius]} :shape} @C
        density (num?? density 1)
        r2 (c/sqr* radius)
        m (* PI r2 density)] [m  (* m r2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-polyon-mass [P density]
  (let [{{:keys [vertices fnv] :as S} :shape} @P
        gv (if (fn? fnv) fnv identity)
        inv3 (/ 1 3)
        density (num?? density 1)]
    ;;calculate centroid and moment of interia
    (loop [i 0 SZ (n# vertices)
           c (m/vec-zero 2) area 0 I 0]
      (if (>= i SZ)
        ;[(vec-scale c (invert area)) (* density area) (* density I)]
        [(* density area) (* density I)]
        (let [[x2 y2 :as p2] (gv (nth vertices (m/wrap?? i SZ)))
              [x1 y1 :as p1] (gv (nth vertices i))
              D (m/vec-xss p1 p2)
              ;;triangle, 3rd vertex is origin
              triArea (* .5 D)
              x' (+ (sqr* x1) (* x2 x1) (sqr* x2))
              y' (+ (sqr* y1) (* y2 y1) (sqr* y2))]
          ;;use area to weight the centroid average, not just vertex position
          (recur (+ 1 i)
                 SZ
                 (m/vec-add c (m/vec-scale (m/vec-add p1 p2)
                                           (* triArea inv3)))
                 (+ area triArea)
                 (+ I (* .25 inv3 D (+ x' y')))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-mass-density!
  "Set mass via density."
  [B density]
  (let [{{:keys [type]} :shape} @B
        [M I] (if (= type :circle)
                (calc-circle-mass B density)
                (calc-polyon-mass B density))]
    (c/assoc!! B :m M :im (c/num-flip M) :i I :ii (c/num-flip I)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-mass! [B mass]
  (let [{{:keys [type]} :shape} @B
        d (if (c/pos?? mass) 1 0)
        [_ I] (if (= type :circle)
                (calc-circle-mass B d)
                (calc-polyon-mass B d))]
    (c/assoc!! B
               :i I :ii (c/num-flip I)
               :m mass :im (c/num-flip mass)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-body-attrs! [B options]
  (let [{:keys [mass density friction bounce]} options]
    (cond
      (number? mass) (set-mass! B mass)
      (number? density) (set-mass-density! B density)
      :else (set-mass! B 1))
    (if (number? bounce) (c/assoc!! B :bounce bounce))
    (if (number? friction) (c/assoc!! B :statF friction))
    (let [{:keys [m]} @B
          {:keys [gravity]} @GWorld]
      (c/assoc!! B :accel (if (zero? m) (m/vec-zero 2) gravity))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-static!
  "Set this body as a static body."
  [B]
  (c/assoc!! B
             :im 0 :m 0
             :i 0 :ii 0
             :vel (m/vec-zero 2) :accel (m/vec-zero 2) :gvel 0 :torque 0) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn static? [obj]
  (let [{:keys [m im]} @obj] (or (zero? m) (zero? im))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dynamic? [obj]
  (let [{:keys [m im]} @obj] (or (pos? m)(pos? im))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private prev-millis (system-time))
(def ^:private lag-millis 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" [dt & [algoIterCount posCorrection]]
  (set! prev-millis (system-time))
  (set! lag-millis (+ lag-millis dt))
  ;;Make sure we update the game the appropriate number of times.
  ;;Update only every Milliseconds per frame.
  ;;If lag larger then update frames, update until caught up.
  (let [{:keys [algo-runner frame-millis]} @GWorld
        icnt (num?? algoIterCount 10)
        cpos (num?? posCorrection .8)]
    (while (and (>= lag-millis frame-millis)
                (fn? algo-runner))
      (set! lag-millis
            (- lag-millis frame-millis)) (algo-runner icnt cpos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step*
  ""
  [& [algoIterCount posCorrection]]
  (step (- (system-time) prev-millis) algoIterCount posCorrection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init-physics
  "Configure the physics world."
  [gravity fps world & [options]]
  (let [{:keys [width height]} world]
    (if (:cc2dx? options)
      (set! _cocos2dx? true))
    (swap! GWorld
           #(merge %
                   (dissoc options :cc2dx?)
                   {:arena world
                    :FPS fps
                    :width width
                    :height height
                    :gravity (vec2 0 gravity)
                    :frame-secs (c/num-flip fps)
                    :frame-millis (* 1000 (c/num-flip fps))}))) GWorld)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


