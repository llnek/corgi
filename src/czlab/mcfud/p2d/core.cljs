;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
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
             :as c :refer [n# POS-INF NEG-INF
                           _1 _2 _E cc+ cc+1 do-with]]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.math :as m :refer [PI V2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Defines 4 points of a rectange.
(defrecord Box4 [left bottom right top])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;bottom left
(defn rect->box4
  "Get the 4 sides of a rect."
  [{:keys [x y width height] :as R}]
  (new Box4 x y (+ x width) (+ y height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collide?
  "Test collision of 2 entities."
  [a b]
  (g/rect-intersects-rect? a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private body-num (atom 0))
(defn- next-body-num [] (swap! body-num inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld (atom {:context nil
                   :canvas nil
                   :samples (c/new-memset)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- dref [s k]
  (get (cond (c/atom? s) @s
             (map? s) s :else (c/raise! "Bad shape!")) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pos!
  "Set new position."
  [b & more] (apply (dref b :repos) b more) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn angle!
  "Set new angle."
  [b & more] (apply (dref b :set-angle) b more) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move!
  "Move body." [b p] ((dref b :move) b p) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotate!
  "Rotate body." [b v] ((dref b :rotate) b v) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw "" [b & more] (apply (dref b :draw) b more) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn config??
  "If the world has extra properties for this type,
  merge them into it."
  [s]
  (let [{:keys [type]} s
        m (@gWorld type)] (merge s m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn body
  "A 2d physics body."
  ([shape] (body shape nil))
  ([shape options]
   (let [B (atom {:oid (next-body-num)
                  :valid? true
                  :type :body
                  :accel (m/vz2)
                  :vel (m/vz2)
                  :bx-radius 0
                  :ii 0 ;;inverse inertia
                  :i 0 ;;inertia
                  :im 0 ;;inverse momentum
                  :m 0 ;;momentum
                  :gvel 0
                  :pos (m/vz2)
                  :torque 0
                  :angle 0
                  :statF .5 ; 0.8
                  :dynaF .3
                  :bounce .2})]
     (swap! B
            #(assoc (merge % options)
                    :shape (assoc shape :body B))) B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-body
  "Add a new body to the world."
  ([B] (add-body B nil))
  ([B pt]
   (let [{:keys [shape angle]} @B
         {:keys [samples]} @gWorld]
     (c/add->set! samples B)
     (pos! B (or pt (m/vz2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-min-max*
  "Find limits (high, low) of vertices."
  [vertices]
  (loop [i 0 SZ (n# vertices)
         xmin POS-INF ymin POS-INF
         xmax NEG-INF ymax NEG-INF]
    (if (>= i SZ)
      [(V2 xmin ymin) (V2 xmax ymax)]
      (let [[x y] (nth vertices i)]
        (recur (+ 1 i)
               SZ
               (min xmin x) (min ymin y)
               (max xmax x) (max ymax y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calc-min-max
  "Find limits (top,bottom) of shape."
  [S] (calc-min-max* (:vertices S)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- find-right-most??
  "Loop through all vertices find the
  rightmost vertex, bottom up."
  [vertices]
  (loop [i 1 SZ (n# vertices)
         right 0 cx (_1 (_1 vertices))]
    (if (>= i SZ)
      right
      (let [[x _] (nth vertices i)
            [r x'] (if (> x cx)
                     [i x]
                     (if (and (= x cx)
                              (< (_2 (nth vertices i))
                                 (_2 (nth vertices right))))
                       [i cx] [right cx]))] (recur (+ 1 i) SZ r x')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sort??
  "Sort out vertices, right-most, then counter-clockwise."
  ([v1 v2 v3 & more] (sort?? (cc+ [v1 v2 v3] more)))
  ([vs]
   (let [rMost (find-right-most?? vs)]
     (loop [hull [rMost]
            cur rMost brk? false]
       (if brk?
         (loop [i 0 SZ (n# hull) out (c/tvec*)]
           (if (>= i SZ)
             (c/ps! out)
             (recur (+ 1 i) SZ
                    (conj! out (nth vs (nth hull i))))))
         (let [nxt
               (loop [i 1 SZ (n# vs) pos 0]
                 (if (>= i SZ)
                   pos
                   (recur (+ 1 i) SZ
                          (if (= pos cur)
                            i
                            (let [v' (nth vs (_E hull))
                                  e1 (m/vec-sub (nth vs pos) v')
                                  e2 (m/vec-sub (nth vs i) v')
                                  c (m/vec-xss e1 e2)]
                              (if (or (neg? c)
                                      (and (zero? c)
                                           (> (m/vec-lensq e2)
                                              (m/vec-lensq e1)))) i pos))))))
               q? (= nxt rMost)]
           (recur (if q? hull (conj hull nxt)) nxt q?)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-circle-mass [C density]
  (let [{{:keys [radius]} :shape} @C
        density (c/num?? density 1)
        r2 (c/sqr* radius)
        m (* PI r2 density)] [m  (* m r2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calc-polyon-mass [P density]
  (let [{{:keys [vertices fnv] :as S} :shape} @P
        gv (if (fn? fnv) fnv identity)
        inv3 (/ 1 3)
        density (c/num?? density 1)]
    ;;calculate centroid and moment of interia
    (loop [i 0 SZ (n# vertices)
           c (m/vz2) area 0 I 0]
      (if (>= i SZ)
        ;[(vec-scale c (invert area)) (* density area) (* density I)]
        [(* density area) (* density I)]
        (let [[x2 y2 :as p2] (gv (nth vertices (m/wrap?? i SZ)))
              [x1 y1 :as p1] (gv (nth vertices i))
              D (m/vec-xss p1 p2)
              ;;triangle, 3rd vertex is origin
              triArea (/ D 2)
              x' (+ (c/sqr* x1) (* x2 x1) (c/sqr* x2))
              y' (+ (c/sqr* y1) (* y2 y1) (c/sqr* y2))]
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
    (c/assoc!! B
               :m M :im (c/flip M)
               :i I :ii (c/flip I))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-mass! [B mass]
  (let [{{:keys [type]} :shape} @B
        d (if (c/pos?? mass) 1 0)
        [_ I] (if (= type :circle)
                (calc-circle-mass B d)
                (calc-polyon-mass B d))]
    (c/assoc!! B
               :i I :ii (c/flip I)
               :m mass :im (c/flip mass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-body-attrs! [B options]
  (let [{:keys [mass density
                friction bounce]} options]
    (cond
      (number? mass) (set-mass! B mass)
      (number? density) (set-mass-density! B density)
      :else (set-mass! B 1))
    (if (number? bounce) (c/assoc!! B :bounce bounce))
    (if (number? friction) (c/assoc!! B :statF friction))
    (let [{:keys [m]} @B
          {:keys [gravity]} @gWorld]
      (c/assoc!! B
                 :accel (if (zero? m) (m/vz2) gravity)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-static!
  "Set this body as a static body."
  [B]
  (c/assoc!! B
             :im 0
             :m 0
             :i 0
             :ii 0
             :gvel 0
             :torque 0
             :vel (m/vz2)
             :accel (m/vz2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn static? [obj]
  (let [{:keys [m im]} @obj] (or (zero? m) (zero? im))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dynamic? [obj]
  (let [{:keys [m im]} @obj] (or (pos? m)(pos? im))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn validate-body [obj]
  (let [{:keys [pos]} @obj
        {a :world} @gWorld]
    (if-not (g/contains-pt? a pos)
      (c/assoc!! obj :valid? false) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private prev-millis (system-time))
(def ^:private lag-millis 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step
  ([dt] (step dt nil nil))
  ([dt algoIterCount posCorrection]
   (set! prev-millis (system-time))
   (set! lag-millis (+ lag-millis dt))
   ;;Make sure we update the game the appropriate number of times.
   ;;Update only every Milliseconds per frame.
   ;;If lag larger then update frames, update until caught up.
   (let [{:keys [algo-runner frame-millis]} @gWorld
         icnt (c/num?? algoIterCount 15)
         cpos (c/num?? posCorrection .8)]
     (while (and (fn? algo-runner)
                 (>= lag-millis frame-millis))
       (set! lag-millis
             (- lag-millis
                frame-millis))
       (algo-runner icnt cpos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step*
  ([] (step* nil nil))
  ([algoIterCnt posCorrection]
   (step (- (system-time) prev-millis)
         algoIterCnt posCorrection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init
  "Configure the physics world."
  ([gravity fps world] (init gravity fps world nil))
  ([gravity fps world options]
   (swap! gWorld
          #(merge %
                  {:world world
                   :FPS fps
                   :gravity (V2 0 gravity)
                   :frame-secs (c/flip fps)
                   :frame-millis (* 1000 (c/flip fps))} options)) gWorld))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


