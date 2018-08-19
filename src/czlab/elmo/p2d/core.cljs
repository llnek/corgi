;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.core

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [_1 _2 do->true assoc!!]])

  (:require [czlab.elmo.afx.core
             :as ec :refer [*pos-inf* *neg-inf* sqr* n# num?? invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [_cocos2dx? toVec2]]
            [czlab.elmo.afx.math
             :as ma :refer [PI vec-zero vec-len vec-lensq
                            wrap??
                            vec-add vec-scale vec-sub vec-xss vec2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *bodyNum* (atom 0))
(defn- nextBodyNum "" [] (swap! *bodyNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gWorld* (atom {:context nil :canvas nil
                     :samples (ec/createStore 10)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- dref "" [s k] (get (if (map? s) s @s) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setPosition! "" [s & more] (apply (dref s :repos) (concat [s] more)))
(defn setAngle! "" [s & more] (apply (dref s :setAngle) (concat [s] more)))
(defn draw "" [s & more] (apply (dref s :draw) (concat [s] more)))
(defn move! "" [s p] ((dref s :move) s p))
(defn rotate! "" [s v] ((dref s :rotate) s v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn config?? "" [s]
  (let [{:keys [type]} s
        m (get @*gWorld* type)] (merge s (or m {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body "" [shape & [options]]
  (let [B (atom (merge {:draw (:bodyDrawer @*gWorld*)
                        :oid (nextBodyNum)
                        :valid? true
                        :type :body
                        :accel (vec-zero 2)
                        :vel (vec-zero 2)
                        :bxRadius 0
                        :ii 0 :im 0
                        :i 0 :m 0
                        :gvel 0
                        :torque 0
                        :angle 0
                        :statF 0.5 ; 0.8
                        :dynaF 0.3
                        :bounce 0.2} (or options {})))]
    (assoc!! B
             :shape (assoc shape :body B)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addBody "" [B & [x y]]
  (let [{:keys [samples]} @*gWorld*
        pt (toVec2 x y)
        {:keys [shape angle]} @B]
    (setPosition! B pt angle)
    (ec/addToStore! samples B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcMinMax "" [S]
  (let [{:keys [vertices]} S]
    (loop [i 0 SZ (n# vertices)
           xmin *pos-inf* ymin *pos-inf*
           xmax *neg-inf* ymax *neg-inf*]
      (if (>= i SZ)
        [(vec2 xmin ymin) (vec2 xmax ymax)]
        (let [[x y] (nth vertices i)]
          (recur (+ 1 i)
                 SZ
                 (min xmin x) (min ymin y)
                 (max xmax x) (max ymax y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- findRightMost?? "" [vertices]
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
(defn sort?? "" [v1 v2 v3 & more]
  (let [vertices (vec (concat [v1 v2 v3] more))
        rightMost (findRightMost?? vertices)]
    ;;sort out vertices right most then counter-clockwise
    (loop [hull [rightMost]
           curIndex rightMost break? false]
      (if break?
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
                           (let [v' (nth vertices (last hull))
                                 e1 (vec-sub (nth vertices pos) v')
                                 e2 (vec-sub (nth vertices i) v')
                                 c (vec-xss e1 e2)]
                             (if (or (neg? c)
                                     (and (zero? c)
                                          (> (vec-lensq e2)
                                             (vec-lensq e1)))) i pos))))))
              q? (= nextIndex rightMost)]
          (recur (if q? hull (conj hull nextIndex)) nextIndex q?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcCircleMass [C density]
  (let [{{:keys [radius]} :shape} @C
        density (num?? density 1)
        r2 (sqr* radius)
        m (* PI r2 density)] [m  (* m r2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- calcPolyonMass "" [P density]
  (let [{{:keys [vertices fnv] :as S} :shape} @P
        gv (if (fn? fnv) fnv identity)
        inv3 (/ 1 3)
        density (num?? density 1)]
    ;;calculate centroid and moment of interia
    (loop [i 0 SZ (n# vertices)
           c (vec-zero 2) area 0 I 0]
      (if (>= i SZ)
        ;[(vec-scale c (invert area)) (* density area) (* density I)]
        [(* density area) (* density I)]
        (let [[x2 y2 :as p2] (gv (nth vertices (wrap?? i SZ)))
              [x1 y1 :as p1] (gv (nth vertices i))
              D (vec-xss p1 p2)
              ;;triangle, 3rd vertex is origin
              triArea (* 0.5 D)
              x' (+ (sqr* x1) (* x2 x1) (sqr* x2))
              y' (+ (sqr* y1) (* y2 y1) (sqr* y2))]
          ;;use area to weight the centroid average, not just vertex position
          (recur (+ 1 i)
                 SZ
                 (vec-add c (vec-scale (vec-add p1 p2)
                                       (* triArea inv3)))
                 (+ area triArea)
                 (+ I (* 0.25 inv3 D (+ x' y')))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setMassViaDensity! "" [B density]
  (let [{{:keys [type]} :shape} @B
        [M I] (if (= type :circle)
                (calcCircleMass B density)
                (calcPolyonMass B density))]
    (assoc!! B :m M :im (invert M) :i I :ii (invert I)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setMass! "" [B mass]
  (let [{{:keys [type]} :shape} @B
        d (if (pos? mass) 1 0)
        [_ I] (if (= type :circle)
                (calcCircleMass B d)
                (calcPolyonMass B d))]
    (assoc!! B
             :i I :ii (invert I)
             :m mass :im (invert mass)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setBodyAttrs! "" [B options]
  (let [{:keys [mass density friction bounce]} options]
    (cond
      (number? mass) (setMass! B mass)
      (number? density) (setMassViaDensity! B density)
      :else (setMass! B 1))
    (if (number? bounce) (assoc!! B :bounce bounce))
    (if (number? friction) (assoc!! B :statF friction))
    (let [{:keys [m]} @B
          {:keys [gravity]} @*gWorld*]
      (assoc!! B :accel (if (zero? m) (vec-zero 2) gravity))) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic! "" [B]
  (assoc!! B
           :im 0 :m 0
           :i 0 :ii 0
           :vel (vec-zero 2) :accel (vec-zero 2) :gvel 0 :torque 0) B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn static? "" [obj]
  (let [{:keys [m im]} @obj] (or (zero? m) (zero? im))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dynamic? "" [obj]
  (let [{:keys [m im]} @obj] (or (pos? m)(pos? im))))

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
        posCorrect (num?? posCorrection 0.8)]
    (while (and (>= lagMillis frameMillis)
                (fn? algoRunner))
      (set! lagMillis
            (- lagMillis frameMillis)) (algoRunner iterCnt posCorrect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step*
  "" [& [algoIterCount posCorrection]]
  (step (- (system-time) prevMillis) algoIterCount posCorrection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initPhysics "" [gravity fps world & [options]]
  (let [{:keys [top right bottom left]} world]
    (if (:cc2dx? options)
      (set! _cocos2dx? true))
    (swap! *gWorld*
           (fn [root]
             (-> (->> (dissoc options :cc2dx?) (merge root))
                 (assoc :arena world
                        :FPS fps
                        :width (+ 1 (- right left))
                        :height (+ 1 (if _cocos2dx? (- top bottom) (- bottom top)))
                        :gravity (vec2 0 gravity)
                        :frameSecs (invert fps)
                        :frameMillis (* 1000 (invert fps)))))) *gWorld*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


