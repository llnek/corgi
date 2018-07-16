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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 do->true assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [n# num?? invert]]
            [czlab.elmo.afx.gfx2d :as gx :refer [V2_ZERO vec2 _cocos2dx?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private *shapeNum* (atom 0))
(defn- nextShapeNum "" [] (swap! *shapeNum* inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *gWorld* (atom {:context nil :canvas nil
                     :samples (ec/createStore 10)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle "" [pt sz]
  (let [s (gx/Rectangle pt sz)]
    (assoc!! s
             :updateInertia ec/noopy) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [pt radius]
  (let [s (gx/Circle pt radius)]
    (assoc!! s
             :updateInertia ec/noopy) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon
  "" [&[pt vertices]]
  (let [p (gx/Polygon pt vertices)]
    (assoc!! p
             :updateInertia ec/noopy) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateInertia! "" [s] ((:updateInertia @s) s) s)
(defn draw "" [s & more] (apply gx/drawShape (concat [s] more)))
(defn move! "" [s p] ((:move @s) s p) s)
(defn rotate! "" [s v] ((:rotate @s) s v) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic! "" [obj]
  (assoc!! obj
           :invMass 0
           :mass 0
           :vel V2_ZERO
           :accel V2_ZERO
           :gvel 0
           :gaccel 0) (updateInertia! obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateMass! "" [s delta]
  (let [m (+ (:mass @s) delta)
        {:keys [gravity]} @*gWorld*]
    (if (pos? m)
      (do (assoc!! s
                   :mass m
                   :accel gravity
                   :invMass (invert m)) (updateInertia! s))
      (setStatic! s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn static? "" [obj]
  (let [{:keys [mass invMass]} @obj] (or (zero? mass) (zero? invMass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dynamic? "" [obj]
  (let [{:keys [mass invMass]} @obj] (or (pos? mass)(pos? invMass))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rigidBody! "" [obj & [mass friction bounce]]
  (let [{:keys [gravity samples] :as www} @*gWorld*
        mass' (num?? mass 1)
        opts (get www (:type @obj))]
    (assoc!! obj
             :invMass (invert mass')
             :oid (nextShapeNum)
             :vel V2_ZERO
             :valid? true
             :mass mass'
             :inertia 0
             :angle 0
             :gvel 0 ;; clockwise = negative
             :gaccel 0
             :bxRadius 0
             :sticky (num?? friction 0.8)
             :bounce (num?? bounce 0.2)
             :accel (if (zero? mass') V2_ZERO gravity))
    (if (map? opts)
      (swap! obj #(merge % opts)))
    (ec/addToStore! samples obj) obj))

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


