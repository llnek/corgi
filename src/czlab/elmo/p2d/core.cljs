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
            [czlab.elmo.afx.gfx2d
             :as gx :refer [V2_ZERO toVec2 vec2 _cocos2dx?]]))

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
(defn updateMass! "" [s & [v]] ((dref s :updateMass) s v))
(defn updateInertia! "" [s] ((dref s :updateInertia) s))
(defn move! "" [s p] ((dref s :move) s p))
(defn rotate! "" [s v] ((dref s :rotate) s v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rigidBody! "" [B & [mass friction bounce]]
  (let [{:keys [gravity samples] :as www} @*gWorld*
        mass' (num?? mass 1)]
    (assoc!! B
             :m mass'
             :im (invert mass')
             :accel (if (zero? mass') V2_ZERO gravity))
    (updateMass! B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body "" [shape & [mass friction bounce options]]
  (let [B (atom {:oid (nextBodyNum)
                 :valid? true
                 :type :body
                 :accel V2_ZERO
                 :vel V2_ZERO
                 :bxRadius 0
                 :ii 0 :im 0
                 :i 0 :m 0
                 :gvel 0
                 :torque 0
                 :angle 0
                 :statF 0.5 ; 0.8
                 :dynaF 0.3
                 :bounce 0.2})]
    (assoc!! B :shape (assoc shape :body B))
    (swap! B #(merge % (or options {})))
    (rigidBody! B mass friction bounce)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn addBody "" [B & [x y]]
  (let [{:keys [samples]} @*gWorld*
        pt (toVec2 x y)
        {:keys [shape angle]} @B]
    (setPosition! B pt angle)
    (ec/addToStore! samples B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic! "" [obj]
  (assoc!! obj
           :im 0
           :m 0
           :i 0
           :ii 0
           :vel V2_ZERO
           :accel V2_ZERO
           :gvel 0
           :torque 0) (updateInertia! obj))

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


