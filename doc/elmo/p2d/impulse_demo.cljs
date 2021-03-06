;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.impulse_demo

  (:require-macros [czlab.elmo.afx.core :as ec :refer [assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.p2d.core :as pc :refer [addBody]]
            [czlab.elmo.p2d.impulse :as ie :refer [Circle]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [Point2D Size2D _cocos2dx?]]
            [czlab.elmo.afx.math
             :as ma :refer [pythag pythagSQ TWO-PI PI vec2
                            vec-len vec-add vec-sub vec-dot vec-zero
                            vec-neg vec-scale vec-rot vec-unit vec-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)
(def XXX 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- userControl "" [evt])
(defn- mouseControl "" [evt]
  (let [x (oget evt "clientX")
        btn? (= 0 (mod XXX 2))
        y (oget evt "clientY")]
    (set! XXX (+ 1 XXX))
    (if btn?
      (let [e (ec/randRange 50 100)
            ne (- e)
            vs (transient [])
            _ (dotimes [_ (ec/randRange 3 12)]
                (conj! vs (vec2 (ec/randRange ne e) (ec/randRange ne e))))
            p (ie/Polygon (persistent! vs))]
        (ie/setOrient! p (ec/randRange (- PI) PI))
        (assoc!! p
                 :bounce 0.2
                 :dynaF 0.2 :statF 0.4)
        (addBody p x y))
      (let [c (-> (Circle (+ 4 (rand 20)))
                  (addBody x y))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawGame "" []
  (let [{:keys [cur samples width height context]} @gWorld]
    (ocall! context "clearRect" 0 0 width height)
    (ec/eachStore samples
                  (fn [s i]
                    (when (:valid? @s)
                      (oset! context
                             "!strokeStyle" (if (= i cur) "red" "blue"))
                      (pc/draw s context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runGameLoop "" []
  (js/requestAnimationFrame #(runGameLoop))
  (drawGame)
  (pc/step*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- myGame "" []
  (set! gWorld (ie/initPhysics 100 60
                               {:left 0 :right 799 :top 0 :bottom 639}
                               {:cur 0}))
  (let [canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @gWorld
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! gWorld #(assoc %
                                :canvas canvas :context context))
        c (-> (Circle 20) (addBody 400 40))
        ;r (-> (ie/PolygonBox (Size2D 50 50)) (addBody 220 100) (ie/setOrient! -2.8))
        p (-> (ie/PolygonBox (Size2D 600 40))
              (addBody 400 600)
              (ie/setOrient! 0);2.8)
              (pc/setStatic! ))]
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(set! js/mouseControl mouseControl)
(set! js/userControl userControl)
(set! js/ImpulseGame myGame)



