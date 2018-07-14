;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.verlet_demo

  (:require-macros [czlab.elmo.afx.core :as ec :refer [assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.p2d.physics2d :as py :refer [rotate!]]
            [czlab.elmo.p2d.verlet2d :as vt :refer [Polygon]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [pythag pythagSQ TWO-PI PI vec2 V2_ZERO _cocos2dx?
                            v2-len v2-add v2-sub v2-dot Point2D
                            v2-neg v2-scale v2-norm v2-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- userControl "" [evt])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawGame "" []
  (let [{:keys [cur samples width height context]} @gWorld]
    (ocall! context "clearRect" 0 0 width height)
    (oset! context "!fillStyle" "black")
    (ocall! context "fillRect" 0 0 width height)
    (ec/eachStore samples
                  (fn [s i]
                    (when (:valid? @s)
                      (oset! context
                             "!strokeStyle" (if (= i cur) "red" "green"))
                      (py/draw s context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runGameLoop "" []
  (js/requestAnimationFrame #(runGameLoop))
  (drawGame)
  (py/step*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- myGame "" []
  (set! gWorld (vt/initPhysics 20 60 {:left 0 :right 799 :top 0 :bottom 449}))
  (assoc!! gWorld :cur 0)
  (let [canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @gWorld
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! gWorld #(assoc % :canvas canvas :context context))
        ;r1 (-> (Polygon [(Point2D 300 210)(Point2D 700 210) (Point2D 700 190)(Point2D 300 190)] 0 0.3 0) (py/setFixed! ))
        r1 (-> (Polygon [(Point2D 100 210)(Point2D 700 210)
                         (Point2D 700 190)(Point2D 100 190)] 100 0.3 0)
               (py/setStatic! ))
        r2 (Polygon [(Point2D 0 410)(Point2D 400 410) (Point2D 400 390)(Point2D 0 390)] 20 1 0.5)
        r3 (Polygon [(Point2D 0 210)(Point2D 200 210) (Point2D 200 190)(Point2D 0 190)] 10)
        r4 (Polygon [(Point2D 0 410) (Point2D 20 410)
                     (Point2D 20 310)(Point2D 0 310)] 10 0 1)]
    (rotate! r1 2.8)
    (dotimes [i 4])
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(set! js/userControl userControl)
(set! js/VerletGame myGame)


