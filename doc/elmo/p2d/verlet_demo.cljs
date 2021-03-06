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

  (:require [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]
            [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.p2d.core :as pc :refer [addBody]]
            [czlab.elmo.p2d.verlet2d :as vt :refer [Polygon]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [Point2D _cocos2dx?]]
            [czlab.elmo.afx.math
             :as ma :refer [pythag pythagSQ TWO-PI PI vec2
                            vec-len vec-add vec-sub vec-dot
                            vec-zero vec-neg vec-scale vec-unit vec-dist]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- userControl "" [evt])
(defn- mouseControl "" [evt])

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
  (set! gWorld (vt/initPhysics 50 60 {:left 0 :right 799 :top 0 :bottom 449}))
  (assoc!! gWorld :cur 0)
  (let [canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @gWorld
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! gWorld #(assoc % :canvas canvas :context context))
        r1 (-> (Polygon [(Point2D 200 310)(Point2D 700 310)
                         (Point2D 700 290)(Point2D 200 290)] 100 0.3 0)
               (addBody )
               (pc/setStatic! ))
        r3 (-> (Polygon [(Point2D 200 210)
                         (Point2D 300 210)
                         (Point2D 300 190)(Point2D 200 190)] 10)
               (addBody))]
    (pc/rotate! r1 2.8)
    (pc/rotate! r3 -2.8)
    (dotimes [i 4])
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(set! js/mouseControl mouseControl)
(set! js/userControl userControl)
(set! js/VerletGame myGame)


