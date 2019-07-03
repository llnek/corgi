;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.impulse_demo

  (:require [czlab.mcfud.afx.core :as c]
            [czlab.mcfud.p2d.core :as pc]
            [czlab.mcfud.p2d.impulse :as i]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.math :as m :refer [V2 PI]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)
(def XXX 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- user-control "" [evt])
(defn- mouse-control "" [evt]
  (let [x (c/get-js evt "clientX")
        btn? (= 0 (mod XXX 2))
        y (c/get-js evt "clientY")]
    (set! XXX (+ 1 XXX))
    (if btn?
      (let [e (c/rand-range 50 100)
            p (i/polygon
                (loop [i 0 SZ (c/rand-range 3 12) vs (c/tvec*)]
                  (if (>= i SZ)
                    (c/ps! vs)
                    (recur (+ 1 i) SZ
                           (conj! vs (V2 (c/rand-range (- e) e)
                                         (c/rand-range (- e) e)))))))]
        (i/set-orient! p (c/rand-range (- PI) PI))
        (c/assoc!! p
                   :bounce 0.2
                   :dynaF 0.2 :statF 0.4)
        (pc/add-body p (V2 x y)))
      (let [c (-> (i/circle (+ 4 (rand 20)))
                  (pc/add-body (V2 x y)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-game "" []
  (let [{:keys [cur samples width height context]} @gWorld]
    (c/call-js! context "clearRect" 0 0 width height)
    (c/each-set samples
                (fn [b i]
                  (when (:valid? @b)
                    (c/set-js! context
                               "strokeStyle" (if (= i cur) "red" "blue"))
                    (pc/draw b context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-game-loop "" []
  (js/requestAnimationFrame #(run-game-loop))
  (draw-game)
  (pc/step*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- my-game "" []
  (set! gWorld (i/init-physics 100 60
                               (g/rect 0 0 799 639) {:cur 0}))
  (let [canvas (js/document.getElementById "canvas")
        context (c/call-js! canvas "getContext" "2d")
        {:keys [width height]} @gWorld
        _ (c/set-js!! canvas
                      "width" width
                      "height" height)
        _ (swap! gWorld #(assoc %
                                :canvas canvas :context context))
        c (-> (i/circle 20) (pc/add-body (V2 400 40)))
        ;r (-> (ie/PolygonBox (Size2D 50 50)) (addBody 220 100) (ie/setOrient! -2.8))
        p (-> (i/polygon-box (g/rect 0 0 600 40))
              (pc/add-body (V2 400 600))
              (i/set-orient! 0);2.8)
              (pc/set-static!))]
    (run-game-loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/mouseControl mouse-control)
(set! js/userControl user-control)
(set! js/PhysicsGame my-game)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

