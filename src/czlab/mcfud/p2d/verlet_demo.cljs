;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.verlet_demo

  (:require [czlab.mcfud.afx.math :as m :refer [V2]]
            [czlab.mcfud.p2d.core :as pc]
            [czlab.mcfud.p2d.verlet2d :as v]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.core :as c :refer [fn_0 let->nil]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mouse-control "" [evt])
(defn- user-control "" [evt])

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
  (set! gWorld (v/init-physics 50 60 (g/rect 0 0 799 479)))
  (c/assoc!! gWorld :cur 0)
  (let->nil
    [canvas (js/document.getElementById "canvas")
     ctx (c/call-js! canvas "getContext" "2d")
     {:keys [width height]} @gWorld
     r1 (-> (v/polygon [(V2 200 290)(V2 700 290)
                        (V2 700 310)(V2 200 310)]
                       {:mass 100 :friction 0.3})
            (pc/add-body)
            (pc/set-static!))
     r3 (-> (v/polygon [(V2 200 190) (V2 300 190)
                        (V2 300 210)(V2 200 210)]
                       {:mass 10})
            (pc/add-body))]
    (swap! gWorld
           #(assoc %
                   :context ctx
                   :canvas
                   (c/set-js!! canvas
                               "width" width
                               "height" height)))

    (pc/rotate! r1 2.8)
    (pc/rotate! r3 -2.8)
    (run-game-loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/mouseControl mouse-control)
(set! js/userControl user-control)
(set! js/PhysicsGame my-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


