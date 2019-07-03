;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.p2d.physics_demo

  (:require [czlab.mcfud.afx.math :as m :refer [V2]]
            [czlab.mcfud.p2d.core :as pc]
            [czlab.mcfud.p2d.physics2d :as py]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.core :as c :refer [_1 _2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gWorld nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mouse-control "" [evt] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- user-control "" [evt]
  (let [key (or (c/get-js evt "keyCode")
                (c/get-js evt "?which"))
        {:keys [cur samples]} @gWorld
        len (c/count-set samples)
        s (c/nth-set samples cur)
        offset (- key 48)]
    (cond
      (and (>= key 48)
           (<= key 57))
      (if (< (- offset 48) len)
        (swap! gWorld #(assoc % :cur offset)))
      (= key 38) ;up arrow
      (if (pos? cur)
        (swap! gWorld #(assoc % :cur (dec cur))))
      (= key 40) ;down arrow
      (if (< cur (- len 1))
        (swap! gWorld #(assoc % :cur (inc cur))))
      (= key 87) ;;W
      (pc/move! s (V2 0 -10))
      (= key 83) ;;S
      (pc/move! s (V2 0 10))
      (= key 65) ;;A
      (pc/move! s (V2 -10 0))
      (= key 68) ;;D
      (pc/move! s (V2 10 0))
      (= key 81) ;;Q
      (pc/rotate! s -0.1)
      (= key 69) ;;E
      (pc/rotate! s 0.1)
      (= key 73) ;;I
      (c/assoc!! s :vel (V2 0 -1))
      (= key 75) ;;k
      (c/assoc!! s :vel (V2 0 1))
      (= key 74) ;;j
      (c/assoc!! s :vel (V2 -1 0))
      (= key 76) ;;l
      (c/assoc!! s :vel (V2 1 0))
      (= key 85) ;;U
      (c/assoc!! s :gvel -0.1)
      (= key 79) ;O
      (c/assoc!! s :gvel 0.1)
      (= key 90) ;Z
      nil ;(pc/updateMass! s -1)
      (= key 88) ;;X
      nil ;(pc/updateMass! s 1)
      (= key 67) ;C
      (c/assoc!! s :statF -0.01)
      (= key 86) ;V
      (c/assoc!! s :statF 0.01)
      (= key 66) ;B
      (c/assoc!! s :bounce -0.01)
      (= key 78) ;N
      (c/assoc!! s :bounce 0.01)
      (= key 70);f
      (let [{:keys [pos]} @s
            r1 (-> (py/rectangle (g/rect 0
                                         0
                                         (+ 10 (rand 30))
                                         (+ 10 (rand 30)))
                                 {:mass (rand 30) :friction (rand) :bounce (rand)})
                   (pc/add-body pos))]
        (c/assoc!! r1 :vel (V2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 71) ;;g
      (let [{:keys [pos]} @s
            c1 (-> (py/circle (+ 20 (rand 10))
                              {:mass (rand 30) :friction (rand) :bounce (rand)})
                   (pc/add-body pos))]
        (c/assoc!! c1 :vel (V2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 72);H
      (c/each-set samples
                  (fn [s i]
                    (if (pos? (:m @s))
                      (c/assoc!! s
                                 :vel (V2 (- (rand 500) 250)
                                          (- (rand 500) 250)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-ui-echo "" []
  (let [{:keys [uiEcho cur samples]} @gWorld
        obj (c/nth-set samples cur)
        {:keys [statF bounce m gvel vel angle] [x y] :pos} @obj]
    (->> (str "<p><b>Selected Object:</b>:</p>"
              "<ul style=\"margin:-10px\">"
              "<li>Id: " cur "</li>"
              "<li>Center: " x "," y "</li>"
              "<li>Angle: " angle "</li>"
              "<li>Velocity: " (_1 vel) "," (_2 vel) "</li>"
              "<li>AngluarVelocity: " gvel "</li>"
              "<li>Mass: " m "</li>"
              "<li>Friction: " statF "</li>"
              "<li>Restitution: " bounce "</li>"
              "</ul> <hr>"
              "<p><b>Control</b>: of selected object</p>"
              "<ul style=\"margin:-10px\">"
              "<li><b>Num</b> or <b>Up/Down Arrow</b>: Select Object</li>"
              "<li><b>WASD</b> + <b>QE</b>: Position [Move + Rotate]</li>"
              "<li><b>IJKL</b> + <b>UO</b>: Velocities [Linear + Angular]</li>"
              "<li><b>Z/X</b>: Mass [Decrease/Increase]</li>"
              "<li><b>C/V</b>: Frictrion [Decrease/Increase]</li>"
              "<li><b>B/N</b>: Restitution [Decrease/Increase]</li>"
              "</ul> <hr>"
              "<b>F/G</b>: Spawn [Rectangle/Circle] at selected object"
              "<p><b>H</b>: Excite all objects</p>"
              "<p><b>R</b>: Reset System</p>"
              "<hr>")
         (c/set-js! uiEcho "innerHTML" ))))

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
  (update-ui-echo)
  (draw-game)
  (pc/step*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- my-game "" []
  (set! gWorld (py/init-physics 100 60 (g/rect 0 0 799 449)))
  (let [html (js/document.getElementById "uiEchoString")
        canvas (js/document.getElementById "canvas")
        context (c/call-js! canvas "getContext" "2d")
        {:keys [width height]} @gWorld
        _ (c/set-js!! canvas
                      "width" width
                      "height" height)
        _ (swap! gWorld #(assoc %
                                :uiEcho html
                                :cur 0
                                :canvas canvas :context context))
        right (-> (py/rectangle (g/rect 0 0 400 20)
                                {:mass 0 :friction 0.3 :bounce 0})
                  (pc/add-body (V2 500 200)))
        left (-> (py/rectangle (g/rect 0 0 200 20) {:mass 0})
                 (pc/add-body (V2 100 200)))
        ;r4 (Rectangle (Point2D 10 360) (Size2D 20 100) 0 0 1)]
        bottom (-> (py/rectangle (g/rect 0 0 400 20)
                                 {:mass 0 :friction 1 :bounce 0.5})
                   (pc/add-body (V2 200 400)))
        br (-> (py/rectangle (g/rect 0 0 20 100)
                             {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 400 360)))
        bl (-> (py/rectangle (g/rect 0 0 20 100)
                             {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 10 360)))]
    (pc/rotate! left -2.8)
    (pc/rotate! right 2.8)
    (dotimes [i 4]
      (-> (py/rectangle (g/rect 0 0
                                (+ 10 (rand 50))
                                (+ 10 (rand 50)))
                        {:mass (rand 30) :friction (rand) :bounce (rand)})
          (pc/add-body (V2 (rand (/ width 2))
                           (rand (/ height 2))))
          (c/assoc!! :vel
                     (V2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (py/circle (+ 10 (rand 20))
                     {:mass (rand 30) :friction (rand) :bounce (rand)})
          (pc/add-body (V2 (rand (/ width 2))
                           (rand (/ height 2))))
          (c/assoc!! :vel (V2 (- (rand 60) 30)
                              (- (rand 60) 30)))))
    (run-game-loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/mouseControl mouse-control)
(set! js/userControl user-control)
(set! js/PhysicsGame my-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


