;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.physics_demo

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.p2d.physics2d
             :as py :refer [alterShapeAttr!
                            updateMass!
                            move! rotate!
                            Rectangle Circle *gWorld*]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [pythag pythagSQ TWO-PI PI vec2 VEC_ZERO _cocos2dx?
                            v2-len v2-add v2-sub v2-dot Point2D Size2D
                            v2-negate v2-scale v2-cross v2-rot v2-norm v2-dist]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawCollisionInfo "" [ci context]
  (let [{:keys [start end]} @ci
        {sx :x sy :y} start
        {ex :x ey :y} end]
    (ocall! context "beginPath")
    (ocall! context "moveTo" sx sy)
    (ocall! context "lineTo" ex ey)
    (ocall context "closePath")
    (oset! context "!strokeStyle" "orange")
    (ocall! context "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- userControl "" [evt]
  (let [key (or (oget evt "?keyCode")
                (oget evt "?which"))
        {:keys [cur samples]} @*gWorld*
        len (ec/countStore samples)
        s (ec/nthStore samples cur)
        offset (- key 48)]
    (cond
      (and (>= key 48)
           (<= key 57))
      (if (< (- offset 48) len)
        (swap! *gWorld* #(assoc % :cur offset)))
      (= key 38) ;up arrow
      (if (pos? cur)
        (swap! *gWorld* #(assoc % :cur (dec cur))))
      (= key 40) ;down arrow
      (if (< cur (- len 1))
        (swap! *gWorld* #(assoc % :cur (inc cur))))
      (= key 87) ;;W
      (move! s (vec2 0 -10))
      (= key 83) ;;S
      (move! s (vec2 0 10))
      (= key 65) ;;A
      (move! s (vec2 -10 0))
      (= key 68) ;;D
      (move! s (vec2 10 0))
      (= key 81) ;;Q
      (rotate! s -0.1)
      (= key 69) ;;E
      (rotate! s 0.1)
      (= key 73) ;;I
      (alterShapeAttr! s :vel (vec2 0 -1))
      (= key 75) ;;k
      (alterShapeAttr! s :vel (vec2 0 1))
      (= key 74) ;;j
      (alterShapeAttr! s :vel (vec2 -1 0))
      (= key 76) ;;l
      (alterShapeAttr! s :vel (vec2 1 0))
      (= key 85) ;;U
      (alterShapeAttr! s :angVel -0.1)
      (= key 79) ;O
      (alterShapeAttr! s :angVel 0.1)
      (= key 90) ;Z
      (updateMass! s -1)
      (= key 88) ;;X
      (updateMass! s 1)
      (= key 67) ;C
      (alterShapeAttr! s :sticky -0.01)
      (= key 86) ;V
      (alterShapeAttr! s :sticky 0.01)
      (= key 66) ;B
      (alterShapeAttr! s :bounce -0.01)
      (= key 78) ;N
      (alterShapeAttr! s :bounce 0.01)
      (= key 70);f
      (let [{:keys [pos]} @s
            r1 (Rectangle pos
                          (Size2D (+ 10 (rand 30))
                                  (+ 10 (rand 30))) (rand 30) (rand) (rand))]
        (alterShapeAttr! r1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 71) ;;g
      (let [{:keys [pos]} @s
            c1 (Circle pos
                       (+ 20 (rand 10)) (rand 30) (rand) (rand))]
        (alterShapeAttr! c1 (vec2 (- (rand 300) 150) (- (rand 300) 150))))
      (= key 72);H
      (ec/eachStore samples
                    (fn [s i]
                      (if (pos? (:invMass @s))
                        (alterShapeAttr! s
                                         :vel (vec2 (- (rand 500) 250)
                                                    (- (rand 500) 250)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateUIEcho "" []
  (let [{:keys [uiEcho cur samples]} @*gWorld*
        obj (ec/nthStore samples cur)
        {:keys [sticky bounce invMass angVel vel angle pos]} @obj]
    (->> (str "<p><b>Selected Object:</b>:</p>"
              "<ul style=\"margin:-10px\">"
              "<li>Id: " cur "</li>"
              "<li>Center: " (:x pos) "," (:y pos) "</li>"
              "<li>Angle: " angle "</li>"
              "<li>Velocity: " (:x vel) "," (:y vel) "</li>"
              "<li>AngluarVelocity: " angVel "</li>"
              "<li>Mass: " (invert invMass) "</li>"
              "<li>Friction: " sticky "</li>"
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
         (oset! uiEcho "!innerHTML" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawGame "" []
  (let [{:keys [cur samples width height context]} @*gWorld*]
    (ocall! context "clearRect" 0 0 width height)
    (ec/eachStore samples
                  (fn [s i]
                    (when (:valid? @s)
                      (oset! context
                             "!strokeStyle" (if (= i cur) "red" "blue"))
                      (py/draw s context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runGameLoop "" []
  (js/requestAnimationFrame #(runGameLoop))
  (updateUIEcho)
  (drawGame)
  (py/step*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- myGame "" []
  (py/initPhysics 20 60 {:left 0 :right 799 :top 0 :bottom 449})
  (let [html (js/document.getElementById "uiEchoString")
        canvas (js/document.getElementById "canvas")
        context (ocall! canvas "getContext" "2d")
        {:keys [width height]} @*gWorld*
        _ (oset! canvas "height" height)
        _ (oset! canvas "width" width)
        _ (swap! *gWorld* #(assoc %
                                  :uiEcho html
                                  :canvas canvas :context context))
        r1 (Rectangle (Point2D 500 200) (Size2D 400 20) 0 0.3 0)
        r2 (Rectangle (Point2D 200 400) (Size2D 400 20) 0 1 0.5)
        r3 (Rectangle (Point2D 100 200) (Size2D 200 20) 0)
        r4 (Rectangle (Point2D 10 360) (Size2D 20 100) 0 0 1)]
    (rotate! r1 2.8)
    (dotimes [i 4]
      (-> (Rectangle (Point2D (rand (/ width 2))
                              (rand (/ height 2)))
                     (Size2D (+ 10 (rand 50))
                             (+ 10 (rand 50))) (rand 30) (rand) (rand))
          (py/alterShapeAttr! :vel
                              (vec2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (Circle (Point2D (rand (/ width 2))
                           (rand (/ height 2)))
                  (+ 10 (rand 20)) (rand 30) (rand) (rand))
          (py/alterShapeAttr! (vec2 (- (rand 60) 30)
                                    (- (rand 60) 30)))))
    (runGameLoop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(set! js/userControl userControl)
(set! js/MyGame myGame)


