;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.physics.impl

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [do->true half*
                    nneg? f#* n# _1 _2 do-with]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [oget-bottom oget-right gcbyn
                    sprite* attr* pos* pos! posX! posY!
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.gfx2d :as gx :refer [vec2 Point2D Size2D]]
    [czlab.elmo.p2d.physics2d
     :as py :refer [Rectangle Circle]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [half-size* *game-arena*
                    cpos bbox4
                    *game-scene* *xcfg* bsize csize]]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [s node i cur]
  (let [{:keys [pos angle radius startPt]} @s
        c (if (= i cur) js/cc.color.RED js/cc.color.GREEN)]
    (ocall! node "drawCircle" (clj->js pos) radius angle 100 true 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rectDraw "" [s node i cur]
  (let [{:keys [vertices width height angle]} @s
        c (if (= i cur) js/cc.color.RED js/cc.color.WHITE)]
    (ocall! node "drawPoly" (clj->js vertices) nil 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [state]
  (let [pw (py/initPhysics -20 60
                          (:arena @state)
                          {:cc2dx? true
                           :rectangle {:draw rectDraw}
                           :circle {:draw circleDraw}})
        {:keys [width height]} @pw]
    (swap! state #(assoc % :phyWorld pw))
    (let [r1 (Rectangle (Point2D 500 400) (Size2D 400 20) 0 0.3 0)
          r2 (Rectangle (Point2D 200 200) (Size2D 400 20) 0 1 0.5)
          r3 (Rectangle (Point2D 100 400) (Size2D 200 20) 0)
          r4 (Rectangle (Point2D 10 240) (Size2D 20 100) 0 0 1)]
      (py/rotate! r1 2.8)
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
                                      (- (rand 60) 30))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [dt]
  (let [state (oget @*game-scene* "gstate")
        ui @*game-arena*
        {:keys [cur samples]}
        (deref (:phyWorld @state))
        node (new js/cc.DrawNode)]
    (ocall! ui "removeAllChildren")
    (cx/addItem ui node)
    (ec/eachStore samples
                  (fn [s i] (py/draw s node i cur)))
    (py/step (* 1000 dt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


