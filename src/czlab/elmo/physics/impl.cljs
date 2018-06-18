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
    [czlab.elmo.p2d.physics2d
     :as py :refer [vec2 Point2D Size2D Rectangle Circle]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [half-size* *game-arena*
                    cpos bbox4
                    *game-scene* *xcfg* bsize csize]]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [state]
  (let [{:keys [arena]} @state
        {:keys [width height]} (cx/bbox4->bbox arena)]
    (py/initPhysics -20 60 {:cc2dx? true :width width :height height})
    (let [r1 (Rectangle (Point2D 500 200) (Size2D 400 20) 0 0.3 0)
          r2 (Rectangle (Point2D 200 400) (Size2D 400 20) 0 1 0.5)
          r3 (Rectangle (Point2D 100 200) (Size2D 200 20) 0)
          r4 (Rectangle (Point2D 10 360) (Size2D 20 100) 0 0 1)]
      (py/rotate! r1 2.8)
      (dotimes [i 4]
        (-> (Rectangle (Point2D (rand width)
                                (rand (/ height 2)))
                       (Size2D (+ 10 (rand 50))
                               (+ 10 (rand 50))) (rand 30) (rand) (rand))
            (py/alterShapeAttr! :vel
                                (vec2 (- (rand 60) 30) (- (rand 60) 30))))
        (-> (Circle (Point2D (rand width)
                             (rand (/ height 2)))
                    (+ 10 (rand 20)) (rand 30) (rand) (rand))
            (py/alterShapeAttr! (vec2 (- (rand 60) 30)
                                      (- (rand 60) 30))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [dt]
  (let [state (oget @*game-scene* "gstate")]


    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


