;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.physics.impl

  (:require [czlab.mcfud.afx.core :as c :refer [_1 _2]]
            [czlab.mcfud.p2d.physics2d :as py]
            [czlab.mcfud.p2d.impulse :as im]
            [czlab.mcfud.afx.geo :as g]
            [czlab.mcfud.p2d.core :as pc]
            [czlab.mcfud.cc.ccsx :as x :refer [xcfg]]
            [czlab.mcfud.afx.math :as m :refer [PI V2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private gWorld nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circle-draw "" [B node i cur]
  (let [{:keys [angle] {:keys [radius]} :shape [x y] :pos} @B
        c (if (= i cur) js/cc.color.RED js/cc.color.GREEN)]
    ;; flip rotation for cocos2d
    (c/call-js! node "drawCircle" #js {:x x :y y} radius (- angle) 100 true 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- poly-draw "" [B node i cur]
  (let [{{:keys [vertices]} :shape :keys[pos angle]} @B
        c (if (= i cur) js/cc.color.RED js/cc.color.WHITE)
        angle' (- (* 2 angle))
        vs (mapv #(let [[x y] (m/vec-rot % angle' pos)]
                   #js {:x x :y y}) vertices)]
    ;;flip rotation for cocos2d
    (c/call-js! node "drawPoly" (clj->js vs) nil 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- XXpoly-draw "" [B node i cur]
  (let [{{:keys [u vertices]} :shape :keys[pos angle]} @B
        c (if (= i cur) js/cc.color.RED js/cc.color.WHITE)
        vs (mapv #(let
                    [[x y] (m/vec-add pos
                                      (m/mat-vmult u %))]
                    #js {:x x :y y}) vertices)]
    ;;flip rotation for cocos2d
    (c/call-js! node "drawPoly" (clj->js vs) nil 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- draw-body "" [B & args]
  (let [{:keys [shape]} @B
        {:keys [type]} shape]
    (cond
      (= type :circle)
      (apply circle-draw B args)
      :else
      (apply poly-draw B args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" []
  (let [sz (get-in @xcfg [:game :size])
        r (g/rect (x/oget-x sz)
                  (x/oget-y sz)
                  (x/oget-width sz)(x/oget-height sz))]
    (set! gWorld
          (py/init-physics -98 60 r
                           {:cur 0 :body-drawer draw-body})))
  (let [{:keys [width height]} @gWorld
        [hw hh] (c/mapfv / 2 width height)
        [p8 p2] (c/mapfv * width 0.8 0.2)
        right (-> (py/rectangle (g/area 400 20)
                                {:mass 0 :friction 0.3 :bounce 0})
                  (pc/add-body (V2 (* .7 width) 500))
                  (pc/set-static!))
        left (-> (py/rectangle (g/area 200 20) {:mass 0})
                 (pc/add-body (V2 (* .4 width) 600))
                 (pc/set-static!))
        bottom (-> (py/rectangle (g/area p8 20)
                                 {:mass 0 :friction 1 :bounce 0.2})
                   (pc/add-body (V2 hw 100))
                   (pc/set-static!))
        br (-> (py/rectangle (g/area 20 600)
                             {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 p2 100))
               (pc/set-static!))
        bl (-> (py/rectangle (g/area 20 600)
                             {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 p8 100))
               (pc/set-static!))]
    (pc/rotate! left 2.8)
    (pc/rotate! right -2.8)
    (dotimes [i 4]
      (-> (py/rectangle (g/area (+ 10 (rand 50))
                                (+ 10 (rand 50)))
                        {:mass (rand 30) :friction (rand) :bounce (rand)})
          (pc/add-body (V2 (+ hw (* (rand) 500)) (rand hh )))
          (c/assoc!! :vel (V2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (py/circle (+ 10 (rand 20))
                     {:mass (rand 30) :friction (rand) :bounce (rand)})
          (pc/add-body (V2 (+ hw (* (rand) 400)) (rand hh )))
          (c/assoc!! :vel (V2 (- (rand 60) 30) (- (rand 60) 30)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- more-poly "" [at]
  (let [e (c/rand-range 50 100)
        ne (- e)
        vs (transient [])
        _ (dotimes [_ (c/rand-range 3 12)]
            (conj! vs (V2 (c/rand-range ne e) (c/rand-range ne e))))
        p (im/polygon (c/ps! vs)
                      {:mass (rand 30) :friction (rand) :bounce (rand)})]
    (im/set-orient! p (c/rand-range (- PI) PI))
    (c/assoc!! p :dynaF 0.2)
    (pc/add-body p at)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXinit "" []
  (let [sz (get-in @xcfg [:game :size])
        r (g/rect (x/oget-x sz)
                  (x/oget-y sz)
                  (x/oget-width sz)(x/oget-height sz))]
    (set! gWorld (im/init-physics
                 -98 60 r
                 {:cc2dx? true :cur 0 :body-drawer draw-body})))
  (let [{:keys [width height]} @gWorld
        [hw hh] (c/mapfv / 2 width height)
        [p8 p2] (c/mapfv * width 0.8 0.2)
        right (-> (im/polygon-box (g/area 400 20)
                                  {:mass 0 :friction 0.3 :bounce 0})
                  (pc/add-body (V2 (* .7 width) 500))
                  (pc/set-static!))
        left (-> (im/polygon-box (g/area 200 20) {:mass 0})
                 (pc/add-body (V2 (* .4 width) 600))
                 (pc/set-static!))
        bottom (-> (im/polygon-box (g/area p8 20)
                                   {:mass 0 :friction 1 :bounce 0.5})
                   (pc/add-body (V2 hw 100))
                   (pc/set-static!))
        br (-> (im/polygon-box (g/area 20 100)
                               {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 p2 100))
               (pc/set-static!))
        bl (-> (im/polygon-box (g/area 20 100)
                               {:mass 0 :friction 0 :bounce 1})
               (pc/add-body (V2 p8 100))
               (pc/set-static!))]
    (im/set-orient! left 2.8)
    (im/set-orient! right -2.8)
    (dotimes [i 4]
      (more-poly (V2 (+ hw (* (rand) 600)) (rand hh)))
      (-> (im/circle (+ 10 (rand 20))
                     {:mass (rand 30) :friction (rand) :bounce (rand)})
          (pc/add-body (V2 (+ hw (* (rand) 500)) (rand hh)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-game "" [dt]
  (let [{{:keys [scene]} :game} @xcfg
        {:keys [cur samples]} @gWorld
        ui (x/gcbyn scene :arena)]
    (if-some [n (x/gcbyn ui :dnode)] (x/remove! n))
    (let [node (x/add-> ui (new js/cc.DrawNode) "dnode")]
      (c/each-set samples (fn [s i] (pc/draw s node i cur))))
    (pc/step (* 1000 dt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


