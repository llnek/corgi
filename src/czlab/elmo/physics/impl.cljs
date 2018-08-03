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
     :as ec :refer [do->true half* assoc!!
                    nneg? f#* n# _1 _2 do-with]]
    [czlab.elmo.cc.ccsx
     :as cx :refer [oget-bottom oget-right gcbyn
                    sprite* attr* pos* pos! posX! posY!
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.gfx2d
     :as gx :refer [PI m2-vmult mat2*
                    v2-add v2-rot vec2 Point2D Size2D]]
    [czlab.elmo.p2d.core :as pc :refer [addBody]]
    [czlab.elmo.p2d.physics2d :as py]
    [czlab.elmo.p2d.impulse :as im]
    [czlab.elmo.cc.ccsx
     :as cx :refer [half-size* *game-arena*
                    cpos bbox4
                    *game-scene* *xcfg* bsize csize]]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- circleDraw "" [B node i cur]
  (let [{:keys [pos angle]
         {:keys [radius]} :shape} @B
        c (if (= i cur)
            js/cc.color.RED js/cc.color.GREEN)]
    ;; flip rotation for cocos2d
    (ocall! node "drawCircle" (clj->js pos) radius (- angle) 100 true 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- polyDraw "" [B node i cur]
  (let [{{:keys [vertices]} :shape :keys[pos angle]} @B
        c (if (= i cur) js/cc.color.RED js/cc.color.WHITE)
        angle' (- (* 2 angle))
        vs (mapv #(v2-rot % pos angle') vertices)]
    ;;flip rotation for cocos2d
    (ocall! node "drawPoly" (clj->js vertices) nil 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- XXpolyDraw "" [B node i cur]
  (let [{{:keys [u vertices]} :shape :keys[pos angle]} @B
        c (if (= i cur) js/cc.color.RED js/cc.color.WHITE)
        ;u (mat2* (- angle))
        vs (mapv #(v2-add pos (m2-vmult u %)) vertices)]
    ;;flip rotation for cocos2d
    (ocall! node "drawPoly" (clj->js vs) nil 2 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- drawBody "" [& args]
  (let [B (_1 args)
        {:keys [shape]} @B
        {:keys [type]} shape]
    (cond
      (= type :circle)
      (apply circleDraw args)
      :else
      (apply polyDraw args)) B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [state]
  (let [pw (py/initPhysics -98 60
                          (:arena @state)
                          {:cc2dx? true
                           :cur 0
                           :bodyDrawer drawBody})
        _ (swap! state #(assoc % :phyWorld pw))
        {:keys [width height]} @pw
        p8 (* 0.8 width)
        p2 (* 0.2 width)
        right (-> (py/Rectangle (Size2D 400 20) {:mass 0 :friction 0.3 :bounce 0})
                  (addBody (Point2D (* 0.7 width) 500))
                  (pc/setStatic!))
        left (-> (py/Rectangle (Size2D 200 20) {:mass 0})
                 (addBody (Point2D (* 0.3 width) 500))
                 (pc/setStatic!))
        bottom (-> (py/Rectangle (Size2D p8 20)
                              {:mass 0 :friction 1 :bounce 0.2})
                   (addBody (Point2D (* 0.5 width) 100))
                   (pc/setStatic!))
        br (-> (py/Rectangle (Size2D 20 600) {:mass 0 :friction 0 :bounce 1})
               (addBody (Point2D p2 100))
               (pc/setStatic!))
        bl (-> (py/Rectangle (Size2D 20 600) {:mass 0 :friction 0 :bounce 1})
               (addBody (Point2D p8 100))
               (pc/setStatic!))]
    (pc/rotate! left -2.8)
    (pc/rotate! right 2.8)
    (dotimes [i 4]
      (-> (py/Rectangle (Size2D (+ 10 (rand 50))
                             (+ 10 (rand 50)))
                     {:mass (rand 30) :friction (rand) :bounce (rand)})
          (addBody (Point2D (+ (/ width 2) (rand 100))
                            (rand (/ height 2))))
          (py/alterBodyAttr! :vel
                              (vec2 (- (rand 60) 30) (- (rand 60) 30))))
      (-> (py/Circle (+ 10 (rand 20))
                  {:mass (rand 30) :friction (rand) :bounce (rand)})
          (addBody (Point2D (+ (/ width 2) (rand 100))
                            (rand (/ height 2))))
          (py/alterBodyAttr! :vel (vec2 (- (rand 60) 30)
                                         (- (rand 60) 30)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- morePoly "" [x y]
  (let [e (ec/randRange 50 100)
        ne (- e)
        vs (transient [])
        _ (dotimes [_ (ec/randRange 3 12)]
            (conj! vs (vec2 (ec/randRange ne e) (ec/randRange ne e))))
        p (im/Polygon (persistent! vs)
                      {:mass (rand 30) :friction (rand) :bounce (rand)})]
    (im/setOrient! p (ec/randRange (- PI) PI))
    (assoc!! p :dynaF 0.2)
    (addBody p x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXinit "" [state]
  (let [pw (im/initPhysics -98 60
                          (:arena @state)
                          {:cc2dx? true :cur 0 :bodyDrawer drawBody})
        _ (swap! state #(assoc % :phyWorld pw))
        {:keys [width height]} @pw
        p8 (* 0.8 width)
        p2 (* 0.2 width)
        right (-> (im/PolygonBox (Size2D 400 20) {:mass 0 :friction 0.3 :bounce 0})
                  (addBody (Point2D (* 0.7 width) 500))
                  (pc/setStatic!))
        left (-> (im/PolygonBox (Size2D 200 20) {:mass 0})
                 (addBody (Point2D (* 0.3 width) 500))
                 (pc/setStatic!))
        bottom (-> (im/PolygonBox (Size2D p8 20)
                               {:mass 0 :friction 1 :bounce 0.5})
                   (addBody (Point2D (* 0.5 width) 100))
                   (pc/setStatic!))
        br (-> (im/PolygonBox (Size2D 20 100) {:mass 0 :friction 0 :bounce 1})
               (addBody (Point2D p2 100))
               (pc/setStatic!))
        bl (-> (im/PolygonBox (Size2D 20 100) {:mass 0 :friction 0 :bounce 1})
               (addBody (Point2D p8 100))
               (pc/setStatic!))]
    (im/setOrient! left -2.8)
    (im/setOrient! right 2.8)
    (dotimes [i 4]
      (morePoly (+ (/ width 2) (rand 100))
                (rand (/ height 2)))
      (-> (im/Circle (+ 10 (rand 20))
                     {:mass (rand 30) :friction (rand) :bounce (rand)})
          (addBody (Point2D (+ (/ width 2) (rand 100))
                            (rand (/ height 2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn updateECS "" [dt]
  (let [state (oget @*game-scene* "gstate")
        ui @*game-arena*
        {:keys [cur samples]}
        @(:phyWorld @state)
        node (new js/cc.DrawNode)]
    (ocall! ui "removeAllChildren")
    (cx/addItem ui node)
    (ec/eachStore samples
                  (fn [s i]
                    (pc/draw s node i cur))) (pc/step (* 1000 dt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


