;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.gfx2d

  (:require [czlab.mcfud.afx.math
             :as m :refer [TWO-PI vec2]]
            [oops.core :as oc]
            [czlab.mcfud.afx.core
             :as c :refer [let->nil do->nil atom? cc+ _1 _2 n# num??]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private _cocos2dx? true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn canvas-batch!
  "Apply a sequence of operations to the html5 canvas,
  with each op being [method arg1 arg2 ...]"
  [ctx & callArgs]
  (doseq [a callArgs
          :let [[m & args] a]] (oc/oapply!+ ctx m args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord TextStyle [font fill align base])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn text-style
  "Html5 Text Style object."
  [font fill align base]
  ;"14px 'Arial'" "#dddddd" "left" "top"
  (new TextStyle font fill align base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-shape
  "Draw the shape onto the html5 canvas."
  [s canvas & args]
  (let->nil
    [{:keys [draw]}
     (cond (atom? s) @s
           (map? s) s
           :else (c/raise! "Bad shape."))] (apply draw s canvas args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cfg-style!
  "Apply styles to the canvas."
  [canvas styleObj]
  (let->nil
    [{:keys [line stroke]} styleObj
     {:keys [cap width]} line
     {:keys [style]} stroke]
    (when line
      (if cap (c/set-js! canvas "lineCap" cap))
      (if width (c/set-js! canvas "lineWidth" width)))
    (when stroke
      (if style (c/set-js! canvas "strokeStyle" style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-poly*
  "Draw and connect this set of points onto the canvas."
  [vertices canvas]
  {:pre [(or (list? vertices)(vector? vertices))]}
  (do->nil
    (c/call-js! canvas "beginPath")
    (loop [i 0
           SZ (n# vertices)]
      (when (< i SZ)
        (let [i2 (m/wrap?? i SZ)
              [x1 y1] (nth vertices i)
              [x2 y2] (nth vertices i2)]
          (c/jsto canvas
                  ["moveTo" x1 y1]
                  ["lineTo" x2 y2])
          (recur (+ 1 i) SZ))))
    (c/call-js! canvas "stroke")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-poly
  "Draw a polygon."
  [p canvas]
  (draw-poly* (:vertices p) canvas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-circle*
  "Draw a circle onto the canvas.  If a starting point
  is provided, draw a line to the center."
  [center radius angle canvas & [startPt?]]
  (let->nil
    [[cx cy] center
     angle' (num?? angle 0)]
    (c/jsto canvas
            ["beginPath"]
            ["arc" cx cy radius 0 TWO-PI true])
    (when startPt?
      (let [[x y] (-> (vec2 (+ cx radius) cy)
                      (m/vec-rot angle' center))]
        (c/jsto canvas
                ["moveTo" cx cy] ["lineTo" x y])))
    (c/jsto canvas
            ["closePath"] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-circle
  "Draw a circle."
  [C canvas center & [angle startPt?]]
  (let [{:keys [radius]} C]
    (draw-circle* (or center (m/vz2))
                  radius angle canvas startPt?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-rect
  "Draw a rectangle, not used."
  [R canvas pos & [angle]]
  (let->nil
    [{:keys [width height]} R
     [cx cy] (or pos (m/vz2))
     left (- cx (/ width 2))
     top (- cy (/ height 2))]
    (c/jsto canvas
            ["save"]
            ["translate" left top]
            ["rotate" (num?? angle 0)]
            ["strokeRect" 0 0 width height] ["restore"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn draw-line
  "Draw a line."
  [line canvas]
  (let->nil
    [{:keys [v1 v2]} line
     [ax ay] v1
     [ex ey] v2]
    (c/jsto canvas
            ["beginPath"]
            ["moveTo" ax ay] ["lineTo" ex ey] ["stroke"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

