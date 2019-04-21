;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.misc

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [half* do-with f#*]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y sprite*]])
  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [mid-rect bsize]]
            [czlab.mcfud.afx.math :refer [vec2]]
            [czlab.mcfud.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mapGridPos

  ""
  [B gsz scale]

  ;;memorize the co-ordinates of each cell on the board, so
  ;;we know which cell the user has clicked on.
  (let [[cx cy :as cp] (mid-rect B)
        out (transient [])
        sp (sprite* "#z.png")
        ro (* (/ 8 72) scale)
        cells (* gsz gsz)
        sz (bsize sp)
        H (* scale (:height sz))
        W (* scale (:width sz))
        gw (* ro W)
        gh (* ro H)
        zw (+ (* gsz W) (* gw (dec gsz)))
        zh (+ (* gsz H) (* gh (dec gsz)))
        x0 (- cx (half* zw))
        y0 (+ cy (half* zh))]
    (loop [row 0 x1 x0 y1 y0]
      (if (< row gsz)
        (recur (+ 1 row)
               x1
               (loop [col 0 x1' x1 y1' y1]
                 (let [y2 (- y1' H)
                       x2 (+ x1' W)]
                   (if-not (< col gsz)
                     (- y2 gh)
                     (do (conj! out
                                (new-rect x1'
                                          y2
                                          (- x2 x1')
                                          (- y1' y2)))
                         (recur (+ 1 col)
                                (+ x2 gw) y1'))))))
        (persistent! out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mapGoalSpace

  "Returns an array of winning combinations."
  [size]

  (let [rows (transient [])
        cols (transient [])
        dx (transient [])
        dy (transient [])]
    (dotimes [row size]
      (let [h (transient [])
            v (transient [])]
        (dotimes [col size]
          (conj! h (+ (* row size) col))
          (conj! v (+ (* col size) row)))
        (conj! rows (persistent! h))
        (conj! cols (persistent! v))
        (conj! dx (+ (* row size) row))
        (conj! dy (+ row (* size (- size row 1))))))
    (into []
          (concat [(persistent! dx) (persistent! dy)]
                  (persistent! rows) (persistent! cols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pkFlip

  ""
  [img flip?]

  (str "#" img (if flip? ".i.png" ".png")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xrefImg

  ""
  [value]

  (case value 88 "x" 79 "o" "z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- value->symbol

  ""
  [value flip?]

  (sprite* (pkFlip (xrefImg value) flip?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


