;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.misc

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [applyScalarOp half* do-with f#*]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-height oget-width
                    oget-x oget-y oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [csize]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mapGridPos "" [gsz scale]
  ;;memorize the co-ordinates of each cell on the board, so
  ;;we know which cell the user has clicked on.
  (let [sp (sprite* "#z.png")
        ro (* (/ 8 72) scale)
        cp (cx/centerPos)
        cells (* gsz gsz)
        sz (csize sp)
        gridMap (array)
        [W H] (applyScalarOp *
                             scale
                             (oget-width sz)
                             (oget-height sz))
        [gw gh] (applyScalarOp * ro W H)
        zw (+ (* gsz W) (* gw (dec gsz)))
        zh (+ (* gsz H) (* gh (dec gsz)))
        x0 (- (oget-x cp) (half* zw))
        y0 (+ (oget-y cp) (half* zh))]
    (dotimes [_ cells] (.push gridMap nil))
    (loop [row 0 x1 x0 y1 y0]
      (if (< row gsz)
        (recur (inc row)
               x1
               (loop [col 0 x1' x1 y1' y1]
                 (let [y2 (- y1' H)
                       x2 (+ x1' W)]
                   (if-not (< col gsz)
                     (- y2 gh)
                     (do (aset gridMap
                               (+ col (* row gsz))
                               #js{:left x1' :top y1' :right x2 :bottom y2})
                         (recur (inc col) (+ x2 gw) y1'))))))))
    gridMap))

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
;;EOF




