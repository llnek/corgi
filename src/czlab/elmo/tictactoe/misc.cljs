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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with f#*]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [csize]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mapGridPos "" [gsz scale]
  ;;memorize the co-ordinates of each cell on the board, so
  ;;we know which cell the user has clicked on.
  (let [sp (sprite* "#z.png")
        sz (csize sp)
        H (* scale (oget-height sz))
        W (* scale (oget-width sz))
        ro (* (/ 8 72) scale)
        cells (* gsz gsz)
        gh (* ro H)
        gw (* ro W)
        zh (+ (* gsz H) (* gh (- gsz 1)))
        zw (+ (* gsz W) (* gw (- gsz 1)))
        cp (cx/centerPos)
        gridMap (array)
        x0 (- (oget-x cp) (* 0.5 zw))
        y0 (+ (oget-y cp) (* 0.5 zh))]
    (dotimes [n cells] (.push gridMap nil))
    (loop [r 0 x1 x0 y1 y0]
      (if (< r gsz)
        (recur (inc r)
               x1
               (loop [c 0 x1' x1 y1' y1]
                 (let [y2 (- y1' H)
                       x2 (+ x1' W)]
                   (if-not (< c gsz)
                     (- y2 gh)
                     (do (aset gridMap
                               (+ c (* r gsz))
                               #js{:left x1' :top y1' :right x2 :bottom y2})
                         (recur (inc c) (+ x2 gw) y1'))))))))
    gridMap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mapGoalSpace
  "Returns an array of winning combinations."
  [size]

  (let [rows (transient [])
        cols (transient [])
        dx (transient [])
        dy (transient [])]
    (dotimes [r size]
      (let [h (transient [])
            v (transient [])]
        (dotimes [c size]
          (conj! h (+ (* r size) c))
          (conj! v (+ (* c size) r)))
        (conj! rows (persistent! h))
        (conj! cols (persistent! v))
        (conj! dx (+ (* r size) r))
        (conj! dy (+ r (* size (- size r 1))))))
    (into []
          (concat [(persistent! dx) (persistent! dy)]
                  (persistent! rows) (persistent! cols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




