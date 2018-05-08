;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.splash

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with each-indexed f#*]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [csize]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mapGridPos "" [gsz scale]
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
(defn- onplay "" [scene]
  (f#* (js/alert "poo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splashScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [bg (sprite* (cx/getImage :game-bg))
          t (sprite* "#title.png")
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          scale 0.75
          cp (cx/centerPos)
          wb (cx/vbox4)
          pmu (cx/gmenu [{:cb (onplay scene) :nnn "#play.png"}]
                        {:pos (js/cc.p (oget-x cp)
                                       (* 0.1 (oget-top wb)))})]
      ;;background
      (cx/setXXX! bg {:pos cp})
      (cx/addItem layer bg "bkgd" -1)
      ;;title
      (cx/setXXX! t {:pos (js/cc.p (oget-x cp)
                                   (* 0.8 (oget-top wb)))})
      (cx/addItem layer t)
      ;;play button
      (cx/addItem layer pmu)
      ;;draw demo
      ;; we scale down the icons to make it look nicer
      (each-indexed
        (fn [mp pos]
          (let [sp (->> (case pos
                          (1  5  6  7) "#x.png"
                          (0  4) "#z.png"
                          "#o.png")
                        (sprite* ))]
            (cx/setXXX! sp {:pos (cx/vbox4MID mp) :scale scale})
            (cx/addItem layer sp)))
        (mapGridPos 3 scale)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




