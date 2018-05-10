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
            [czlab.elmo.tictactoe.mmenu :as mu]
            [czlab.elmo.tictactoe.misc :as mc]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onplay "" [& xs]
  (f#* (cx/run* (mu/mmenuScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splashScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [bg (sprite* (cx/gimg :game-bg))
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
        (mc/mapGridPos 3 scale)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




