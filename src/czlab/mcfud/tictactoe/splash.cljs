;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.splash

  (:require-macros [czlab.mcfud.afx.core :as ec :refer [do-with each* f#*]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   pos! scale!
                                   oget-x oget-y sprite*]])

  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [bsize xcfg vrect vec2->cp
                            add-> cp->vec2 rect->box4 mid-rect]]
            [czlab.mcfud.tictactoe.mmenu :as mu]
            [czlab.mcfud.tictactoe.misc :as mc]
            [czlab.mcfud.afx.math :refer [vec2]]
            [czlab.mcfud.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onplay

  "Action on play."
  [& xs]

  (f#* (cx/run* (mu/mmenuScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splashScene

  "Create a game splash scene."
  []

  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [GRID-SIZE]} (:game @xcfg)
          bg (sprite* (cx/gimg :game-bg))
          t (sprite* "#title.png")
          layer (new js/cc.Layer)
          scale 0.75
          B (vrect)
          {:keys [peak]} (rect->box4 B)
          [cx cy :as cp] (mid-rect B)
          pmu (cx/gmenu [{:nnn "#play.png"
                          :cb (onplay scene)}]
                        {:pos (vec2 cx (* 0.1 peak))})]
      (pos! bg (vec2->cp cp))
      (add-> scene layer)
      ;;background
      (add-> layer bg "bkgd" -1)
      ;;title
      (pos! t (vec2 cx (* 0.8 peak)))
      (add-> layer t)
      ;;play button
      (add-> layer pmu)
      ;;draw demo
      ;; we scale down the icons to make it look nicer
      (each* (fn [mp pos]
               (let [sp (->> (case pos
                               (1  5  6  7) "#x.png"
                               (0  4) "#z.png"
                               "#o.png")
                             (sprite*))]
                 (pos! sp (vec2->cp (mid-rect mp)))
                 (scale! sp scale)
                 (add-> layer sp)))
             (mc/mapGridPos B GRID-SIZE 0.75)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




