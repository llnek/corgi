;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.options

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with f#*]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [bsize *xcfg*]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [czlab.elmo.tictactoe.game :as ga]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn optionsScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          {:keys [top] :as B} (cx/vbox4)
          cp (cx/vbox4MID B)
          tt (cx/bmfLabel
               (cx/l10n "%options")
               (cx/gfnt :title)
               {:pos {:x (:x cp)
                      :y (* 0.8 top)}
                :color (js/cc.color "#F6B17F")})
          mnu (cx/gmenu
                [{:nnn "#online.png" :cb onnetplay}
                 {:nnn "#player2.png" :cb (onplayXXX 2)}
                 {:nnn "#player1.png" :cb (onplayXXX 1)}] {:pos cp})]
      (cx/setXXX! bg {:pos cp})
      (cx/addItem layer bg "bg" -1)
      (cx/addItem layer tt)
      (cx/addItem layer mnu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

