;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.mmenu

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with each-indexed f#*]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [csize]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [czlab.elmo.tictactoe.game :as ga]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

(defn- netScene [& xs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onnetplay "" [& xs]
  (let [pms {:no (f#* (cx/run* nil))
             :yes (fn [ws p0 msg]
                    (->> (merge {:ws ws :pnum p0} msg)
                         (ga/gameScene (:1 msg) (:2 msg))
                         (cx/run*)))}]
    (cx/run* (netScene pms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onplay1 "" [& xs]
  (->> (ga/gameScene [:1 (cx/l10n "%p1") (cx/l10n "%player1")]
                  [:2 (cx/l10n "%cpu") (cx/l10n "%computer")])
       (cx/run* )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onplay2 "" [& xs]
  (->> (ga/gameScene [:1 (cx/l10n "%p1") (cx/l10n "%player1")]
                  [:2 (cx/l10n "%p2") (cx/l10n "%player2")])
       (cx/run* )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mmenuScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          cp (cx/centerPos)
          wb (cx/vbox4)
          tt (cx/bmfLabel
               (cx/l10n "%mmenu")
               (cx/gfnt :title)
               {:pos (js/cc.p (oget-x cp)
                              (* 0.8 (oget-top wb)))
                :color (js/cc.color "#F6B17F")})
          mnu (cx/gmenu
                [{:nnn "#online.png" :cb onnetplay}
                 {:nnn "#player2.png" :cb onplay2}
                 {:nnn "#player1.png" :cb onplay1}] {:pos cp})]
      (cx/setXXX! bg {:pos cp})
      (cx/addItem layer bg "bkgd" -1)
      (cx/addItem layer tt)
      ;const color= cc.color('#5E3178'),
      (cx/addItem layer mnu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

