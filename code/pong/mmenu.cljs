;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.pong.mmenu

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with f#*]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [bsize *xcfg*]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [czlab.elmo.pong.game :as ga]
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
(defn- onplayXXX "" [mode]
  (let [{:keys [CV-X CV-O CC-X CC-O CX CO]} (:game @*xcfg*)
        ;if mode 1 -> p2 is computer(2)
        ;if mode 2 -> p2 is person (1)
        p2cat (if (= 1 mode) 2 1)]
    (f#*
      (cx/run* (ga/gameScene
                 mode
                 [CX 1 CV-X CC-X (cx/l10n "%p1")(cx/l10n "%player1")]
                 (concat [CO p2cat CV-O CC-O]
                         (if (= 1 mode)
                           [(cx/l10n "%cpu") (cx/l10n "%computer")]
                           [(cx/l10n "%p2") (cx/l10n "%player2")])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mmenuScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [
          bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          cp (cx/centerPos)
          wb (cx/vbox4)
          tt (cx/bmfLabel
               (cx/l10n "%mmenu")
               (cx/gfnt :title)
               {:pos {:x (:x cp)
                      :y (* 0.8 (:top wb))}
                :color (js/cc.color "#F6B17F")})
          mnu (cx/gmenu
                [{:nnn "#online.png" :cb onnetplay}
                 {:nnn "#player2.png" :cb (onplayXXX 2)}
                 {:nnn "#player1.png" :cb (onplayXXX 1)}] {:pos cp})]
      (cx/setXXX! bg {:pos cp})
      (cx/addItem layer bg "bg" -1)
      (cx/addItem layer tt)
      ;const color= cc.color('#5E3178'),
      (cx/addItem layer mnu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



