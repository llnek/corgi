;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.pong.splash

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [with-local-vars
                                   var-set var-get
                                   do-with each-indexed f#*]]
                   [czlab.elmo.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.cc.ccsx :as cx :refer [bsize *xcfg*]]
            [czlab.elmo.pong.mmenu :as mu]
            [czlab.elmo.pong.misc :as mc]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget+ oset!+ oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn-  onPlay "" [] (f#* (cx/run* (mu/mmenuScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splashScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [x y] :as cp} (cx/centerPos)
          {:keys [top bottom left right]} (cx/vbox4)
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          bg (mc/rotFlat?? (sprite* (cx/gimg :game-bg)))
          _ (cx/setXXX! bg {:pos cp})
          tt (-> (sprite* "#title.png")
                 (cx/setXXX! {:pos {:x x :y (* 0.85 top)}}))
          play (cx/gmenu [{:nnn "#play.png" :cb (onPlay)}]
                         {:pos {:x x :y (* 0.1 top)}})]
      (cx/addItem layer bg "bg" -1)
      (cx/addItem layer tt )
      (cx/addItem layer play))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


