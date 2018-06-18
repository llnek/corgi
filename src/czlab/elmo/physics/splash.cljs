;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.physics.splash

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with each-indexed f#*]]
                   [czlab.elmo.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.cc.ccsx :as cx :refer [bsize *xcfg*]]
            [czlab.elmo.physics.game :as ga]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn-  onPlay "" [] (f#* (cx/run* (ga/gameScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splashScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [x y] :as cp} (cx/centerPos)
          {:keys [top bottom left right]} (cx/vbox4)
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          tt (cx/bmfLabel
               "Physics!"
               (cx/gfnt :title)
               {:pos {:x (:x cp)
                      :y (* 0.8 top)}
                :color (js/cc.color "#EDFF90")})
          play (cx/gmenu [{:nnn "#play.png" :cb (onPlay)}]
                         {:pos {:x x :y (* 0.1 top)}})]
      (cx/addItem layer tt )
      (cx/addItem layer play))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


