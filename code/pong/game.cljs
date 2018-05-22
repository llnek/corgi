;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.pong.game

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [_1 _2 f#* do-with]]
    [czlab.elmo.afx.ccsx :as cx :refer [sprite* attr*]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [*xcfg*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.afx.ecs :as ecs]
            [czlab.elmo.afx.ebus :as ebus]
            ;[czlab.elmo.tictactoe.hud :as hud]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" [mode px py & more]
  (do-with [scene (new js/cc.Scene)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



