;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.pong.misc

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [applyScalarOp half* do-with f#*]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-height oget-width
                    oget-x oget-y oget-top sprite* ]])
  (:require [czlab.elmo.afx.ccsx
             :as cx :refer [*game-arena* *xcfg*
                            *game-scene* bsize half-size*]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotFlat?? "" [obj]
  (if-not (cx/isPortrait?)
    (ocall! obj "setRotation" 90)) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clampBall "" [sp]
  (let [state (oget @*game-scene* "?gstate")
        {:keys [ball arena]} @state
        {:keys [top right bottom left]} arena
        [hw hh] (half-size* ball)
        pt (ocall sp "getPosition")]
    (if (> (+ (oget-y pt) hh) top) (ocall! sp "setPositionY" (- top hh)))
    (if (< (- (oget-x pt) hw) left) (ocall! sp "setPositionX" (+ left hw)))
    (if (> (+ (oget-x pt) hw) right) (ocall! sp "setPositionX" (- right hw)))
    (if (< (- (oget-y pt) hh) bottom) (ocall! sp "setPositionY" (+ bottom hh)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clampPaddle "" [pad]
  (let [state (oget @*game-scene* "?gstate")
        {:keys [paddle arena]} @state
        {:keys [top right bottom left]} arena
        [hw hh] (half-size* paddle)
        pt (ocall pad "getPosition")]
    (if (cx/isPortrait?)
      (do (if (< (- (oget-x pt) hw) left) (ocall! pad "setPositionX" (+ left hw)))
          (if (> (+ (oget-x pt) hw) right) (ocall! pad "setPositionX" (- right hw))))
      (do (if (< (- (oget-y pt) hh) bottom) (ocall! pad "setPositionY" (+ bottom hh)))
          (if (> (+ (oget-y pt) hh) top) (ocall! pad "setPositionY" (- top hh)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


