;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.game

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#* do-with each-indexed]]
                   [czlab.elmo.afx.ccsx :as cx :refer [sprite*]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [*xcfg*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.misc :as mc]
            [czlab.elmo.tictactoe.hud :as hud]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- showGrid "" [node]
  (let [{:keys [GRID-SIZE
                CELLS CV-Z]} (:game @*xcfg*)]
    (reduce
      (fn [acc mp]
        (let [sp (-> (sprite* "#z.png")
                     (cx/setXXX! {:pos (cx/vbox4MID mp)}))]
          (cx/addItem node sp)
          (conj! acc [sp CV-Z])))
      (transient [])
      (mc/mapGridPos GRID-SIZE 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gameLayer "" []
  (do-with [layer (new js/cc.Layer)]
    (let [cp (cx/centerPos)
          wb (cx/vbox4)
          bg (-> (cx/gimg :game-bg)
                 (sprite*)
                 (cx/setXXX! {:pos cp}))]
      (cx/addItem layer bg)
      (showGrid layer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" [px py & more]
  (do-with [scene (new js/cc.Scene)]
    (let [gl (gameLayer)
          hud (hud/hudLayer px py)]
      (cx/addItem scene gl "main" 1)
      (cx/addItem scene hud "hud" 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



