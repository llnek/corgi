;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.motion

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [each-indexed f#* n# _1 _2 do-with]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-bottom oget-right
                                   oget-x oget-y oget-left oget-top]])
  (:require [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onceOnly "" []
  (let [s (f#* (.push evQ msg))]
    (ebus/sub+ chan "touch.one.end" s)
    (ebus/sub+ chan "mouse.up" s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn update "" [dt]
  (if-some [msg (.shift evQ)]
    (onGUI msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onGUI "" [evt]
  ;let sel = node.selection, map = node.view.gridMap, rect, sz= map.length
  ;;set the mouse/touch position
  (let [loc (-> (ocall evt "getLocation")
                (js->clj :keywordize true))]
    (reset! sel {:loc loc :cell -1})
    (cond
      (= 0 actor)
      nil
      :else ;;which cell did he click on?
      (some
        (fn [pos]
          (let [rect (nth gridMap pos)]
            (if (and (>= (:x loc) (oget-left rect))
                     (<= (:x loc) (oget-right rect))
                     (>= (:y loc) (oget-bottom rect))
                     (<= (:y loc) (oget-top rect)))
              (do->true (swap! sel #(assoc % :cell pos))))))
        gridRange))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




