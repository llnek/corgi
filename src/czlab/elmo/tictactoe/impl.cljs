;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.impl

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [do->true each-indexed f#* n# _1 _2 do-with]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-bottom oget-right
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.ccsx :as cx]
    [czlab.elmo.afx.ecs :as ecs]
    [czlab.elmo.afx.ebus :as ebus]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onGUI "" [topic msgTopic & msgs]
  ;let sel = node.selection, map = node.view.gridMap, rect, sz= map.length
  ;;set the mouse/touch position
  (let [evt (_1 msgs)
        e (ocall evt "getLocation")
        loc (js->clj e :keywordize-keys true)]
(js/console.log "e -> " e)
    (js/console.log "clicked -> " (:x loc) ", " (:y loc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- XXonGUI "" [topic msgTopic & msgs]
  ;let sel = node.selection, map = node.view.gridMap, rect, sz= map.length
  ;;set the mouse/touch position
  (let [gridRange (range 9)
        gridMap []
        evt (_1 msgs)
        sel (atom nil)
        actor 1
        loc (-> (ocall evt "getLocation")
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
(defn motion "" [ecs dt]
  (let [scene (deref cx/*game-scene*)
        state (oget scene "?gstate")
        {:keys [evQ]} @state]
    (if-some [msg (.shift evQ)] (apply onGUI msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [scene]
  (let [state (oget scene "?gstate")
        {:keys [ebus ecs evQ]} @state
        cb (fn [& xs] (.push evQ xs))]
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus "mouse.up" cb))
    (if (cx/onTouchOne ebus)
      (ebus/sub+ ebus "touch.one.end" cb))
    (ecs/addSystem ecs motion)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




