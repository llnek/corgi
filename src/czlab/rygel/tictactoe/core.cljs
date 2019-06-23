;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.tictactoe.core

  (:require [czlab.mcfud.afx.core
             :as c
             :refer [if-some+ fn_0 fn_* fn_1 n# _1 _2]]
            [oops.core :as oc]
            [czlab.rygel.tictactoe.impl :as b]
            [czlab.mcfud.cc.dialog :as d]
            [czlab.mcfud.afx.ebus :as u]
            [czlab.mcfud.afx.algos :as a]
            [czlab.mcfud.cc.ccsx
             :as x :refer [P-BOT CV-X CV-Z CV-O xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn write-status [msg]
  (-> (get-in @xcfg
              [:game :scene])
      (x/gcbyn+ :hud :status)
      (c/call-js! "setString" msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn write-score [value score]
  (let [{:keys [scene pmap]} (:game @xcfg)]
    (c/call-js! (x/gcbyn+ scene
                          :hud
                          (get pmap value))
                "setString" (str score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn value->symbol [value & [flip?]]
  (let [img (condp = value CV-X "x" CV-O "o" "z")]
    (x/sprite* (str "#" img (if flip? ".i.png" ".png")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare process-cell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-bot [first?]
  (fn_* (let [{:keys [depth grid turn bot]} (:game @xcfg)
              cell (if first?
                     (c/vtbl* bot :first-move)
                     (a/negamax bot depth grid turn))]
          (assert (c/nneg? cell) "bad bot move")
          (process-cell cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- click->cell [gpos pt]
  (if-some+
    [ret (some
           #(if (x/contains-pt? (nth gpos %) pt) [%])
           (range (n# gpos)))]
    (_1 ret)
    -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- switch-over []
  (let [{:keys [scene
                pmap
                turn
                bot-time] :as G} (:game @xcfg)
        next' (if (= turn CV-X) CV-O CV-X)
        {:keys [ptype pid]} (get G (get pmap next'))]
    (swap! xcfg
           #(assoc-in % [:game :turn] next'))
    (if (= P-BOT ptype)
      (c/call-js! scene
                  "scheduleOnce"
                  (run-bot false) bot-time))
    (write-status (str pid "'s turn"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-end []
  (let [{{:keys [scene]} :game :keys [start-scene]} @xcfg
        [h g] (x/gcbyn* scene :hud :arena)]
    (js/cc.eventManager.pauseTarget g true)
    (js/cc.eventManager.pauseTarget h true)
    (->> {:yes #(x/run-scene (start-scene))
          :msg (x/l10n "%playMore?")
          :cleanup (fn_0 (js/cc.eventManager.resumeTarget g true)
                         (js/cc.eventManager.resumeTarget h true))}
         (d/pop-dlg scene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- tie-game []
  (write-status (x/l10n "%tieGame"))
  (x/sfx-effect :game-tie)
  (on-end)
  (swap! xcfg
         #(assoc-in % [:game :running?] false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- won-game [value]
  (let [{:keys [scores pmap] :as G} (:game @xcfg)
        {:keys [pid]} (get G (get pmap value))
        s (get scores value)]
    (x/sfx-effect :game-end)
    (write-score value (+ 1 s))
    (write-status (x/l10n "%winGame" pid))
    (on-end)
    (swap! xcfg
           (fn_1 (update-in ____1
                            [:game]
                            #(-> (assoc % :running? false)
                                 (update-in [:scores value] inc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-game-state []
  (let [{:keys [turn grid
                pmap goals gpos]} (:game @xcfg)
        combo (some (fn [c]
                      (if (every? #(= % turn)
                                  (map #(nth grid %) c)) c)) goals)]
    (cond (some? combo)
          (won-game turn)
          (not-any? #(= CV-Z %) grid)
          (tie-game)
          :else
          (switch-over))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-arena [cell]
  (let [{:keys [scene turn
                pmap cells grid]} (:game @xcfg)
        [sp v] (nth cells cell)
        pk (get pmap turn)
        pt (x/pos?? sp)
        sp' (x/set!! (value->symbol turn) {:pos pt})]
    (x/sfx-effect pk)
    (x/remove! sp)
    (x/add-> (x/gcbyn scene :arena) sp')
    (swap! xcfg
           (fn [root]
             (aset grid cell turn)
             (update-in root [:game :cells cell] (fn [_] [sp' turn]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- XXXprocess-cell [cell]
  (let [{:keys [grid pmap turn ]} (:game @xcfg)]
    ;(x/debug* "cell=>>>>>> " cell)
    (when (and (c/nneg? cell)
               (= CV-Z (nth grid cell)))
      (update-arena cell)
      (check-game-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- process-cell [cell]
  (let [{:keys [evQ grid pmap turn]} (:game @xcfg)]
    ;(x/debug* "cell=>>>>>> " cell)
    (when (and (c/nneg? cell)
               (= CV-Z (nth grid cell))) (.push evQ cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-game [dt]
  (let [{:keys [running? evQ]} (:game @xcfg)]
    (when running?
      (when-some [cell (.shift evQ)]
        (update-arena cell) (check-game-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-click [topic msgTopic & msgs]
  (let [{:keys [gpos pmap
                turn running?] :as G} (:game @xcfg)]
    (when running?
      (let [{:keys [ptype]} (get G (get pmap turn))
            cell (click->cell gpos
                              (c/call-js! (_1 msgs)
                                          "getLocation"))]
        (when (and (not= P-BOT ptype)
                   (c/nneg? cell)) (process-cell cell))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn on-touch [topic msgTopic & msgs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

