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
     :as ec :refer [do->true each-indexed
                    nneg? f#* n# _1 _2 do-with]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-bottom oget-right gcbyn
                    oget-x oget-y oget-left oget-top]])
  (:require
    [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
    [czlab.elmo.afx.ccsx :as cx :refer [*game-scene* *xcfg*]]
    [czlab.elmo.tictactoe.misc :as mc]
    [czlab.elmo.tictactoe.hud :as hud]
    [czlab.elmo.afx.ecs :as ecs]
    [czlab.elmo.afx.ebus :as ebus]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- click->cell "" [gridPos x y]
  (let [ret (some
              (fn [i]
                (let [r (nth gridPos i)]
                  (if (cx/contains? r x y) [i])))
              (range (n# gridPos)))]
    (if (not-empty ret) (_1 ret) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- syncStatus "" [state]
  (let [{:keys [whosTurn]} @state
        user (get @state whosTurn)
        id (get user :pid)]
    (hud/writeStatus (str id "'s move..."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- switchOver "" [state]
  (let [{:keys [whosTurn]} @state
        {:keys [CX CO]} (:game @*xcfg*)]
    (swap! state
           #(assoc %
                   :whosTurn
                   (if (= whosTurn CX)
                     CO
                     (if (= whosTurn CO) CX nil))))
    (syncStatus state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- tieGame "" [state]
  (hud/writeStatus "It's a draw!")
  (cx/sfxPlayEffect :game-tie)
  (swap! state #(assoc % :running? false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- wonGame "" [state who player]
  (let [s (get-in @state [:scores who])]
    (cx/sfxPlayEffect :game-end)
    (hud/writeScore who (inc s))
    (hud/writeStatus (str (get player :pid) " wins!"))
    (swap! state #(-> (assoc % :running? false)
                      (update-in [:scores who] inc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkGameState "" [state player]
  (let [{:keys [gspace whosTurn grid]} @state
        {:keys [CV-Z]} (:game @*xcfg*)
        value (get player :pvalue)
        combo (some (fn [c]
                      (if (every? #(= % value)
                                  (map #(nth grid %) c)) c nil)) gspace)]
    (cond
      (some? combo)
      (wonGame state whosTurn player)
      (not-any? #(= CV-Z %) grid)
      (tieGame state)
      :else
      (switchOver state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateArena "" [state player cell]
  (let [{:keys [whosTurn cells grid]} @state
        {:keys [CX CO]} (:game @*xcfg*)
        view (gcbyn @*game-scene* "arena")
        value (get player :pvalue)
        [sp pt v] (nth cells cell)
        sp' (mc/value->symbol value false)]
    (cx/sfxPlayEffect whosTurn)
    (if (some? sp) (cx/remove! sp))
    (cx/setXXX! sp' {:pos pt})
    (cx/addItem view sp')
    (swap! state
           (fn [root]
             (aset grid cell value)
             (update-in root [:cells cell] (fn [_] [sp' pt value]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onClick "" [state topic msgTopic & msgs]
  (let [{:keys [gmode gpos grid whoAmI whosTurn running?]} @state]
    (when running?
      (let [e (ocall (_1 msgs) "getLocation")
            cell (click->cell gpos (oget-x e) (oget-y e))
            {:keys [CV-Z]} (:game @*xcfg*)
            player (get @state whosTurn)
            cat (get player :ptype)]
        (cx/info* "cell=>>>>>> " cell)
        (when (and (number? cat)
                   (not= 2 cat)
                   (nneg? cell)
                   (= CV-Z (nth grid cell)))
          (updateArena state player cell)
          (checkGameState state player))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onTouch "" [state topic msgTopic & msgs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn motion "" [ecs dt]
  (let [scene (deref cx/*game-scene*)
        state (oget scene "?gstate")
        {:keys [whoAmI whosTurn evQ]} @state]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init "" [state]
  (let [{:keys [gmode gpos ebus ecs evQ]} @state
        {:keys [CX CO]} (:game @*xcfg*)
        cb (fn [& xs] (.push evQ xs))]
    (cx/info* "impl.init called")
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus
                 "mouse.up"
                 (fn [& xs] (apply onClick (concat [state] xs)))))
    (if (cx/onTouchOne ebus)
      (ebus/sub+ ebus
                 "touch.one.end"
                 (fn [& xs] (apply onTouch (concat [state] xs)))))

    ;always player 1 for mode 1
    (if (= 1 gmode)
      (swap! state #(assoc % :whoAmI CX)))
    ;unless online, randomly choose who starts first
    (case gmode
      (1 2)
      (swap! state #(assoc %
                           :whosTurn
                           (if (pos? (ec/randSign)) CX CO)))
      nil)

    (syncStatus state)
    (ecs/addSystem ecs motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




