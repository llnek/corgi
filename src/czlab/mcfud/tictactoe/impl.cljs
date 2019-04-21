;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.impl

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid- defvoid do->true
                                   nneg? f#* n# _1 _2 do-with]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [pos! gcbyn oget-x oget-y]])
  (:require [czlab.mcfud.afx.core :as ec :refer [raise! nil-fn]]
            [czlab.mcfud.cc.ccsx
             :as cx :refer [ptInRect?
                            game-arena game-scene xcfg]]
            [czlab.mcfud.cc.dialog :as dlg]
            [czlab.mcfud.tictactoe.board :as bot]
            [czlab.mcfud.tictactoe.misc :as mc]
            [czlab.mcfud.tictactoe.hud :as hud]
            [czlab.mcfud.afx.algos :as ag]
            [czlab.mcfud.afx.ecs :as ecs]
            [czlab.mcfud.afx.ebus :as ebus]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare processCell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- runAI

  ""
  [state first?]

  (cx/debug* "runAI === " first?)
  (f#* (let [{:keys [CX CO CV-X CV-O]} (:game @xcfg)
             {:keys [bot grid whosTurn]} @state
             cur (if (= whosTurn CX) CV-X CV-O)
             cell (if first?
                    ((:firstMove bot))
                    (ag/evalNegaMax bot 10 grid cur))]
         (when (neg? cell) (raise! "Ooops: " cell))
         (processCell state cell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- click->cell

  ""
  [gridPos pt]

  (let [ret (some (fn [i]
                    (let [r (nth gridPos i)]
                      (if (ptInRect? pt r) [i])))
                  (range (n# gridPos)))] (if (not-empty ret) (_1 ret) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- startMsg

  ""
  [state]

  (let [{:keys [whosTurn] :as S} @state
        n (get (get S whosTurn) :pname)]
    (hud/writeStatus (str n " starts."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- syncStatus

  ""
  [state]

  (hud/writeStatus ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- switchOver

  ""
  [state]

  (let [{:keys [whosTurn]} @state
        {:keys [BOT-THINK-TIME CX CO]} (:game @xcfg)]
    (swap! state #(assoc %
                         :whosTurn
                         (if (= whosTurn CX)
                           CO
                           (if (= whosTurn CO) CX nil))))
    ;if bot, run it
    (let [S @state]
      (if (= 2 (get (get S (get S :whosTurn)) :ptype))
        (ocall! @game-scene
                "scheduleOnce"
                (runAI state false) BOT-THINK-TIME)))
    (syncStatus state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- XonEnd

  ""
  [state]

  (hud/enableReplay state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onEnd

  ""
  [state]

  (let [{:keys [startScene]} @xcfg
        scene @game-scene
        arena @game-arena
        hud (gcbyn scene "hud")]
    (js/cc.eventManager.pauseTarget arena true)
    (js/cc.eventManager.pauseTarget hud true)
    (->> {:msg "Play Again?"
          :yes #(cx/run* (startScene))
          :cleanup #(do (js/cc.eventManager.resumeTarget arena true)
                        (js/cc.eventManager.resumeTarget hud true))}
         (dlg/popDlg scene ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- tieGame

  ""
  [state]

  (hud/writeStatus "It's a draw!")
  (cx/sfxPlayEffect :game-tie)
  (onEnd state)
  (swap! state #(assoc % :running? false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- wonGame

  ""
  [state who player]

  (let [s (get-in @state [:scores who])]
    (cx/sfxPlayEffect :game-end)
    (hud/writeScore who (+ 1 s))
    (hud/writeStatus (str (get player :pid) " wins!"))
    (onEnd state)
    (swap! state #(-> (assoc % :running? false)
                      (update-in [:scores who] inc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- checkGameState

  ""
  [state player]

  (let [{:keys [gspace whosTurn grid]} @state
        {:keys [CV-Z]} (:game @xcfg)
        value (get player :pvalue)
        combo (some (fn [c]
                      (if (every? #(= % value)
                                  (map #(nth grid %) c)) c nil)) gspace)]
    (cond (some? combo)
          (wonGame state whosTurn player)
          (not-any? #(= CV-Z %) grid)
          (tieGame state)
          :else
          (switchOver state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- updateArena

  ""
  [state player cell]

  (let [{:keys [whosTurn cells grid]} @state
        {:keys [CX CO]} (:game @xcfg)
        view (gcbyn @game-scene "arena")
        value (get player :pvalue)
        [sp pt v] (nth cells cell)
        sp' (mc/value->symbol value false)]
    (cx/sfxPlayEffect whosTurn)
    (if (some? sp) (cx/remove! sp))
    (pos! sp' (vec2->cp pt))
    (addItem view sp')
    (swap! state (fn [root]
                   (aset grid cell value)
                   (update-in root
                              [:cells cell]
                              (fn [_] [sp' pt value]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- processCell

  ""
  [state cell]

  (let [{:keys [grid whosTurn] :as S} @state
        {:keys [CV-Z]} (:game @xcfg)
        player (get S whosTurn)]
    (cx/debug* "cell=>>>>>> " cell)
    (when (and (nneg? cell)
               (= CV-Z (nth grid cell)))
      (updateArena state player cell)
      (checkGameState state player))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onClick

  ""
  [state topic msgTopic & msgs]

  (let [{:keys [gpos whosTurn running?] :as S} @state]
    (when running?
      (let [player (get S whosTurn)
            cat (get player :ptype)
            e (ocall (_1 msgs) "getLocation")
            cell (click->cell (cp->vec2 e) gpos)]
        (when (and (number? cat)
                   (not= 2 cat)
                   (nneg? cell))
          (processCell state cell))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onTouch

  ""
  [state topic msgTopic & msgs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn motion

  ""
  [ecs dt]

  (let [scene (deref cx/game-scene)
        state (oget scene "?gstate")
        {:keys [whoAmI whosTurn evQ]} @state]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init

  ""
  [state]

  (let [{:keys [gspace gmode gpos ebus ecs evQ]} @state
        {:keys [BOT-THINK-TIME GRID-SIZE
                CV-Z CV-X CV-O CX CO
                CC-X CC-O CC-Z BEGIN-WITH]} (:game @xcfg)
        cb (fn [& xs] (.push evQ xs))]
    (cx/debug* "impl.init called")
    (if (cx/onMouse ebus)
      (ebus/sub+ ebus
                 "mouse.up"
                 (fn [& xs] (apply onClick (concat [state] xs)))))
    (if (cx/onTouchOne ebus)
      (ebus/sub+ ebus
                 "touch.one.end"
                 (fn [& xs] (apply onTouch (concat [state] xs)))))

    ;unless online choose who starts first
    ;who starts?
    (when (not= GAME-NET gmode)
      (swap! state #(assoc %
                           :whosTurn
                           (condp = BEGIN-WITH
                             CC-X CX
                             CC-O CO
                             (if (pos? (ec/randSign)) CX CO)))))

    ;always player 1 for mode 1, and create the bot
    (when (= GAME-ONE gmode)
      (swap! state #(assoc %
                           :whoAmI CX
                           :bot (bot/TTTBoard GRID-SIZE CV-Z CV-X CV-O gspace))))

    ;if bot, run it
    (let [S @state]
      (if (= GAME-TWO (get (get S (get S :whosTurn)) :ptype))
        (ocall! @game-scene
                "scheduleOnce"
                (runAI state true) BOT-THINK-TIME)))

    (cx/debug* "game cfg= " (ec/clj->json (:game @xcfg)))

    (startMsg state)
    (ecs/addSystem ecs motion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




