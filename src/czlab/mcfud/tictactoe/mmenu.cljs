;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.mmenu

  (:require-macros [czlab.mcfud.afx.core :as ec :refer [_1 _2 do-with f#*]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y pos!
                                   popToRoot pushScene sprite*]])

  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [l10n debug* bsize xcfg
                            vec2->cp cp->vec2
                            addItem rect->box4 vrect]]
            [czlab.mcfud.afx.core :as ec :refer [nichts?]]
            [czlab.mcfud.tictactoe.options :as opt]
            [czlab.mcfud.tictactoe.game :as ga]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- netScene

  ""
  [& xs]

  (debug* "net-scene called"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onoptions

  ""
  [& xs]

  (pushScene (opt/optionsScene {:quit? false})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onquit

  ""
  [& xs]

  (let [{:keys [startScene]} @xcfg]
    (popToRoot)
    (cx/run* (startScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onnetplay

  ""
  [& xs]

  (->> {:no (f#* (cx/run* nil))
        :yes (fn [ws p0 msg]
               (->> (merge {:ws ws
                            :pnum p0} msg)
                    (ga/gameScene (:1 msg)
                                  (:2 msg))
                    (cx/run*)))}
       (netScene)
       (cx/run*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onplayXXX

  ""
  [mode]

  (f#* (let [{:keys [CV-X CV-O
                     CC-X CC-O
                     CX CO P1-ICON]} (:game @xcfg)
             p2cat (if (= 1 mode) 2 1)
             syms
             (condp = P1-ICON
               CC-X [[CX 1 CV-X CC-X][CO p2cat CV-O CC-O]]
               CC-O [[CO 1 CV-O CC-O][CX p2cat CV-X CC-X]] nil)]
         (-> (ga/gameScene
               mode
               (concat (_1 syms) [(l10n "%p1")(l10n "%player1")])
               (concat (_2 syms)
                       (if (= 1 mode)
                         [(l10n "%cpu") (l10n "%computer")]
                         [(l10n "%p2") (l10n "%player2")])))
             (cx/run*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mmenuScene

  ""
  []

  (do-with [scene (new js/cc.Scene)]
    (let [bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          B (vrect)
          [cx cy :cp] B
          {:keys [top]} (rect->box4 B)]
      (pos! bg (vec2->cp cp))
      (addItem scene layer)
      (addItem layer bg "bg" -1)
      (->> (cx/bmfLabel (l10n "%mmenu")
                        (cx/gfnt :title)
                        {:pos (vec2 cx
                                    (* 0.8 top))
                         :color (js/cc.color "#F6B17F")})
           (addItem layer))
      ;const color= cc.color('#5E3178'),
      (->> (cx/gmenu [{:nnn "#player1.png" :cb (onplayXXX 1)}
                      {:nnn "#player2.png" :cb (onplayXXX 2)}
                      {:nnn "#online.png" :cb onnetplay}
                      {:nnn "#options.png" :cb onoptions}
                      {:nnn "#quit.png" :cb onquit}] {:pos cp})
           (addItem layer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

