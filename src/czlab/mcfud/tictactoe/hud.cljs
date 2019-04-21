;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.mcfud.tictactoe.hud

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [half* _1 _2 f#* do-with x->str]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [pushScene
                                   sprite* oget-x oget-y gcbyn]])

  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [ANCHOR-TOP-RIGHT ANCHOR-TOP-LEFT
                            mid->rect rect->box4
                            gmenu audioIcon addItem bmfLabel
                            vrect game-scene xcfg ANCHOR-TOP]]
            [czlab.mcfud.afx.core :as ec]
            [czlab.mcfud.afx.math :refer [vec2]]
            [czlab.mcfud.tictactoe.options :as opt]
            [czlab.mcfud.tictactoe.misc :as mc]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onReplay

  ""
  [& xs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onPause

  ""
  [& xs]

  (pushScene (opt/optionsScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hudLayer

  ""
  [px py state score1' score2']

  (do-with [layer (new js/cc.Layer)]
    (let [{:keys [P1-ICON CC-X CC-O]} (:game @xcfg)
          {:keys [gpos] as S} @state
          c (js/cc.color "#5e3178")
          B (vrect)
          [cx cy :as cp] (mid-rect B)
          gend (_2 (ec/minBy #(_2 %) gpos))
          {:keys [top bottom]} (rect->box4 B)
          title (bmfLabel (str (get-in S [px :pid]) "/"
                               (get-in S [py :pid]))
                          (cx/gfnt :title)
                          {:pos (vec2 cx top)
                           :anchor ANCHOR-TOP
                           :color c
                           :scale 0.6})
          p1 (bmfLabel (str (get-in S [px :pid])
                            "[" P1-ICON "]")
                       (cx/gfnt :title)
                       {:pos (vec2 0 top)
                        :color (js/cc.color 255 255 255)
                        :scale 0.6
                        :anchor ANCHOR-TOP-LEFT})
          score1 (bmfLabel (x->str score1')
                           (cx/gfnt :label)
                           {:pos (vec2 0 top)
                            :color (js/cc.color 255 255 255)
                            :scale 0.6
                            :anchor ANCHOR-TOP-LEFT})
          p2 (bmfLabel (str "["
                            (if (= P1-ICON CC-X) CC-O CC-X) "]"
                            (get-in S [py :pid]))
                       (cx/gfnt :title)
                       {:pos (vec2 right top)
                        :color (js/cc.color 255 255 255)
                        :scale 0.6
                        :anchor ANCHOR-TOP-RIGHT})
          score2 (bmfLabel (x->str score2')
                           (cx/gfnt :label)
                           {:pos (vec2 right top)
                            :color (js/cc.color 255 255 255)
                            :scale 0.6
                            :anchor ANCHOR-TOP-RIGHT})
          status (bmfLabel ""
                           (cx/gfnt :text)
                           {:pos (vec2 cx (+ bottom
                                             (half* (- gend bottom))))
                            :color (js/cc.color 255 255 255)
                            :scale 0.3})
          audio (audioIcon "#sound_on.png"
                           "#sound_off.png"
                           {:anchor cx/ANCHOR-BOTTOM-LEFT})
          pmenu (gmenu [{:nnn "#icon_menu.png" :cb onPause}]
                       {:region B :anchor cx/ANCHOR-TOP})
          replay (gmenu [{:nnn "#icon_replay.png" :cb onReplay}]
                        {:anchor cx/ANCHOR-BOTTOM :show? false})]
      (cx/debug* "hud called")
      ;(cx/pegToAnchor audio cx/*anchor-bottom-left* B)
      ;(cx/addItem layer title "title")
      (addItem layer p1 "p1")
      (addItem layer p2 "p2")
      (addItem layer status "status")
      ;(cx/addItem layer audio "audio")
      (addItem layer pmenu "pause"))))
      ;(cx/addItem layer replay "replay"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid writeStatus

  ""
  [msg]

  (-> (gcbyn @game-scene "hud")
      (gcbyn "status")
      (ocall! "setString" msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid writeScore

  ""
  [who score]

  (-> (gcbyn @game-scene "hud")
      (gcbyn (str "p" (name who)))
      (ocall! "setString" (x->str score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid enableReplay

  ""
  [state]

  (-> (gcbyn @game-scene "hud")
      (gcbyn "replay")
      (ocall! "setVisible" true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


