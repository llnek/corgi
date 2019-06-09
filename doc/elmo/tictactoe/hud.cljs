;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.hud

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [half* _1 _2 f#* do-with numStr]]
                   [czlab.elmo.cc.ccsx
                    :as cx :refer [sprite* oget-top oget-x oget-y
                                   gcbyn
                                   oget-bottom oget-right oget-left]])
  (:require [czlab.elmo.cc.ccsx
             :as cx :refer [*anchor-top-right* *anchor-top-left*
                            *game-scene* *xcfg* *anchor-top*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.options :as opt]
            [czlab.elmo.tictactoe.misc :as mc]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onReplay "" [& xs])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onPause "" [& xs]
  (js/cc.director.pushScene (opt/optionsScene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hudLayer "" [px py state score1' score2']
  (do-with [layer (new js/cc.Layer)]
    (let [{:keys [P1-ICON CC-X CC-O]} (:game @*xcfg*)
          adon? (get-in @*xcfg* [:AD :on?])
          B (if adon? (cx/ebox4) (cx/vbox4))
          {:keys [gpos]} @state
          gend (:bottom (ec/minBy #(get % :bottom) gpos))
          cp (cx/vbox4MID B)
          {:keys [bottom] :as wb} B
          c (js/cc.color "#5e3178")
          title (cx/bmfLabel (str (get-in @state [px :pid]) "/"
                                  (get-in @state [py :pid]))
                             (cx/gfnt :title)
                             {:pos {:x (:x cp) :y (:top wb)}
                              :anchor *anchor-top*
                              :color c
                              :scale 0.6})
          p1 (cx/bmfLabel (str (get-in @state
                                       [px :pid])
                               "[" P1-ICON "]")
                          (cx/gfnt :title)
                          {:pos {:x 0 :y (:top wb)}
                           :color (js/cc.color 255 255 255)
                           :scale 0.6
                           :anchor *anchor-top-left* })
          score1 (cx/bmfLabel (numStr score1')
                              (cx/gfnt :label)
                              {:pos {:x 0 :y (:top wb)}
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-left* })
          p2 (cx/bmfLabel (str "["  (if (= P1-ICON CC-X) CC-O CC-X) "]"
                               (get-in @state [py :pid]))
                          (cx/gfnt :title)
                          {:pos {:x (:right wb) :y (:top wb)}
                           :color (js/cc.color 255 255 255)
                           :scale 0.6
                           :anchor *anchor-top-right* })
          score2 (cx/bmfLabel (numStr score2')
                              (cx/gfnt :label)
                              {:pos {:x (:right wb) :y (:top wb)}
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-right* })
          status (cx/bmfLabel ""
                              (cx/gfnt :text)
                              {:pos {:x (:x cp) :y (+ bottom (half* (- gend bottom)))}
                               :color (js/cc.color 255 255 255)
                               :scale 0.3})
          audio (cx/audioIcon "#sound_on.png"
                              "#sound_off.png"
                              {:anchor cx/*anchor-bottom-left*})

          pmenu (cx/gmenu [{:nnn "#icon_menu.png" :cb onPause}]
                          {:region B :anchor cx/*anchor-top*})

          replay (cx/gmenu [{:nnn "#icon_replay.png" :cb onReplay}]
                           {:anchor cx/*anchor-bottom* :show? false})]
      (cx/info* "hud called")
      ;(cx/pegToAnchor audio cx/*anchor-bottom-left* B)
      ;(cx/addItem layer title "title")
      (cx/addItem layer p1 "p1")
      (cx/addItem layer p2 "p2")
      (cx/addItem layer status "status")
      ;(cx/addItem layer audio "audio")
      (cx/addItem layer pmenu "pause"))))
      ;(cx/addItem layer replay "replay"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn writeStatus "" [msg]
  (let [hud (gcbyn @*game-scene* "hud")
        status (gcbyn hud "status")]
    (ocall! status "setString" msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn writeScore "" [who score]
  (let [hud (gcbyn @*game-scene* "hud")
        s (str "p" (name who))
        obj (gcbyn hud s)]
    (ocall! obj "setString" (numStr score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn enableReplay "" [state]
  (let [hud (gcbyn @*game-scene* "hud")
        r (gcbyn hud "replay")]
    (ocall! r "setVisible" true)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


