;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.options

  (:require-macros [czlab.mcfud.afx.core :as ec :refer [do-with f#*]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y pos!
                                   popToRoot popScene sprite*]])

  (:require [czlab.mcfud.cc.ccsx
             :as cx :refer [bsize xcfg addItem l10n
                            vec2->cp cp->vec2 vrect
                            rect->box4 mid-rect
                            miLabel* miToggle* toggleSelect!
                            ttfLabel* miFontItem* miFontLabel*]]
            [czlab.mcfud.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onFirstMoveToggle

  ""
  [node]

  (let [n (ocall node "getSelectedIndex")
        {:keys [CC-X CC-O CC-Z]} (:game @xcfg)]
    (swap! xcfg
           #(assoc-in %
                      [:game :BEGIN-WITH]
                      (cond (= 0 n) CC-X
                            (= 1 n) CC-O :else CC-Z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onSoundToggle

  ""
  [node]

  (let [n (ocall node "getSelectedIndex")] (cx/setSfx! (zero? n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onP1Toggle

  ""
  [node]

  (let [n (ocall node "getSelectedIndex")
        {:keys [CC-X CC-O]} (:game @xcfg)]
    (swap! xcfg
           #(assoc-in %
                      [:game :P1-ICON]
                      (if (zero? n) CC-X CC-O)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- onGoBack

  ""
  [node]

  (popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onQuit

  ""
  [node]

  (popToRoot)
  (cx/run* (:startScene @xcfg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn optionsScene

  ""
  [& [options]]

  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [P1-ICON BEGIN-WITH CC-X]} (:game @xcfg)
          {:keys [quit?] :or {quit? true}} options
          bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          B (vrect)
          [cx cy :as cp] (mid-rect B)
          {:keys [top]} (rect->box4 B)]
      (pos! bg (vec2->cp cp))
      (addItem scene layer)
      (cx/addItem layer bg "bg" -1)
      (->> (cx/bmfLabel (l10n "%options")
                        (cx/gfnt :title)
                        {:pos (vec2 cx
                                    (* 0.8 top))
                         :color (js/cc.color "#F6B17F")})
           (cx/addItem layer))
      (let [t1 (miFontLabel* "Sound" 18)
            i1 (-> (miToggle* (miFontItem* "On" 26)
                              (miFontItem* "Off" 26) onSoundToggle)
                   (toggleSelect! (if (cx/sfxOn?) 0 1)))
            t2 (miFontLabel* "Player 1" 18)
            i2 (-> (miToggle* (miFontItem* "X" 26)
                              (miFontItem* "O" 26) onP1Toggle)
                   (toggleSelect! (if (= P1-ICON CC-X) 0 1)))
            t3 (miFontLabel* "First Move" 18)
            i3 (-> (miToggle* (miFontItem* "X" 26)
                              (miFontItem* "O" 26)
                              (miFontItem* "?" 26) onFirstMoveToggle)
                   (toggleSelect! (if (= BEGIN-WITH CC-X) 0 1)))
            quit (miLabel* (ttfLabel* "Quit" "Arial" 20) onQuit)
            back (miLabel* (ttfLabel* "Go back" "Arial" 20) onGoBack)
            menu (if-not quit?
                   (new js/cc.Menu t1 i1 t2 i2 t3 i3 back)
                   (new js/cc.Menu t1 i1 t2 i2 t3 i3 back quit))]
        (if-not quit?
          (ocall! menu "alignItemsInColumns" 2 2 2 1)
          (ocall! menu "alignItemsInColumns" 2 2 2 1 1))
        (addItem layer menu)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

