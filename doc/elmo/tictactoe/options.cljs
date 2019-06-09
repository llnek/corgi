;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.tictactoe.options

  (:require-macros [czlab.elmo.afx.core :as ec :refer [do-with f#*]]
                   [czlab.elmo.cc.ccsx
                    :as cx :refer [oget-height oget-width
                                   oget-x oget-y
                                   oget-top sprite* ]])
  (:require [czlab.elmo.cc.ccsx :as cx :refer [bsize *xcfg*]]
            [czlab.elmo.afx.core :as ec :refer [nichts?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onFirstMoveToggle "" [node]
  (let [n (ocall node "getSelectedIndex")
        {:keys [CC-X CC-O CC-Z]} (:game @*xcfg*)]
    (swap! *xcfg*
           #(assoc-in % [:game :BEGIN-WITH]
                      (cond
                        (= 0 n) CC-X
                        (= 1 n) CC-O
                        :else CC-Z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onSoundToggle "" [node]
  (let [n (ocall node "getSelectedIndex")] (cx/setSfx! (zero? n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onP1Toggle "" [node]
  (let [n (ocall node "getSelectedIndex")
        {:keys [CC-X CC-O]} (:game @*xcfg*)]
    (swap! *xcfg*
           #(assoc-in % [:game :P1-ICON]
                      (if (zero? n) CC-X CC-O)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onGoBack "" [node]
  (js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onQuit "" [node]
  (let [{:keys [startScene]} @*xcfg*]
    (js/cc.director.popToRootScene)
    (cx/run* (startScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn optionsScene "" [& [options]]
  (do-with [scene (new js/cc.Scene)]
    (let [{:keys [P1-ICON BEGIN-WITH CC-X]} (:game @*xcfg*)
          {:keys [quit?] :or {quit? true}} options
          bg (sprite* (cx/gimg :game-bg))
          layer (new js/cc.Layer)
          _ (cx/addItem scene layer)
          {:keys [top] :as B} (cx/vbox4)
          cp (cx/vbox4MID B)
          tt (cx/bmfLabel
               (cx/l10n "%options")
               (cx/gfnt :title)
               {:pos {:x (:x cp)
                      :y (* 0.8 top)}
                :color (js/cc.color "#F6B17F")})]
      (cx/setXXX! bg {:pos cp})
      (cx/addItem layer bg "bg" -1)
      (cx/addItem layer tt)
      (let [t1  (cx/miFontLabel* "Sound" 18)
            i1 (new js/cc.MenuItemToggle
                       (cx/miFontItem* "On" 26) (cx/miFontItem* "Off" 26) onSoundToggle nil)
            _ (ocall! i1 "setSelectedIndex" (if (cx/sfxOn?) 0 1))

            t2  (cx/miFontLabel* "Player 1" 18)
            i2 (new js/cc.MenuItemToggle
                       (cx/miFontItem* "X" 26) (cx/miFontItem* "O" 26) onP1Toggle nil)
            _ (ocall! i2 "setSelectedIndex" (if (= P1-ICON CC-X) 0 1))

            t3  (cx/miFontLabel* "First Move" 18)
            i3 (new js/cc.MenuItemToggle
                       (cx/miFontItem* "X" 26)
                       (cx/miFontItem* "O" 26)
                       (cx/miFontItem* "?" 26)
                       onFirstMoveToggle nil)
            _ (ocall! i3 "setSelectedIndex" (if (= BEGIN-WITH CC-X) 0 1))

            back (new js/cc.MenuItemLabel
                      (new js/cc.LabelTTF  "Go back" "Arial" 20) onGoBack nil)

            quit (new js/cc.MenuItemLabel
                      (new js/cc.LabelTTF  "Quit" "Arial" 20) onQuit nil)
            menu (if-not quit?
                   (new js/cc.Menu t1 i1 t2 i2 t3 i3 back)
                   (new js/cc.Menu t1 i1 t2 i2 t3 i3 back quit))]
        (if-not quit?
          (ocall! menu "alignItemsInColumns" 2 2 2 1)
          (ocall! menu "alignItemsInColumns" 2 2 2 1 1))
        (cx/addItem layer menu)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

