;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.cc.dialog

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid- defvoid each*
                                   half* f#* n# _1 _2 do-with]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-x oget-y oget-piccy pos!
                                   sprite* attr* ccmenu?
                                   not-native? native?
                                   gcbyn gcbyt oget-id
                                   oget-width oget-height sprite?]])

  (:require [czlab.mcfud.afx.core :as ec :refer [nil-fn raise! x->str]]
            [czlab.mcfud.cc.ccsx :as cx :refer [snode? ccnode? add->]]
            [czlab.mcfud.afx.math :refer [vec2]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- undoDlg

  "Close the dialog window."
  []

  (js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onNO

  "Action to do when cancel or no is selected."
  [finz no]

  (f#* (finz)
       (if (fn? no) (no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onOK

  "Action to do when yes or ok is selected."
  [finz yes]

  (f#* (finz)
       (if (fn? yes) (yes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popDlgBg

  "Show dialog box background."
  []

  (let [{:keys [wide tall]} (cx/vrect)
        x (* 0.5 (* wide (- 1 0.6)))
        y (* 0.5 (* tall (- 1 0.6)))]
    (-> (new js/cc.LayerColor (js/cc.color 123 123 123)
                              (* 0.6 wide) (* 0.6 tall))
        (pos! (vec2 x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popDlg*

  "Implement dialog box"
  [d0 options]

  (do-with [dlg (new js/cc.Layer)]
           (let [{:keys [msg yes no cleanup]} options
                 finz #(do (cx/remove! d0)
                           (cx/remove! dlg)
                           (if (fn? cleanup) (cleanup)))
                 fnt (cx/gfnt :text)
                 mnu (cx/tmenu [{:text msg :font fnt :cb nil-fn}
                                {:text "OK" :font fnt :cb (onOK finz yes)}
                                {:text "Cancel" :font fnt :cb (onNO finz no)}]
                               {:align? false
                                :scale 0.3
                                :color (js/cc.color 0 0 0)})]
             (ocall! mnu "alignItemsInColumns" 1 2)
             (add-> dlg mnu "menu"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn popDlg

  "Show dialog box."
  [par options]

  (let [d0 (popDlgBg )
        d1 (popDlg* d0 options)]
    (add-> par d0 "dlg0" 999)
    (add-> par d1 "dlg1" 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

