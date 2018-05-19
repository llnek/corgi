;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.dialog

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [each-indexed applyScalarOp
                    half* f#* n# _1 _2 do-with numStr]]
    [czlab.elmo.afx.ccsx
     :as cx :refer [oget-x oget-y oget-piccy
                    oget-bottom oget-right
                    sprite* attr* ccmenu?
                    not-native? native?
                    gcbyn gcbyt zeropt
                    newBBox newBBox4
                    oget-width oget-height
                    oget-left oget-top oget-id
                    snode? bbox? bbox4? ccnode? sprite?]])
  (:require [czlab.elmo.afx.core :as ec :refer [xmod raise! noopy]]
            [czlab.elmo.afx.ccsx :as cx]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- undoDlg "" []
  (js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onNO "" [finz no]
  (f#* (finz)
       (if (fn? no) (no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onOK "" [finz yes]
  (f#* (finz)
       (if (fn? yes) (yes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popDlgBg "" []
  (let [{:keys [width height]} (cx/vbox)
        pt {:x (half* (* width (- 1 0.6)))
            :y (half* (* height (- 1 0.6)))}
        cp (cx/centerPos)
        c (js/cc.color 123 123 123)
        dlg (new js/cc.LayerColor c (* 0.6 width) (* 0.6 height))]
    (cx/setXXX! dlg {:pos pt})
    dlg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popDlg* "" [d0 options]
  (let [{:keys [msg yes no cleanup]} options
        cp (cx/centerPos)
        dlg (new js/cc.Layer)
        mnu (new js/cc.Menu)
        finz #(do (cx/remove! d0)
                  (cx/remove! dlg) (if (fn? cleanup) (cleanup)))
        mnu (cx/tmenu [{:text msg :font (cx/gfnt :text) :cb noopy }
                       {:text "Cancel"
                        :font (cx/gfnt :text) :cb (onNO finz no) }
                       {:text "OK" :font (cx/gfnt :text) :cb (onOK finz yes) }]
                      {:color (js/cc.color 0 0 0)
                       :scale 0.3 :align? false})]
    (ocall! mnu "alignItemsInColumns" 1 2)
    (cx/addItem dlg mnu "mnu")
    dlg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn popDlg "" [par options]
  (let [d0 (popDlgBg )
        d1 (popDlg* d0 options)]
    (cx/addItem par d0 "dlg0" 999)
    (cx/addItem par d1 "dlg1" 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

