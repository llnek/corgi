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
(defn- onNO "" [& xs]
  (js/cc.director.popScene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- onOK "" [& xs]
  (js/alert "xxx"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn popDlg "" [msg']
  (let [{:keys [width height]} (cx/vbox)
        cp (cx/centerPos)
        dlg (new js/cc.Layer)
        _ (cx/setXXX! dlg {:pos cp})
        msg (cx/bmfLabel msg'
                         (cx/gfnt :text)
                         {:pos cp})
        mnu (cx/gmenu [{:nnn "#ok.png" :cb onOK}
                       {:nnn "#cancel.png" :cb onNO}]
                      {:anchor cx/*anchor-bottom* :flat? true})]
    (cx/pegToAnchor mnu cx/*anchor-bottom*)
    (cx/addItem dlg msg)
    (cx/addItem dlg mnu) dlg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

