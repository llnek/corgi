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
                    :as ec :refer [_1 _2 f#* do-with each-indexed]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [sprite* oget-top oget-x oget-y
                                   oget-bottom oget-right oget-left]])
  (:require [czlab.elmo.afx.ccsx
             :as cx :refer [*anchor-top-right* *anchor-top-left*
                            *xcfg* *anchor-top*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.misc :as mc]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hudLayer "" [px py]
  (do-with [layer (new js/cc.Layer)]
    (let [state (atom {:scores {:1 0 :2 0}
                       (_1 px) {:id (_2 px) :name (last px)}
                       (_1 py) {:id (_2 py) :name (last py)}})
          cp (cx/centerPos)
          wb (cx/vbox4)
          c (js/cc.color "#5e3178")
          title (cx/bmfLabel (str (_2 px) "/" (_2 py))
                             (cx/gfnt :title)
                             {:pos (js/cc.p (oget-x cp) (oget-top wb))
                              :anchor *anchor-top*
                              :color c
                              :scale 0.6})
          score1 (cx/bmfLabel "0"
                              (cx/gfnt :label)
                              {:pos (js/cc.p 0 (oget-top wb))
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-left* })
          score2 (cx/bmfLabel "0"
                              (cx/gfnt :label)
                              {:pos (js/cc.p (oget-right wb) (oget-top wb))
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-right* })
          status (cx/bmfLabel ""
                              (cx/gfnt :text)
                              {:pos (js/cc.p (oget-x cp) (oget-bottom wb))
                               :color (js/cc.color 255 255 255)
                               :scale 0.3})
          result (cx/bmfLabel ""
                              (cx/gfnt :text)
                              {:pos (js/cc.p (oget-x cp) (oget-bottom wb))
                               :color (js/cc.color 255 255 255)
                               :show? false
                               :scale 0.3})]
      (cx/addItem layer title)
      (cx/addItem layer score1)
      (cx/addItem layer score2)
      (cx/addItem layer status)
      (cx/addItem layer result)
      (oset! layer "!____state" state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


