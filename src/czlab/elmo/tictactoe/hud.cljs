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
                    :as ec :refer [_1 _2 f#* do-with each-indexed numStr]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [sprite* oget-top oget-x oget-y
                                   gcbyn
                                   oget-bottom oget-right oget-left]])
  (:require [czlab.elmo.afx.ccsx
             :as cx :refer [*anchor-top-right* *anchor-top-left*
                            *game-scene* *xcfg* *anchor-top*]]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.misc :as mc]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hudLayer "" [px py state score1' score2']
  (do-with [layer (new js/cc.Layer)]
    (let [cp (cx/centerPos)
          wb (cx/vbox4)
          c (js/cc.color "#5e3178")
          title (cx/bmfLabel (str (get-in @state [px :pid]) "/"
                                  (get-in @state [py :pid]))
                             (cx/gfnt :title)
                             {:pos {:x (:x cp) :y (:top wb)}
                              :anchor *anchor-top*
                              :color c
                              :scale 0.6})
          score1 (cx/bmfLabel (numStr score1')
                              (cx/gfnt :label)
                              {:pos {:x 0 :y (:top wb)}
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-left* })
          score2 (cx/bmfLabel (numStr score2')
                              (cx/gfnt :label)
                              {:pos {:x (:right wb) :y (:top wb)}
                               :color (js/cc.color 255 255 255)
                               :scale 0.6
                               :anchor *anchor-top-right* })
          status (cx/bmfLabel ""
                              (cx/gfnt :text)
                              {:pos {:x (:x cp) :y (:bottom wb)}
                               :color (js/cc.color 255 255 255)
                               :scale 0.3})
          result (cx/bmfLabel ""
                              (cx/gfnt :text)
                              {:pos {:x (:x cp) :y (:bottom wb)}
                               :color (js/cc.color 255 255 255)
                               :show? false
                               :scale 0.3})]
      (cx/info* "hud called")
      (cx/addItem layer title "title")
      (cx/addItem layer score1 "p1")
      (cx/addItem layer score2 "p2")
      (cx/addItem layer status "status")
      (cx/addItem layer result "result"))))

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
;;EOF


