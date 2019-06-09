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

  (:require [czlab.mcfud.afx.core
             :as c :refer [raise! fn_* n# _1 _2 do-with]]
            [czlab.mcfud.cc.ccsx
             :as x :refer [add-> r-> p-> not-native? native?]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- undo-dlg [] (x/pop-scene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-NO
  "Action to do when cancel or no is selected."
  [finz no]
  (fn_* (finz)
        (if (fn? no) (no))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-OK
  "Action to do when yes or ok is selected."
  [finz yes]
  (fn_* (finz)
        (if (fn? yes) (yes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-dlg-bg
  "Show dialog box background."
  []
  (let [[width height] (r-> (x/vrect))
        x (* 0.5 (* width (- 1 0.6)))
        y (* 0.5 (* height (- 1 0.6)))]
    (x/set!! (new js/cc.LayerColor
                  (x/color* 123 123 123)
                  (* 0.6 width) (* 0.6 height))
             {:pos (js/cc.p x y)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-dlg*
  "Implement dialog box"
  [d0 options]
  (do-with [dlg (x/layer*)]
    (let [{:keys [msg yes no cleanup]} options
          fnt (x/gres+ :fonts :text)
          finz #(do (x/remove! d0)
                    (x/remove! dlg)
                    (if (fn? cleanup) (cleanup)))]
      (-> (add-> dlg
                 (x/tmenu [{:text msg :font fnt :cb c/fn-nil}
                           {:text "OK" :font fnt :cb (on-OK finz yes)}
                           {:text "Cancel" :font fnt :cb (on-NO finz no)}]
                          {:align? false :scale .3 :color (x/color* 0 0 0)}))
          (ocall! "alignItemsInColumns" 1 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pop-dlg
  "Show dialog box."
  [par options]
  (let [d0 (pop-dlg-bg)
        d1 (pop-dlg* d0 options)]
    (add-> par d0 "dlg0" 999)
    (add-> par d1 "dlg1" 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

