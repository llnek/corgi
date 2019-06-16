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

  (:require [czlab.mcfud.cc.ccsx :as x]
            [oops.core :as oc]
            [czlab.mcfud.afx.core
             :as c :refer [raise! fn_* n# _1 _2 do-with]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-dlg-bg
  "Show dialog box background."
  [color]
  (do-with [layer (x/clayer* color)]
    (let [[width height] (x/vrect*)]
      (oc/ocall! layer
                 "setContentSize"
                 (* .6 width) (* .6 height))
      (x/pos! layer
              (/ (* width .4) 2) (/ (* height .4) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-dlg*
  "Implement dialog box"
  [d0 options]
  (do-with [dlg (x/layer*)]
    (let [{:keys [msg yes no cleanup]} options
          fnt (x/gfnt :text)
          finz #(do (x/remove! d0 dlg)
                    (if (fn? cleanup) (cleanup)))
          ok (fn_* (finz) (if (fn? yes) (yes)))
          no (fn_* (finz) (if (fn? no) (no)))
          t (x/tmenu [{:text msg :font fnt :cb c/fn-nil}
                      {:text (x/l10n "%ok") :font fnt :cb ok}
                      {:text (x/l10n "%cancel") :font fnt :cb no}]
                     {:align? false :scale .3 :color "#000000"})]
      (x/align-in-cols (x/add-> dlg t) 1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pop-dlg
  "Show dialog box."
  [par options]
  (let [d0 (pop-dlg-bg "#7b7b7b")]
    (x/add-> par d0 "dlg0" 999)
    (x/add-> par (pop-dlg* d0 options) "dlg1" 1000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

