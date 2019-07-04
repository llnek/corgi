;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.physics.gui

  (:require [czlab.mcfud.cc.ccsx :as x :refer [xcfg]]
            [czlab.rygel.physics.impl :as impl]
            [czlab.mcfud.afx.core :as c :refer [_1 _2 fn_0 fn_1 do-with]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-scene "" []
  (do-with [scene (x/scene*)]
    (let [gl (x/add-> scene (x/layer*) "arena" 1)]
      (swap! xcfg
             (fn_1 (update-in ____1
                              [:game]
                              #(assoc %
                                      :scene scene))))
      (impl/init)
      (x/hook-update scene #(impl/run-game %))
      (swap! xcfg
             #(assoc-in % [:game :running?] true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splash-scene "" []
  (do-with [scene (x/scene*)]
    (let [R (x/vrect)
          [cx cy] (x/mid-rect* R)
          {:keys [top]} (x/r->b4 R)
          layer (x/add-> scene (x/layer*))]
      (x/add-> layer
               (x/bmf-label* "Physics!"
                             (x/gfnt :title)
                             {:pos (x/ccp* cx (* .8 top))
                              :color "#EDFF90"}))
      (x/add-> layer
               (x/gmenu {:nnn "#play.png"
                         :cb (fn_0 (x/run-scene (game-scene)))}
                        {:pos (x/ccp* cx (* .1 top))})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


