;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.pong.gui

  (:require [czlab.mcfud.afx.core
             :as c :refer [_1 _2 fn_1 fn_* do-with]]
            [oops.core :as oc]
            [czlab.rygel.pong.core :as p]
            [czlab.mcfud.cc.ccsx :as x :refer [CV-X CV-O
                                               P-MAN P-BOT
                                               G-TWO G-ONE xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare options-scene splash-scene)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hlayer [layer R]
  (do-with [layer]
    (let [{:keys [scores pmap] :as G} (:game @xcfg)
          {:keys [top low lhs rhs]} (x/r->b4 R)
          [cx cy] (x/mid-rect* R)
          cx2 (/ cx 2)
          fcb (fn_* (x/push-scene (options-scene)))
          [kx ky] (x/pkeys pmap)
          s1 (x/bmf-label* ""
                           (x/gfnt :label)
                           {:pos (x/ccp* cx2 top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-LEFT})
          s2 (x/bmf-label* ""
                           (x/gfnt :label)
                           {:pos (x/ccp* (- rhs cx2) top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-RIGHT})]
      (x/add-> layer s1 (name kx))
      (x/add-> layer s2 (name ky))
      (p/write-score CV-X (scores CV-X))
      (p/write-score CV-O (scores CV-O))
      (-> (x/add-> layer (x/bmf-label*
                           "" (x/gfnt :text)) "status")
          (x/set!! {:pos (x/ccp* cx (/ cy 4))
                    :color "#ffffff" :scale .3}))
      (x/add-> layer
               (x/gmenu {:nnn "#icon_menu.png" :cb fcb}
                        {:region R :anchor x/ANCHOR-TOP}) "pause"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-scene [mode & more]
  (do-with [scene (x/scene*)]
    (let [[bl gl hud] [(x/layer*) (x/layer*) (x/layer*)]
          white (x/color* 255 255 255)
          R (x/vrect)
          {:keys [top lhs rhs low]} (x/r->b4 R)
          [width height] (x/r-> R)
          [cx cy] (x/mid-rect* R)
          rl (x/add-> gl (new js/cc.DrawNode) "border" -1)
          S {:walls {:w (x/ccr* lhs low 1 height)
                     :e (x/ccr* (- rhs 1) low 1 height)
                     :n (x/ccr* lhs (- top 1) width 1)
                     :s (x/ccr* lhs low width 1)}
             ;^ 4 invisible static walls
             :running? false
             :inited? false
             :scene scene
             :gmode mode
             :evQ #js []
             :scores {CV-X 0 CV-O 0}
             :player {:ptype P-MAN
                      :pid (x/l10n "%p1")
                      :pname (x/l10n "%player1")}
             :pother (if (not= mode G-ONE)
                       {:ptype P-MAN
                        :pid (x/l10n "%p2") :pname (x/l10n "%player2")}
                       {:ptype P-BOT
                        :pid (x/l10n "%cpu") :pname (x/l10n "%computer")})}]
      (swap! xcfg
             (fn_1 (update-in ____1
                              [:game] #(c/merge+ % S))))
      (x/center-image R
                      (x/add-> scene
                               bl "bg" -2)
                      (x/gimg :arena-bg))
      (x/add-> scene gl "arena" 1)
      (x/add-> scene hud "hud" 2)
      (hlayer hud R)
      (p/init)
      (x/hook-update scene #(p/step %1))
      (let [xxx (fn_* ((x/on-scene-enter scene gl))
                      (x/remove-all! rl)
                      (c/call-js! rl
                                  "drawRect"
                                  (x/ccp* lhs low)
                                  (x/ccp* rhs top) nil 12 white)
                      ;(c/call-js! rl "drawCircle" (x/ccp* cx cy) 64 0 100 false 8 white)
                      (c/call-js! rl
                                  "drawSegment"
                                  (x/ccp* cx low)
                                  (x/ccp* cx top) 4 white))]
        (x/attr* scene #js{:onEnter xxx}))
      (swap! xcfg #(assoc-in % [:game :running?] true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn options-scene [& [options]]
  (do-with [scene (x/scene*)]
    (let [fquit (fn_* (x/pop->root) (x/run-scene (splash-scene)))
          fsound (fn_* (x/sfx! (zero? (x/gsidx (_1 ____xs)))))
          fback (fn_* (x/pop-scene))
          fp1 (fn_*
                (let [n (x/gsidx (_1 ____xs))]
                  (swap! xcfg
                         #(-> (if (zero? n)
                                (-> (assoc-in % [:game :player :pvalue] CV-X)
                                    (assoc-in [:game :pother :pvalue] CV-O))
                                (-> (assoc-in % [:game :player :pvalue] CV-O)
                                    (assoc-in [:game :pother :pvalue] CV-X)))
                              (update-in [:game]
                                         (fn_1 (assoc ____1
                                                      :pmap
                                                      (if (zero? n)
                                                        {CV-X :player CV-O :pother}
                                                        {CV-O :player CV-X :pother}))))))))
          {:keys [quit?] :or {quit? true}} options
          {:keys [player]} (:game @xcfg)
          layer (x/add-> scene (x/layer*))
          R (x/vrect)
          {:keys [top]} (x/r->b4 R)
          [cx cy] (x/mid-rect* R)
          t1 (x/mifont-text* (x/l10n "%sound") 18)
          i1 (x/mitoggle* (x/mifont-item* (x/l10n "%on") 26)
                          (x/mifont-item* (x/l10n "%off") 26) fsound)
          t2 (x/mifont-text* (x/l10n "%player1") 18)
          i2 (x/mitoggle* (x/mifont-item* "Red" 26)
                          (x/mifont-item* "Blue" 26) fp1)
          quit (x/milabel* (x/ttf-text* (x/l10n "%quit") "Arial" 20) fquit)
          back (x/milabel* (x/ttf-text* (x/l10n "%back") "Arial" 20) fback)
          gmenu (x/add-> layer
                         (if-not quit?
                           (x/menu* t1 i1 t2 i2 back)
                           (x/menu* t1 i1 t2 i2 back quit)))]
      (x/center-image R layer (x/gimg :game-bg) "bg" -1)
      (x/add-> layer (x/bmf-label* (x/l10n "%options")
                                   (x/gfnt :title)
                                   {:color "#F6B17F"
                                    :pos (x/ccp* cx (* .8 top))}))
      (x/toggle-select! i1 (if (x/sfx?) 0 1))
      (x/toggle-select! i2 (if (= (:pvalue player) CV-X) 0 1))
      (if quit?
        (x/align-in-cols gmenu 2 2 1 1)
        (x/align-in-cols gmenu 2 2 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- net-scene [& xs])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn menu-scene []
  (do-with [scene (x/scene*)]
    (let [pms {:no (fn_* (x/run-scene nil))
               :yes (fn [ws p0 msg]
                      (x/run-scene
                        (game-scene (:1 msg)
                                    (:2 msg)
                                    (merge {:ws ws
                                            :pnum p0} msg))))}
          fnet (fn_* (x/run-scene (net-scene pms)))
          fquit (fn_* (x/pop->root)
                      (x/run-scene (:start-scene @xcfg)))
          fopt (fn_* (x/push-scene (options-scene)))
          fp1 (fn_* (x/run-scene (game-scene G-ONE)))
          fp2 (fn_* (x/run-scene (game-scene G-TWO)))
          layer (x/add-> scene (x/layer*))
          R (x/vrect)
          [cx cy] (x/mid-rect* R)
          {:keys [top]} (x/r->b4 R)]
      ;const color= cc.color('#5E3178'),
      (x/center-image R
                      layer
                      (x/gimg :game-bg) "bg" -1)
      (x/add-> layer
               (x/bmf-label* (x/l10n "%mmenu")
                             (x/gfnt :title)
                             {:color "#EDFF90"
                              :pos (x/ccp* cx (* .8 top))}))
      (x/center!! R
                  layer
                  (x/gmenu
                    [{:nnn "#player1.png" :cb fp1}
                     {:nnn "#player2.png" :cb fp2}
                     {:nnn "#online.png" :cb fnet}
                     {:nnn "#options.png" :cb fopt}
                     {:nnn "#quit.png" :cb fquit}])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splash-scene []
  (do-with [scene (x/scene*)]
    (let [fplay (fn_* (x/run-scene (menu-scene)))
          R (x/vrect)
          {:keys [top low lhs rhs]} (x/r->b4 R)
          [x y] (x/mid-rect* R)
          layer (x/add-> scene (x/layer*))]
      (x/center-image R
                      layer
                      (x/gimg :game-bg) "bg" -1)
      (x/pos! (x/add-> layer
                       (x/sprite* "#title.png")) x (* .8 top))
      (x/pos! (->> (x/gmenu {:cb fplay
                             :nnn "#play.png"})
                   (x/add-> layer)) x (* .1 top)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


