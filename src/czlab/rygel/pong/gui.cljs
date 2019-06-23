;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.pong.splash

  (:require [czlab.mcfud.afx.core
             :as c :refer [do-with nichts?]]
            [oops.core :as oc]
            [czlab.mcfud.cc.ccsx :as x :refer [xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hlayer [R]
  (do-with [layer (x/layer*)]
    (let [{:keys [scores pmap] :as G} (:game @xcfg)
          {:keys [top rhs low lhs]} (x/r->b4 R)
          [cx cy] (x/mid-rect* R)
          pause (fn_* (x/push-scene (options-scene)))
          [kx ky] [(get pmap CV-X) (get pmap CV-O)]
          [px py] [(get G kx) (get G ky)]
          s1 (x/bmf-label* (str (get scores CV-X))
                           (x/gfnt :label)
                           {:pos (js/cc.p 0 top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-LEFT})
          s2 (x/bmf-label* (str (get scores CV-O))
                           (x/gfnt :label)
                           {:pos (js/cc.p rhs top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-RIGHT})]
      ;(x/debug* "hud called")
      (x/add-> layer s1 (name kx))
      (x/add-> layer s2 (name ky))
      (-> (x/add-> layer (x/bmf-label*
                           "" (x/gfnt :text)) "status")
          (x/set!! {:pos (js/cc.p cx cy)
                    :color "#ffffff" :scale .3}))
      (x/add-> layer
               (x/gmenu {:nnn "#icon_menu.png" :cb pause}
                        {:region R :anchor x/ANCHOR-TOP}) "pause"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-scene [mode]
  (do-with [scene (x/scene*)]
    (let [{:keys [vert?]} (:game @xcfg)
          [bl gl] [(x/layer*) (x/layer*)]
          white (x/color* 255 255 255)
          R (x/vrect)
          {:keys [top lhs rhs low]} (x/r->b4 R)
          [width height] (x/r-> R)
          [cx cy] (x/mid-rect* R)
          rl (x/add-> layer (new js/cc.DrawNode) "border" -1)
          hud (hlayer R)
          S {:walls {:w (js/cc.rect lhs low 1 height)
                     :e (js/cc.rect (- rhs 1) low 1 height)
                     :n (js/cc.rect lhs (- top 1) width 1)
                     :s (js/cc.rect lhs low width 1)}
             ;^ 4 invisible static walls
             :arena R
             :running? false
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
                              [:game] #(c/deep-merge % S))))
      (ocall! rl
              "drawRect"
              (js/cc.p lhs low)
              (js/cc.p rhs top) nil 64 white)
      (if vert?
        (ocall! r "drawSegment" (js/cc.p lhs y ) (js/cc.p rhs y) 16 white)
        (ocall! r "drawSegment" (js/cc.p x low) (js/cc.p x top) 16 white))
      (x/center!! R
                  bl
                  (rot-flat?? (x/sprite*
                                (x/gimg :game-bg))) "bg" -2)
      (x/add-> scene gl "arena" 1)
      (x/add-> scene hud "hud" 2)
      (impl/init )
      (ocall! @*game-scene* "scheduleUpdate")
      (swap! xcfg #(assoc-in % [:game :running?] true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn options-scene [& [options]]
  (do-with [scene (x/scene*)]
    (let [fquit (fn_* (x/pop->root) (x/run-scene (splash-scene)))
          fsound (fn_* (x/sfx! (zero? (x/gsidx (_1 ____xs)))))
          fback (fn_* (x/pop-scene))
          fp1 (fn_* (let [n (x/gsidx (_1 ____xs))
                          m (if (zero? n)
                              {CV-X :player CV-O :pother}
                              {CV-X :pother CV-O :player})]
                      (swap! xcfg
                             (fn [root]
                               (-> (assoc-in root [:game :pmap] m)
                                   (assoc-in [:game :player :pvalue]
                                             (if (zero? n) CV-X CV-O))
                                   (assoc-in [:game :pother :pvalue]
                                             (if (zero? n) CV-O CV-X)))))))
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
          i2 (x/mitoggle* (x/mifont-item* "X" 26)
                          (x/mifont-item* "O" 26) fp1)
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
                                    :pos (js/cc.p cx (* .8 top))}))
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
      (x/center!! R
                  layer
                  (-> (x/gimg :game-bg)
                      (x/sprite*)
                      (rot-flat??)) "bg" -1)
      (x/add-> layer
               (x/bmf-label* (x/l10n "%mmenu")
                             (x/gfnt :title)
                             {:color "#EDFF90"
                              :pos (js/cc.p cx (* .8 top))}))
      (x/center!! R
                  layer
                  (x/gmenu
                    [{:nnn "#player1.png" :cb fp1}
                     {:nnn "#player2.png" :cb fp2}
                     {:nnn "#online.png" :cb fnet}
                     {:nnn "#options.png" :cb fopt}
                     {:nnn "#quit.png" :cb fquit}])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rot-flat?? "" [obj]
  (if-not (x/is-portrait?)
    (oc/ocall! obj "setRotation" 90)) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splash-scene []
  (do-with [scene (x/scene*)]
    (let [fplay (fn_* (x/run-scene (menu-scene)))
          R (x/vrect)
          {:keys [top low lhs rhs]} (x/r->b4 R)
          [x y] (x/mid-rect* R)
          layer (x/add-> scene (x/layer*))]
      (x/center!! R
                  layer
                  (rot-flat?? (x/sprite*
                                (x/gimg :game-bg))) "bg" -1)
      (x/pos! (x/add-> layer
                       (x/sprite* "#title.png")) x (* .8 top))
      (x/pos! (->> (x/gmenu {:cb fplay
                             :nnn "#play.png"})
                   (x/add-> layer)) x (* .1 top)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


