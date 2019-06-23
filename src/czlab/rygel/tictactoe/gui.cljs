;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.tictactoe.gui

  (:require [czlab.mcfud.afx.core :as c
                                  :refer [_1 _2 cc+ cc+1
                                          fn_0 fn_1 fn_*
                                          let->nil do-with fn-nil]]
            [oops.core :as oc]
            [czlab.rygel.tictactoe.core :as t]
            [czlab.rygel.tictactoe.impl :as b]
            [czlab.mcfud.afx.ebus :as u]
            [czlab.mcfud.cc.ccsx :as x
                                 :refer [P-BOT G-ONE G-TWO
                                         P-MAN CV-X CV-O CV-Z xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare options-scene splash-scene)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- map-goal-space
  "Get all possible winning combinations."
  [size]
  (loop [row 0
         dx (c/tvec*)
         dy (c/tvec*)
         rows (c/tvec*)
         cols (c/tvec*)]
    (if (>= row size)
      (cc+ [(c/ps! dx) (c/ps! dy)]
           (c/ps! rows) (c/ps! cols))
      (let [[h v]
            (loop [col 0
                   h (c/tvec*)
                   v (c/tvec*)]
              (if (>= col size)
                [h v]
                (recur (+ 1 col)
                       (conj! h (+ (* row size) col))
                       (conj! v (+ (* col size) row)))))]
        (recur (+ 1 row)
               (conj! dx (+ (* row size) row))
               (conj! dy (+ row (* size
                                   (- size row 1))))
               (conj! rows (c/ps! h))
               (conj! cols (c/ps! v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- map-grid-pos
  "Memorize the co-ordinates of each cell on the board, so
  that we know which cell the user has clicked on."
  [R gsz scale]
  (let [[width height] (x/r-> (x/bsize "#z.png"))
        [W H] (c/mapfv * scale width height)
        [cx cy] (x/mid-rect* R)
        ro (* (/ 8 72) scale)
        cells (* gsz gsz)
        gsz' (- gsz 1)
        [gw gh] (c/mapfv * ro W H) ;line-gap
        zw (+ (* gsz W) (* gw gsz')) ;sum width of icons
        zh (+ (* gsz H) (* gh gsz')) ;sum height of icons
        [x0 y0] [(- cx (/ zw 2)) (+ cy (/ zh 2))]] ;top-left
    (loop [row 0 x1 x0 y1 y0 out (c/tvec*)]
      (if (>= row gsz)
        (c/ps! out)
        (let [[y' out']
              (loop [col 0 x1' x1 y1' y1 out'' out]
                (let [y2 (- y1' H) x2 (+ x1' W)]
                  (if (>= col gsz)
                    [(- y2 gh) out'']
                    (recur (+ 1 col)
                           (+ x2 gw)
                           y1'
                           (conj! out''
                                  (x/ccr* x1' y2 W H))))))]
          (recur (+ 1 row) x1 y' out'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- hlayer [R]
  (do-with [layer (x/layer*)]
    (let [{:keys [scores gpos pmap] :as G} (:game @xcfg)
          gend (x/oget-y (c/min-by #(x/oget-y %) gpos))
          {:keys [top rhs low lhs]} (x/r->b4 R)
          [cx cy] (x/mid-rect* R)
          pause (fn_* (x/push-scene (options-scene)))
          [kx ky] [(get pmap CV-X) (get pmap CV-O)]
          [px py] [(get G kx) (get G ky)]
          title (x/bmf-label* (str (:pid px)
                                   " [X]/[O] " (:pid py))
                              (x/gfnt :title)
                              {:anchor x/ANCHOR-TOP
                               :pos (x/ccp* cx top)
                               :color "#5e3178"
                               :scale .6})
          s1 (x/bmf-label* (str (get scores CV-X))
                           (x/gfnt :label)
                           {:pos (x/ccp* 0 top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-LEFT})
          s2 (x/bmf-label* (str (get scores CV-O))
                           (x/gfnt :label)
                           {:pos (x/ccp* rhs top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-RIGHT})]
      ;(x/debug* "hud called")
      (x/add-> layer s1 (name kx))
      (x/add-> layer s2 (name ky))
      (x/add-> layer title)
      (-> (x/add-> layer (x/bmf-label*
                           "" (x/gfnt :text)) "status")
          (x/set!! {:color "#ffffff"
                    :scale .3
                    :pos (x/ccp* cx (/ (+ low gend) 2))}))
      (x/add-> layer
               (x/gmenu {:nnn "#icon_menu.png" :cb pause}
                        {:region R :anchor x/ANCHOR-BOTTOM-RIGHT}) "pause"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init-game-scene []
  (let [{{:keys [begin-with]} :game :keys [ebus]} @xcfg
        ;whose turn?
        turn (if (= begin-with CV-Z)
               (if (pos? (c/rand-sign)) CV-X CV-O) begin-with)]
    (u/sub+ ebus x/TOUCH-ONE-END t/on-touch)
    (u/sub+ ebus x/MOUSE-UP t/on-click)
    ;select who starts
    (swap! xcfg #(assoc-in % [:game :turn] turn))
    (let [{:keys [goals gmode scene
                  grid-size pmap bot-time] :as G} (:game @xcfg)
          {:keys [ptype pid]} (get G (get pmap turn))]
      (when (= G-ONE gmode) ;if single player, create the A.I.
        (swap! xcfg
               #(assoc-in %
                          [:game :bot] (b/ttt grid-size goals)))
        (if (= P-BOT ptype) ;if bot starts first, run it
          (c/call-js! scene
                      "scheduleOnce"
                      (t/run-bot true) bot-time)))
      (t/write-status (x/l10n "%whoStarts" pid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-scene [mode & more]
  (do-with [scene (x/scene*)]
    (let [{:keys [grid-size]} (:game @xcfg)
          [bg gl] [(x/layer*) (x/layer*)]
          sz (* grid-size grid-size)
          R (x/vrect)
          S {:goals (map-goal-space grid-size)
             :gpos (map-grid-pos R grid-size 1)
             :grid (c/fill-array CV-Z sz)
             :player {:ptype P-MAN
                      :pid (x/l10n "%p1")
                      :pname (x/l10n "%player1")}
             :pother (if (not= mode G-ONE)
                       {:ptype P-MAN
                        :pid (x/l10n "%p2") :pname (x/l10n "%player2")}
                       {:ptype P-BOT
                        :pid (x/l10n "%cpu") :pname (x/l10n "%computer")})
             :running? false
             :scene scene
             :gmode mode
             :selected -1
             :evQ #js []
             :cells []
             :depth 10
             :turn CV-Z
             :scores {CV-X 0 CV-O 0}}
          cs (mapv #(let [s (x/sprite* "#z.png")]
                      [(x/center!! %1 gl s) CV-Z]) (:gpos S))
          fout js/cc.Node.prototype.onExit
          fin js/cc.Node.prototype.onEnter]
      (swap! xcfg
             (fn_1 (update-in ____1
                              [:game]
                              #(assoc (c/merge+ % S) :cells cs))))
      (x/center-image R
                      (x/add-> scene bg "bg" -1) (x/gimg :game-bg))
      (x/add-> scene gl "arena" 1)
      (x/add-> scene (hlayer R) "hud" 2)
      (x/attr* scene
               #js{:update (fn_1 (t/run-game ____1))
                   :onExit (fn_0 (x/disable-events)
                                 (.call fout scene))
                   :onEnter (fn_0 (.call fin scene)
                                  (x/enable-events gl))})
      (init-game-scene)
      (c/call-js! scene "scheduleUpdate")
      (swap! xcfg #(assoc-in % [:game :running?] true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn options-scene [& [options]]
  (do-with [scene (x/scene*)]
    (let [fquit (fn_* (x/pop->root) (x/run-scene (splash-scene)))
          fsound (fn_* (x/sfx! (zero? (x/gsidx (_1 ____xs)))))
          fback (fn_* (x/pop-scene))
          ffmove (fn_* (let [n (x/gsidx (_1 ____xs))]
                         (swap! xcfg
                                #(assoc-in %
                                           [:game :begin-with]
                                           (condp = n 0 CV-X 1 CV-O CV-Z)))))
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
          {:keys [player begin-with]} (:game @xcfg)
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
          t3 (x/mifont-text* (x/l10n "%1stMove") 18)
          i3 (x/mitoggle* (x/mifont-item* "X" 26)
                          (x/mifont-item* "O" 26)
                          (x/mifont-item* "?" 26) ffmove)
          quit (x/milabel* (x/ttf-text* (x/l10n "%quit") "Arial" 20) fquit)
          back (x/milabel* (x/ttf-text* (x/l10n "%back") "Arial" 20) fback)
          gmenu (x/add-> layer
                         (if-not quit?
                           (x/menu* t1 i1 t2 i2 t3 i3 back)
                           (x/menu* t1 i1 t2 i2 t3 i3 back quit)))]
      (x/center-image R layer (x/gimg :game-bg) "bg" -1)
      (x/add-> layer (x/bmf-label* (x/l10n "%options")
                                   (x/gfnt :title)
                                   {:color "#F6B17F"
                                    :pos (x/ccp* cx (* .8 top))}))
      (x/toggle-select! i1 (if (x/sfx?) 0 1))
      (x/toggle-select! i3 (if (= begin-with CV-X) 0 1))
      (x/toggle-select! i2 (if (= (:pvalue player) CV-X) 0 1))
      (if quit?
        (x/align-in-cols gmenu 2 2 2 1 1)
        (x/align-in-cols gmenu 2 2 2 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- net-scene [& xs] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn menu-scene []
  (do-with [scene (x/scene*)]
    (let [fopts (fn_* (x/push-scene (options-scene {:quit? false})))
          {:keys [start-scene]} @xcfg
          pms {:no (fn_* (x/run-scene nil))
               :yes (fn [ws p0 msg]
                      (->> (merge {:ws ws :pnum p0} msg)
                           (game-scene (:1 msg) (:2 msg))
                           (x/run-scene)))}
          fnet (fn_* (x/run-scene (net-scene pms)))
          fquit (fn_* (x/pop->root)
                      (x/run-scene (start-scene)))
          fp1 (fn_* (x/run-scene (game-scene G-ONE)))
          fp2 (fn_* (x/run-scene (game-scene G-TWO)))
          layer (x/add-> scene (x/layer*))
          R (x/vrect)
          {:keys [top]} (x/r->b4 R)
          [cx cy] (x/mid-rect* R)]
      (x/center-image R layer (x/gimg :game-bg) "bg" -1)
      (x/add-> layer (x/bmf-label* (x/l10n "%mmenu")
                                   (x/gfnt :title)
                                   {:color "#F6B17F"
                                    :pos (x/ccp* cx (* .8 top))}))
      ;const color= cc.color('#5E3178')
      (x/center!! R
                  layer
                  (x/gmenu [{:nnn "#player1.png" :cb fp1}
                            {:nnn "#player2.png" :cb fp2}
                            {:nnn "#online.png" :cb fnet}
                            {:nnn "#options.png" :cb fopts}
                            {:nnn "#quit.png" :cb fquit}])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splash-scene []
  (let [[w h](x/browser-size)]
    (x/debug* "browser size, w= " w ", h= " h))
  (do-with [scene (x/scene*)]
    (let [onplay (fn_* (x/run-scene (menu-scene)))
          {:keys [grid-size]} (:game @xcfg)
          layer (x/add-> scene (x/layer*))
          R (x/vrect)
          scale .75
          [cx cy] (x/mid-rect* R)
          {:keys [top]} (x/r->b4 R)]
      ;add background image
      (x/center-image R layer (x/gimg :game-bg) "bg" -1)
      ;add title
      (-> (x/add-> layer
                   (x/sprite* "#title.png"))
          (x/set!! {:pos (x/ccp* cx (* .8 top))}))
      ;add play button
      (x/pos! (x/add-> layer
                       (x/gmenu {:cb onplay
                                 :nnn "#play.png"})) cx (* .1 top))
      ;;draw demo
      ;; we scale down the icons to make it look nicer
      (c/each*
        (fn [mp pos]
            (x/add-> layer
                     (x/set!! (x/sprite* (case pos
                                           (1 5 6 7) "#x.png"
                                           (0 4) "#z.png" "#o.png"))
                              {:pos (x/mid-rect mp) :scale scale})))
        (map-grid-pos R grid-size scale)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

