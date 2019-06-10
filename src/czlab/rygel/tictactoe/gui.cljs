;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
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
                                          fn_1 fn_*
                                          let->nil do-with fn-nil]]
            [czlab.mcfud.afx.ebus :as u]
            [czlab.mcfud.cc.ccsx :as x
                                 :refer [P-BOT G-ONE G-TWO
                                         CV-X CV-O CV-Z xcfg]]
            [czlab.rygel.tictactoe.core :as t]
            [czlab.rygel.tictactoe.board :as b]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare options-scene splash-scene)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- map-goal-space
  "Set of all possible winning combinations."
  [size]
  (loop [row 0
         dx (c/tvec*)
         dy (c/tvec*)
         rows (c/tvec*)
         cols (c/tvec*)]
    (if (>= row size)
      (cc+ [(c/pert! dx) (c/pert! dy)]
           (c/pert! rows) (c/pert! cols))
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
               (conj! rows (c/pert! h))
               (conj! cols (c/pert! v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- map-grid-pos
  "Memorize the co-ordinates of each cell on the board, so
  that we know which cell the user has clicked on."
  [R gsz scale]
  (let [[width height] (x/r-> (x/bsize "#z.png"))
        [W H] (c/mapfv * scale width height)
        [cx cy] (x/p-> (x/mid-rect R))
        ro (* (/ 8 72) scale)
        cells (* gsz gsz)
        gsz' (- gsz 1)
        [gw gh] (c/mapfv * ro W H) ;line-gap
        zw (+ (* gsz W) (* gw gsz')) ;sum width of icons
        zh (+ (* gsz H) (* gh gsz')) ;sum height of icons
        [x0 y0] [(- cx (/ zw 2)) (+ cy (/ zh 2))]] ;top-left
    (loop [row 0 x1 x0 y1 y0 out (c/tvec*)]
      (if (>= row gsz)
        (c/pert! out)
        (let [[y' out']
              (loop [col 0 x1' x1 y1' y1 out'' out]
                (let [y2 (- y1' H) x2 (+ x1' W)]
                  (if (>= col gsz)
                    [(- y2 gh) out'']
                    (recur (+ 1 col)
                           (+ x2 gw)
                           y1'
                           (conj! out'' (js/cc.rect x1' y2 W H))))))]
          (recur (+ 1 row) x1 y' out'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hlayer [R]
  (do-with [layer (x/layer*)]
    (let [{:keys [scores gpos pmap] :as G} (:game @xcfg)
          gend (x/oget-y (c/min-by #(x/oget-y %) gpos))
          [cx cy] (x/p-> R)
          {:keys [top right bottom left]} (x/r->b4 R)
          pause (fn_* (x/push-scene (options-scene)))
          kx (get pmap CV-X)
          ky (get pmap CV-O)
          px (get G kx)
          py (get G ky)
          title (x/bmf-label* (str (:pid px) " [X]/"
                                   (:pid py) "[O] ")
                              (x/gfnt :title)
                              {:pos (js/cc.p cx top)
                               :anchor x/ANCHOR-TOP
                               :color "#5e3178"
                               :scale .6})
          s1 (x/bmf-label* (str (get scores CV-X))
                           (x/gfnt :label)
                           {:pos (js/cc.p 0 top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-LEFT})
          s2 (x/bmf-label* (str (get scores CV-O))
                           (x/gfnt :label)
                           {:pos (js/cc.p right top)
                            :color "#ffffff"
                            :scale .6
                            :anchor x/ANCHOR-TOP-RIGHT})]
      (x/debug* "hud called")
      (x/add-> layer s1 (name kx))
      (x/add-> layer s2 (name ky))
      (x/add-> layer
               (x/bmf-label* ""
                             (x/gfnt :text)
                             {:pos (js/cc.p cx
                                            (+ bottom (/ (- gend bottom) 2)))
                              :color "#ffffff"
                              :scale .3}) "status")
      (x/add-> layer
               (x/gmenu [{:nnn "#icon_menu.png" :cb pause}]
                        {:region R :anchor x/ANCHOR-TOP}) "pause"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init-game-scene []
  (let [;cb (fn_* (.push evQ ____xs))
        {:keys [gspace
                gmode
                scene
                ebus
                pmap
                bot-time
                grid-size
                begin-with] :as G} (:game @xcfg)
        ;whose turn?
        turn (if (= begin-with CV-Z)
               (if (pos? (c/rand-sign)) CV-X CV-O) begin-with)]
    (u/sub+ ebus "mouse.up" (fn_* (apply t/on-click  ____xs)))
    (u/sub+ ebus "touch.one.end" (fn_* (apply t/on-touch ____xs)))
    ;select who starts
    (swap! xcfg #(assoc-in % [:game :turn] turn))
    (let [{:keys [ptype pid]} (get G (get pmap turn))]
      (when (= G-ONE gmode)
        ;if single player, create the A.I.
        (swap! xcfg
               #(assoc-in %
                          [:game :bot] (b/game-board grid-size gspace)))
        ;if bot starts, run it
        (if (= P-BOT ptype)
          (ocall! scene
                  "scheduleOnce"
                  (t/run-bot true) bot-time)))
      (t/write-status (str pid " starts!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-scene [mode & more]
  (do-with [scene (x/scene*)]
    (let [{{:keys [grid-size]} :game} @xcfg
          sz (* grid-size grid-size)
          R (x/vrect)
          bg (x/layer*)
          gl (x/layer*)
          S {:gspace (map-goal-space grid-size)
             :gpos (map-grid-pos R grid-size 1)
             :grid (c/fill-array CV-Z sz)
             :ebus (u/new-event-bus)
             :running? false
             :depth 10
             :scene scene
             :gmode mode
             :selected -1
             :evQ #js []
             :turn nil
             :scores {CV-X 0 CV-O 0}}
          cs (mapv #(let [s (x/sprite* "#z.png")]
                      [(x/center!! %1 gl s) CV-Z]) (:gpos S))]
      (swap! xcfg
             (fn_1 (update-in ____1
                              [:game]
                              #(merge %
                                      (assoc S :cells cs)))))
      (x/center-image R
                      (x/add-> scene bg "bg" -1) (x/gimg :game-bg))
      (x/add-> scene (hlayer R) "hud" 2)
      (x/add-> scene gl "arena" 1)
      (x/attr* scene #js{:update #()})
      (init-game-scene)
      (x/debug* "game cfg= " (c/jsonize (:game @xcfg)))
      (swap! xcfg
             #(assoc-in % [:game :running?] true)))))

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
          region (x/vrect)
          {:keys [top]} (x/r->b4 region)
          [cx cy] (x/p-> (x/mid-rect region))
          t1 (x/mifont-text* "Sound" 18)
          i1 (x/mitoggle* (x/mifont-item* "On" 26)
                          (x/mifont-item* "Off" 26) fsound)
          t2 (x/mifont-text* "Player 1" 18)
          i2 (x/mitoggle* (x/mifont-item* "X" 26)
                          (x/mifont-item* "O" 26) fp1)
          t3 (x/mifont-text* "First Move" 18)
          i3 (x/mitoggle* (x/mifont-item* "X" 26)
                          (x/mifont-item* "O" 26)
                          (x/mifont-item* "?" 26) ffmove)
          gmenu (x/add-> layer
                         (if quit?
                           (x/menu* t1 i1 t2 i2 t3 i3 [fback fquit])
                           (x/menu* t1 i1 t2 i2 t3 i3 [fback])))
          quit (x/milabel* (x/ttf-text* "Quit" "Arial" 20) fquit)
          back (x/milabel* (x/ttf-text* "Go back" "Arial" 20) fback)]
      (x/center-image layer (x/gimg :game-bg) "bg" -1)
      (x/add-> layer (x/bmf-label* (x/l10n "%options")
                                   (x/gfnt :title)
                                   {:color "#F6B17F"
                                   :pos (js/cc.p cx (* .8 top))}))
      (x/toggle-select! i1 (if (x/sfx?) 0 1))
      (x/toggle-select! i3 (if (= begin-with CV-X) 0 1))
      (x/toggle-select! i2 (if (= (:pvalue player) CV-X) 0 1))
      (if quit?
        (x/align-in-cols gmenu [2 2 2 1 1])
        (x/align-in-cols gmenu [2 2 2 1])))))

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
          [cx cy] (x/p-> (x/mid-rect R))]
      (x/center-image layer (x/gimg :game-bg) "bg" -1)
      (x/add-> layer (x/bmf-label* (x/l10n "%mmenu")
                                   (x/gfnt :title)
                                   {:color "#F6B17F"
                                    :pos (js/cc.p cx (* .8 top))}))
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
  (do-with [scene (x/scene*)]
    (let [onplay (fn_* (x/run-scene (menu-scene)))
          {:keys [grid-size]} (:game @xcfg)
          layer (x/add-> scene (x/layer*))
          R (x/vrect)
          scale .75
          [cx cy] (x/mid-rect R)
          {:keys [top]} (x/r->b4 R)]
      ;add background image
      (x/center-image layer (x/gimg :game-bg) "bg" -1)
      ;add title
      (x/add-> layer
               (x/sprite* "#title.png")
               {:pos (js/cc.p cx (* .8 top))})
      ;add play button
      (x/add-> layer
               (x/gmenu [{:cb onplay :nnn "#play.png"}]
                        {:pos (js/cc.p cx (* .1 top))}))
      ;;draw demo
      ;; we scale down the icons to make it look nicer
      (c/each*
        (fn [mp pos]
            (x/add-> layer
                     (x/sprite* (case pos
                                (1 5 6 7) "#x.png"
                                (0 4) "#z.png" "#o.png"))
                     {:pos (x/mid-rect mp) :scale scale}))
        (map-grid-pos R grid-size scale)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF




