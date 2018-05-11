(defn syncup "" []
  (let [values node.grid.values
        view node.view
        cs view.cells]

    (each-indexed
      (fn [v pos]
        (if (not= v CV-Z)
          (if-some [c (xrefCell pos view.gridMap)]
            (if-some [z cs[pos]]
              (remove! z[0]))
            (aset cs
                  pos
                  [(drawSymbol view c[0] c[1] v)
                   c[0] c[1] v]))))
      values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xrefCell "" [pos  map]
  ;let gg, x, y, delta=0;
  (if (and (>= pos 0)
           (< pos CELLS))
    (let [gg map[pos]
          x (+ gg.left (* 0.5 (- gg.right gg.left delta)))
          y (- gg.top (* 0.5 (- gg.top gg.bottom delta)))]
      ;;the cell's center
      [x y])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn doit "" [node dt]
  (let [values node.grid.values
        res (some #(if (some? %)
                     (if-some [rc (checkWin % values)] [% rc]))
                  state.players)]
    (cond
      (some? res)
      (doWin node res[0] res[1])
      (checkDraw values)
      (doDraw node)
      (> state.msgQ.length 0)
      (if (= "forfeit" (.shift this.state.msgQ))
        (doForfeit node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doWin "" [node winner combo]
  (cx/fire "hud.score.update"
           {:color winner.color :score 1})
  (doDone node winner combo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doDraw "" []
  (doDone node nil []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doForfeit "" [node]
  (let [other (if (= 1 state.actor) 2 (if (= 2 state.actor) 1 0))
        tv (nth state.players this.state.actor)
        win (nth this.state.players other)
        cs node.view.cells
        layer node.view.layer
        v2 (if (some? tv) tv.value -1)]
    (cx/fire "hud.score.update" {:color win.color :score 1})
    ;;gray out the losing icons
    (each-indexed
      (fn [z n]
        (if (and (some? z)
                 (= z[4] v2))
          (remove! z[0])
          (aset z 0 (utils/drawSymbol node.view z[1] z[2] z[3]+2))))
      cs)
    (doDone node win nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- showWinningIcons "" [view combo]
  (when (some? combo)
    (let [layer view.layer
          cs view.cells]
      (each-indexed
        (fn [z n]
          (if-not (contains? combo n)
            (when (and z (not= z[3] CV-Z))
              (remove! z[0])
              (aset z 0 (utils/drawSymbol view z[1] z[2] z[3] true)))))
        cs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doDone "" [node pobj combo]
  (let [pnum  (if pobj pobj.pnum 0)]
    (showWinningIcons node.view combo)
    (cx/fire "hud.timer.hide")
    (cx/sfxPlay "game_end")
    (cx/fire "hud.end" {:winner pnum })
    (set! this.state.lastWinner pnum)
    (set! this.state.running false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkDraw "" [values]
  (not-any? #(= % CV-Z) values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkWin "" [actor game]
  (cx/debug* "checking win for " actor.color)
  (some
    (fn [combo]
      (if (every? #(= actor.value %)
                  (map #(nth game %) combo))
        combo))
    this.state.GOALSPACE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


