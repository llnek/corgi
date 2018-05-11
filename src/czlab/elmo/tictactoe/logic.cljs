

(defn doit "" [node evt]

  (let [ps this.state.players
        cp (nth ps this.state.actor)
        board= node.board
        grid= node.grid
        bot= node.robot
        sel= node.selection]
    ;;for the bot, create some small delay...
    (cond
      (= 2 cp.category)
      (if (some? botTimer)
        (when (cx/timerDone? botTimer)
          (let [bd bot.algo.getGameBoard]
            (syncState bd grid.values cp.value)
            (rc (getFirstMove bd))
            (if (nichts? rc)  (rc (algo.eval)))
            (enqueue rc cp.value grid)
            (botTimer (cx/undoTimer! botTimer))))
        (botTimer (cx/createTimer sh.main 0.6)))
      (>= sel.cell 0)
      ;;possibly a valid click ? handle it
      (enqueue sel.cellcp.value grid))

    (oset! sel.cell -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- enqueue "" [pos value grid]
  (when (and (>= pos 0)
             (< pos (n# grid))
             (= CV-Z (nth grid pos)))
    (cx/fire "hud.timer.hide")
    (let [[snd pnum]
          (if (= 1 this.state.actor) ["x_pick" 2] ["o_pick" 1])]
      (oset! grid.values pos value)
      (set! state.actor pnum)
      (cx/sfxPlay snd)
      (if (= 1 state.players[pnum].category)
        (cx/fire "hud.timer.show")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


