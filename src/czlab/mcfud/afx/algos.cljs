;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.algos

  (:require [czlab.mcfud.afx.core
             :as c :refer [n# _1 zero?? POS-INF NEG-INF]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare nega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mark!!
  "Checkpoint the current game state."
  [current other lastMove state]
  (atom {:last-best-move lastMove
         :other other :cur current :state state}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nega*
  "The core of the nega max algo."
  [board game level depth alpha beta]
  (let [{:keys [make-move
                undo-move
                next-moves
                switch-player]} board
        tries (next-moves game)
        sz (n# tries)
        m1 (_1 tries)]
    (if (= level depth)
      (swap! game #(assoc % :last-best-move m1)))
    (loop [n 0 brk? false v' NEG-INF
           move' m1 A' alpha B' beta]
      (if (or brk? (>= n sz))
        v'
        (let [n' (+ 1 n) move (nth tries n)]
          (make-move game move)
          (switch-player game)
          (let [rc (- (nega board
                            game
                            (- level 1)
                            depth
                            (- B')
                            (- A'))) v'' (max v' rc)]
            (switch-player game)
            (undo-move game move)
            ;;check
            (if (< A' rc)
              (do (if (= level depth)
                    (swap! game #(assoc % :last-best-move move)))
                  (recur n'
                         (if (>= rc B')
                           true brk?)
                         v'' move rc B'))
              (recur n' brk? v'' move' A' B'))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nega
  "Entry point of the nega max algo."
  [board game level depth alpha beta]
  (let [{:keys [is-over? eval-score]} board]
    (if (or (zero?? level)
            (is-over? game))
      (eval-score game)
      (nega* board game level depth alpha beta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn negamax
  "Run the algo nega-max, returning the next best move."
  [{:keys [marker sync-state] :as board} level & args]
  (apply sync-state args)
  (let [game (marker)]
    (nega board game
          level level
          NEG-INF POS-INF) (:last-best-move @game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

