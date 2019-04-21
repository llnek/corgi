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

  (:require-macros [czlab.mcfud.afx.core :as ec
                                         :refer [_1 n#]])
  (:require [czlab.mcfud.afx.core :as ec
                                  :refer [zero?? POS-INF NEG-INF]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare negamax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mark!!

  "Checkpoint the current game state."
  [current other lastMove state]

  (atom {:lastBestMove lastMove
         :other other :cur current :state state}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negamax*

  "The core of the nega max algo."
  [{:keys [nextMoves
           makeMove
           switchPlayer
           undoMove] :as board} game depth maxDepth alpha beta]

  (let [attempts (nextMoves game)
        sz (n# attempts)
        m1 (_1 attempts)]
    (when (= depth maxDepth)
      (swap! game #(assoc % :lastBestMove m1)))
    (loop [n 0 break? false
           bestValue' NEG-INF bestMove' m1 alpha' alpha beta' beta]
      (if (or break? (>= n sz))
        bestValue'
        (let [move (nth attempts n)
              n' (+ 1 n)]
          (doto game (makeMove move) (switchPlayer))
          (let [rc (- (negamax board
                               game
                               (- depth 1) maxDepth (- beta') (- alpha')))
                bestValue'' (max bestValue' rc)]
            (doto game (switchPlayer) (undoMove move))
            ;;check
            (if (< alpha' rc)
              (do (when (= depth maxDepth)
                    (swap! game #(assoc % :lastBestMove move)))
                  (recur n' (if (>= rc beta') true break?) bestValue'' move rc beta'))
              (recur n' break? bestValue'' bestMove' alpha' beta'))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negamax

  "The entry point of the nega max algo."
  [{:keys [isOver? evalScore] :as board} game depth maxDepth alpha beta]

  (if (or (zero?? depth)
          (isOver? game))
    (evalScore game)
    (negamax* board game depth maxDepth alpha beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn evalNegaMax

  "Run the algo nega-max, returning the next best move."
  [{:keys [marker syncState] :as board} depth & args]

  (apply syncState args)
  (let [game (marker)]
    (negamax board game
             depth depth
             NEG-INF POS-INF) (:lastBestMove @game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



