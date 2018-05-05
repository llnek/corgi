;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.algos

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1]])
  (:require [czlab.elmo.afx.core :as ec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def PINF 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare negamax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isGameOver? "" [board game] false)
(defn evalScore "" [board game] 0)
(defn getNextMoves "" [board game] [])
(defn makeMove "" [board game move] nil)
(defn switchPlayer "" [board game] nil)
(defn unmakeMove "" [board game move] nil)
(defn takeSnapshot "" [board] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negamax* "" [board game maxDepth depth alpha beta]
  (let [openMoves (getNextMoves board game) sz (count openMoves) m1 (_1 openMoves)]
    (if (= depth maxDepth) (swap! game #(update-in % [:lastBestMove] (fn [_] m1))))
    (loop [n 0 break? false bestValue' (- PINF) bestMove' m1 alpha' alpha beta' beta]
      (if (or break? (>= n sz))
        bestValue'
        (let [move (nth openMoves n)
              _ (makeMove board game move)
              _ (switchPlayer board game)
              rc (- (negamax board game maxDepth
                             (- depth 1) (- beta') (- alpha')))
              _ (switchPlayer board game)
              _ (unmakeMove board game move)
              bestValue'' (js/Math.max bestValue' rc )]
          (if (< alpha' rc)
            (do (if (= depth maxDepth) (swap! game #(update-in % [:lastBestMove] (fn [_] move))))
                (recur (inc n) (if (>= rc beta') true break?) bestValue'' move rc beta'))
            (recur (inc n) break? bestValue'' bestMove' alpha' beta')))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negamax "" [board game maxDepth depth alpha beta]
  (if (or (zero? depth)
          (isGameOver? board game))
    (evalScore board game)
    (negamax* board game maxDepth depth alpha beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Snapshot "" []
  (atom {:lastBestMove nil :other nil :cur nil :state nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn evalNegaMax "" [board]
  (let [game (takeSnapshot board)]
    (negamax board game
             10 10 (- PINF) PINF)
    (get-in @game [:lastBestMove])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



