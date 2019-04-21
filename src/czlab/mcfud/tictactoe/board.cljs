;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.tictactoe.board

  (:require-macros [czlab.mcfud.cc.ccsx :as cx]
                   [czlab.mcfud.afx.core
                    :as ec :refer [each* _1 n#]])

  (:require [czlab.mcfud.cc.ccsx :as cx :refer []]
            [czlab.mcfud.afx.algos :as ag]
            [czlab.mcfud.afx.core :as ec :refer [raise!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- noWin?

  ""
  [game cvz]

  (not-any? #(= % cvz) game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isWin??

  ""
  [actor game gspace]

  (let [combo
        (some (fn [c]
                (if (every? #(= % actor)
                            (map #(nth game %) c)) c)) gspace)]
    (if (some? combo)
      [actor combo] [nil nil])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn TTTBoard

  ""
  [size CV-Z CV-X CV-O gspace]

  (let [grid (ec/fillArray CV-Z (* size size))
        actors (array 0 CV-X CV-O)
        gop (fn [pv]
              (cond (= pv (nth actors 1))
                    (nth actors 2)
                    (= pv (nth actors 2))
                    (nth actors 1)))]
    {:firstMove (fn []
                  (if (every? #(= CV-Z %) grid)
                    (ec/randRange 0 (dec (n# grid))) -1))
     :syncState (fn [seed actor]
                  (ec/copyArray seed grid)
                  (aset actors 0 actor))
     :nextMoves (fn [snap]
                  (let [rc (transient [])]
                    (each*
                      (fn [v pos]
                        (if (= CV-Z v) (conj! rc pos))) (:state @snap))
                    (persistent! rc)))
     :undoMove (fn [snap move]
                 (aset (:state @snap) move CV-Z))
     :makeMove (fn [snap move]
                 (let [{:keys [state cur]} @snap]
                   (if (= CV-Z (nth state move))
                     (aset state move cur)
                     (raise! "cell [" move "] is not free."))))
     :switchPlayer (fn [snap]
                     (let [{:keys [cur other]} @snap]
                       (swap! snap #(assoc %
                                           :cur other :other cur))))
     :getOtherPlayer gop
     :takeSnapshot #(ag/mark!! (nth actors 0)
                               (gop (nth actors 0)) -1 (.slice grid 0))
     :evalScore (fn [snap]
                  (let [{:keys [other state]} @snap]
                    ;;if we lose, return a nega value
                    (if (number? (_1 (isWin?? other state gspace))) -100  0)))
     :isOver? (fn [snap]
                (let [{:keys [cur other state]} @snap]
                  (or (number? (_1 (isWin?? cur state gspace)))
                      (number? (_1 (isWin?? other state gspace)))
                      (noWin? state CV-Z))))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


