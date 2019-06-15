;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.tictactoe.board

  (:require [czlab.mcfud.cc.ccsx :as x :refer [xcfg CV-Z CV-X CV-O]]
            [czlab.mcfud.afx.algos :as a]
            [czlab.mcfud.afx.core :as c :refer [fn_0 n# _1 _2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- no-win? [game] (not-any? #(= % CV-Z) game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-win?? [who game goals]
  (if-some [combo (some (fn [c]
                          (if (every? #(= % who)
                                      (map #(nth game %) c)) c)) goals)]
    [who combo] [nil nil]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ttt [size goals]
  (let [grid (c/fill-array CV-Z (* size size))
        grid-size (n# grid)
        actor (atom 0)
        gop #(condp = % CV-X CV-O CV-O CV-X nil)]
    {:first-move (fn_0 (if (every? #(= CV-Z %) grid)
                         (c/rand-range 0 (- grid-size 1)) -1))
     :sync-state! (fn [seed cur]
                    (reset! actor cur)
                    (c/copy-array seed grid)
                    (atom (a/Snapshot. @actor
                                       (gop @actor)
                                       (.slice grid 0) -1)))
     :best-move! (fn [game move]
                   (swap! game
                          #(assoc % :last-best-move move)))
     :next-moves #(loop [state (:state (deref %))
                         i 0
                         CZ (n# state)
                         out (c/tvec*)]
                    (if (>= i CZ)
                      (c/ps! out)
                      (recur state
                             (+ 1 i)
                             CZ
                             (if (= CV-Z
                                    (nth state i)) (conj! out i) out))))
     :undo-move! #(aset (:state (deref %1)) %2 CV-Z)
     :make-move! #(let [{:keys [state cur]} (deref %1)]
                    (if (= CV-Z (nth state %2))
                      (aset state %2 cur)
                      (c/raise! "cell [" %2 "] is not free.")))
     :switch-play! #(swap! %
                           (fn [{:keys [cur other] :as S}]
                             (assoc S :cur other :other cur)))
     :eval-score #(let [{:keys [other state]} (deref %)]
                    ;;if we lose, return a nega value
                    (if (number? (_1 (is-win?? other state goals))) -100  0))
     :is-over? #(let [{:keys [cur other state]} (deref %)]
                  (or (number? (_1 (is-win?? cur state goals)))
                      (number? (_1 (is-win?? other state goals)))
                      (no-win? state)))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


