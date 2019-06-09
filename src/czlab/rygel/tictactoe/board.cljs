;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
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
(defn- is-win?? [who game gspace]
  (if-some [combo (some (fn [c]
                          (if (every? #(= % who)
                                      (map #(nth game %) c)) c)) gspace)]
    [who combo] [nil nil]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn game-board [size gspace]
  (let [actor (atom 0)
        gop #(condp = %
               CV-X CV-O
               CV-O CV-X nil)
        grid (c/fill-array CV-Z (* size size))]
    {:first-move (fn_0 (if (every? #(= CV-Z %) grid)
                         (c/rand-range 0 (dec (n# grid))) -1))
     :sync-state (fn [seed cur]
                   (c/copy-array seed grid) (reset! actor cur))
     :next-moves #(loop [state (:state (deref %))
                         i 0
                         CZ (n# state)
                         out (c/tvec*)]
                    (if (>= i CZ)
                      (c/pert! out)
                      (recur state
                             (+ 1 i)
                             CZ
                             (if (= CV-Z
                                    (nth state i)) (conj! out i) out))))
     :undo-move #(aset (:state (deref %1)) %2 CV-Z)
     :make-move #(let [{:keys [state cur]} (deref %1)]
                   (if (= CV-Z (nth state %2))
                     (aset state %2 cur)
                     (c/raise! "cell [" %2 "] is not free.")))
     :switch-player #(swap! %
                            (fn [{:keys [cur other] :as S}]
                              (assoc S :cur other :other cur)))
     :get-other-player gop
     :take-snapshot #(a/mark!! @actor
                               (gop @actor) -1 (.slice grid 0))
     :eval-score #(let [{:keys [other state]} (deref %)]
                    ;;if we lose, return a nega value
                    (if (number? (_1 (is-win?? other state gspace))) -100  0))
     :is-over? #(let [{:keys [cur other state]} (deref %)]
                  (or (number? (_1 (is-win?? cur state gspace)))
                      (number? (_1 (is-win?? other state gspace)))
                      (no-win? state)))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


