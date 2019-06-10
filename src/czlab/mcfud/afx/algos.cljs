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
             :as c :refer [n# _1 _2 POS-INF NEG-INF]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Snapshot [cur other state last-best-move])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare nega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nega*
  "The core of the nega max algo."
  [board game depth level alpha beta]
  (let [{:keys [make-move! undo-move!
                best-move!
                next-moves switch-play!]} board
        [m1 & _ :as tries] (next-moves game)]
    (if (= level depth) (best-move! game m1))
    (loop [n 0 SZ (n# tries)
           brk? false
           v' NEG-INF
           move' m1
           a' alpha b' beta]
      (if (or brk? (>= n SZ))
        [v' game]
        (let [move (nth tries n)]
          (make-move! game move)
          (switch-play! game)
          (let [[rc _] (nega board game depth
                             (- level 1) (- b') (- a'))
                rc (- rc)
                n' (+ 1 n)
                v'' (max v' rc)]
            (switch-play! game)
            (undo-move! game move)
            ;;check
            (if (< a' rc)
              (do (if (= level depth)
                    (best-move! game move))
                  (recur n'
                         SZ
                         (if (>= rc b') true brk?) v'' move rc b'))
              (recur n' SZ brk? v'' move' a' b'))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nega
  "Entry point of the nega max algo."
  [{:keys [is-over?
           eval-score] :as board} game depth level alpha beta]
  (if (or (zero? level)
          (is-over? game))
    [(eval-score game) game]
    (nega* board game depth level alpha beta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn negamax
  "Run the algo nega-max, returning the next best move."
  [board depth state player]
  (let [{:keys [sync-state!]} board
        game (sync-state! state player)]
    (:last-best-move @(_2 (nega board
                                game
                                depth
                                depth
                                NEG-INF POS-INF)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

