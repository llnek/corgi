;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.test

  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core
             :as ec :refer [deftest if-some+ when-some+
                            ensureThrown _1 _2 _3
                            runtest ensure?? raise!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def TMPVAR (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for testing state monad
(defn mult3 "" [x] (* 3 x))
(defn add2 "" [x] (+ 2 x))

;wrapper so that the actual computation is inside
;a state-monadic value, together with the log msg
(defn exlog "" [expr log]
  (fn [s]
    (let [v (:value s)
          v' (expr v)
          msg (str log "(" v ")")
          log' (conj (:log s) msg)]
      [v' {:value v' :log log'}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-core

  (ensure?? (= 1 (_1 [1 2])) "1st")
  (ensure?? (= 2 (_2 [1 2])) "2nd")
  (ensure?? (= 3 (_3 [1 2 3])) "3rd")

  (ensure?? (= (ec/toFloat "1.2") 1.2) "toFloat")
  (ensure?? (= (ec/toInt "12") 12) "toInt")

  (ensure?? (= 2 (ec/last-index [1 2 3])) "last-index")
  (ensure?? (= 2 (nth (ec/cdr [1 2]) 0)) "cdr")
  (ensure?? (= 1 (ec/car [1 2])) "car")
  (ensure?? (= 2 (ec/nexth [1 2] 0)) "nexth")

  (ensure?? (= 3 (ec/do-with [a (+ 1 2)]
                             (/ a 3))) "do-with")
  (ensure?? (= false (ec/do->false (+ 1 2) (= 1 1))) "do->false")
  (ensure?? (= true (ec/do->true (+ 1 2) (= 1 2))) "do->true")
  (ensure?? (= nil (ec/do->nil (+ 1 2) 911)) "do->nil")

  (ensure?? (= "hello!"
               (if-some+ [s (identity "hello")] (str s "!"))) "if-some+")
  (ensure?? (= "ab"
               (if-some+ [s (identity "")]
                         (str s "!")
                         (str "ab"))) "if-some+->else")
  (ensure?? (= "$hello"
               (when-some+ [s (identity "hello")]
                           (count s)
                           (str "$" s))) "when-some+")

  (ensure?? (= 6 (do (reset! TMPVAR 0)
                     (ec/each #(swap! TMPVAR + %) [1 2 3]) (deref TMPVAR))) "each")

  (ensure?? (and (ec/nichts? nil)
                 (ec/nichts? js/undefined)) "nichts?")

  (ensure?? (= "abc&lt;&gt;&amp;&quot;&apos;xyz"
               (ec/escXml "abc<>&\"'xyz")) "escXml")

  (ensure?? (let [[x y]
                  (ec/split-seq [1 2 3 4 5] 3)]
              (and (= [1 2 3] x)
                   (= [4 5] y))) "split-seq")

  (ensure?? (= 50 (ec/percent 20 40)) "percent")
  (ensure?? (= "3.333" (ec/toFixed (/ 10 3) 3)) "toFixed")

  (ensure?? (= ["123" "456" "78"]
               (ec/split-str 3 "12345678")) "split-str")

  (ensure?? (= 698 (ec/maxBy identity [78 7 698 4 5 2 -1])) "maxBy")
  (ensure?? (= -1 (ec/minBy identity [78 7 6 4 5 2 -1])) "minBy")

  (ensure?? (= 3 (ec/domonad ec/m-identity
                             [a 1 b (inc a)] (+ a b))) "identity monad")
  (ensureThrown "any"
                (ec/domonad ec/m-identity
                            [a nil
                             b a
                             c (ec/toStr b)] (+ a b c)) "identity monad->boom")

  (ensure?? (= 3 (ec/domonad ec/m-maybe
                             [a 1 b (inc a)] (+ a b))) "maybe monad")

  (ensure?? (nil? (ec/domonad ec/m-maybe
                              [a 1
                               b (inc a)
                               c nil] (+ a b c))) "maybe monad->nil")

  (ensure?? (= [5, {:value 5 :log ["mult3(1)" "add2(3)"]}]
               ((ec/domonad ec/m-state
                            [c1 (exlog mult3 "mult3")
                             c2 (exlog add2 "add2")]
                            c2) {:value 1 :log []})) "state monad")

  (ensure?? (= 3 (ec/run-cont
                   (ec/domonad ec/m-continuation
                               [x ((fn [v] (fn [c] (c v))) 1)
                                y ((fn [v] (fn [c] (c v))) 2)]
                               (+ x y)))) "continuation monad")

  (ensure?? (let [f (fn [v] (fn [s] [v s]))
                  lhs ((:bind ec/m-state) ((:unit ec/m-state) 911) f)
                  rhs (f 911)
                  lf (lhs "hello")
                  rt (rhs "hello")]
              (and (= (_1 lf)(_1 rt))
                   (= (last lf)(last rt))))
            "monad rule 1: bind(unit(x), f) ≡ f(x)")

  (ensure?? (let [mv (fn [s] [3 s])
                  lhs ((:bind ec/m-state) mv (:unit ec/m-state))
                  lf (lhs "hello")
                  rt (mv "hello")]
              (and (= (_1 lf)(_1 rt))
                   (= (last lf)(last rt))))
            "monad rule 2: bind(m, unit) ≡ m")

  (ensure?? (let [f (fn [v] (fn [s] [3 s]))
                  g (fn [v] (fn [s] [5 s]))
                  bb (:bind ec/m-state)
                  mv (fn [s] [7 s])
                  lhs (bb (bb mv f) g)
                  rhs (bb mv (fn [v] (bb (f v) g)))
                  lf (lhs "hello")
                  rt (rhs "hello")]
              (and (= (_1 lf)(_1 rt))
                   (= (last lf)(last rt))))
            (str "monad rule 3:"
                 " bind(bind(m, f), g)"
                 " ≡ bind(m, v ⇒ bind(f(v), g))"))

  (ensureThrown js/Error (raise! "hello" "world") "raise!"))

(js/console.log (runtest test-core "elmo test-suite"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

