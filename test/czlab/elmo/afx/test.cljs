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

  (:require [czlab.elmo.afx.ebus :as bus]
            [czlab.elmo.afx.ecs :as ecs]
            [clojure.string :as cs]
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

(js/console.log (runtest test-core "elmo test-core"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test ecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- compA "" [] #js{:a 50 })
(defn- compB "" [] #js{:b 10 })
(defn- compC "" [] #js{:c 15 })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def compDPool (ecs/createPool
                 (fn [] #js{:d 10}) (fn [x] x) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compD "" [] (ecs/takeFromPool! compDPool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def tmpA
  {:components [[:c1] [:c3]] :initor (fn [] nil) })

(def tmpB
  {:components [[:c2] [:c3]] :initor (fn [] nil) })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *ecs* (ecs/createECS))
(def TEMP-VAR (atom []))

(defn- sys1 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 1)))
(defn- sys2 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 2)))
(defn- sys3 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def pool1 (ecs/createPool
             (fn [] #js{:a 0}) (fn [x] x) 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest ecs-test

  (ensure?? (zero? (ecs/sizeOfPool pool1)) "pool,size=0")
  (ensure?? (let [x (ecs/takeFromPool! pool1)
                  y (ecs/takeFromPool! pool1)
                  z (ecs/takeFromPool! pool1)]
              (= 3 (ecs/countUsedInPool pool1))) "pool,used")
  (ensure?? (let [x (ecs/takeFromPool! pool1)
                  y (ecs/takeFromPool! pool1)
                  z (ecs/takeFromPool! pool1)]
              (ecs/returnToPool! pool1 z)
              (= 5 (ecs/countUsedInPool pool1))) "pool,drop")
  (ensure?? (= 6 (ecs/sizeOfPool pool1)) "pool,size>0")
  (ensure?? (let [x (ecs/takeFromPool! pool1)
                  y (ecs/takeFromPool! pool1)
                  z (ecs/takeFromPool! pool1)]
              (ecs/returnToPool! pool1 x)
              (and (= 7 (ecs/countUsedInPool pool1))
                   (= 12 (ecs/sizeOfPool pool1)))) "pool,grow")

  (ensure?? (= 1 (do (ecs/addComponent *ecs* :c1 compA)
                     (count (ecs/getComponentKeys *ecs*)))) "addComponent,1")
  (ensure?? (= 3 (do (ecs/addComponent *ecs* :c2 compB :c3 compC)
                     (count (ecs/getComponentKeys *ecs*)))) "addComponent,*")

  (ensure?? (= 2 (do (ecs/removeComponent *ecs* :c1)
                     (count (ecs/getComponentKeys *ecs*)))) "removeComponent,1")
  (ensure?? (= 0 (do (ecs/removeComponent *ecs* :c2 :c3)
                     (count (ecs/getComponentKeys *ecs*)))) "removeComponent,*")

  (ensure?? (= 4 (do (ecs/addComponent *ecs* :c1 compA :c2 compB :c3 compC :c4 compD)
                     (count (ecs/getComponentKeys *ecs*)))) "addComponent,**")

  (ensure?? (= 2 (do (ecs/addTemplate *ecs* :t1 tmpA :t2 tmpB)
                     (count (ecs/getTemplateKeys *ecs*)))) "addTemplate,*")

  (ensure?? (= 0 (do (ecs/removeTemplate *ecs* :t1 :t2)
                     (count (ecs/getTemplateKeys *ecs*)))) "removeTemplate,*")

  (ensure?? (= 2 (do (ecs/addTemplate *ecs* :t1 tmpA :t2 tmpB)
                     (count (ecs/getTemplateKeys *ecs*)))) "addTemplate,*")

  (ensure?? (let [x (ecs/createEntity *ecs* [:c1] [:c2] [:c3])]
              (and (ecs/componentInEntity? *ecs* x :c1 :c2 :c3)
                   (ecs/getEntityData *ecs* x :c1)
                   (ecs/getEntityData *ecs* x :c2)
                   (ecs/getEntityData *ecs* x :c3))) "createEntity,getData")

  (ensure?? (let [a (ecs/findComponent *ecs* :c1)
                  b (ecs/findComponent *ecs* :c2)
                  c (ecs/findComponent *ecs* :c3)
                  d (ecs/findComponent *ecs* :xxx)]
              (and a b c (ec/nichts? d))) "engine,find")

  (ensure?? (let [x (ecs/createTemplateEntity *ecs* :t2)]
              (and (ecs/componentInEntity? *ecs* x :c2 :c3)
                   (ec/nichts? (ecs/getEntityData *ecs* x :c1))
                   (ecs/getEntityData *ecs* x :c2)
                   (ecs/getEntityData *ecs* x :c3))) "createTemplateEntity,getData")

  (ensure?? (let [x (ecs/createTemplateEntity *ecs* :t1)
                  y (ecs/getEntityData *ecs* x :c3)
                  _ (ecs/removeEntity *ecs* x)
                  z (ecs/getEntityData *ecs* x :c1)]
              (and (some? y)
                   (ec/nichts? z))) "removeEntity")

  (ensure?? (let [x (ecs/createEntity *ecs* [:c4])
                  ok (and (ecs/componentInEntity? *ecs* x :c4)
                          (ecs/getEntityData *ecs* x :c4))]
              (ecs/removeEntity *ecs* x)
              (and ok
                   (not (ecs/componentInEntity? *ecs* x :c4)))) "createEntity,pooled")

  (ensure?? (= [1 2 3]
               (do (reset! TEMP-VAR [])
                   (ecs/addSystem *ecs* sys1 sys2 sys3)
                   (ecs/updateECS *ecs* 10)
                   @TEMP-VAR)) "engine,addSystem"))

(js/console.log (runtest ecs-test "elmo test-ecs"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cbbus [x y z] (swap! TMPVAR inc))
(deftest ebus-test

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createEvBus)]
              (bus/sub* b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "evbus, sub*")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createEvBus)]
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "evbus, sub+")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createEvBus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/pause b s)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (bus/resume b s)
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "evbus, pause, resume")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createEvBus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/unsub b s)
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "evbus, unsub")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createRvBus)]
              (bus/sub* b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "rvbus, sub*")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createRvBus)]
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "rvbus, sub+")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createRvBus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/pause b s)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (bus/resume b s)
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "rvbus, pause, resume")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createRvBus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/unsub b s)
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "rvbus, unsub")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/createRvBus)]
              (bus/sub+ b "a.*.>" cbbus)
              (bus/sub+ b "a.*.c" cbbus)
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (= 3 @TMPVAR)) "rvbus, sub a.*.b")

  (ensure?? (let [b (bus/createRvBus) m (deref b)]
              (bus/sub+ b "a.*.>" cbbus)
              (bus/sub+ b "a.*.c" cbbus)
              (bus/sub+ b "a.b.c" cbbus)
              (bus/finz b)
              (= m @b)) "rvbus, finz")

  (ensure?? (= 3 (count (deref (bus/createEvBus)))) "createEvBus")
  (ensure?? (= 3 (count (deref (bus/createRvBus)))) "createRvBus"))

(js/console.log (runtest ebus-test "elmo test-ebus"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

