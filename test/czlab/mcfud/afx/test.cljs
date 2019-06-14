;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.test

  (:require [czlab.mcfud.afx.algos :as algos]
            [czlab.mcfud.afx.gfx2d :as gx]
            [czlab.mcfud.afx.crypt :as cas]
            [czlab.mcfud.p2d.core]
            [czlab.mcfud.cc.ccsx :as cx]
            [czlab.mcfud.cc.boot :as bt]
            [czlab.mcfud.cc.dialog :as dlg]
            [czlab.mcfud.afx.odin :as odin]
            [czlab.mcfud.afx.ecs :as ecs]
            [czlab.mcfud.afx.ebus :as bus]
[czlab.rygel.tictactoe.app :as aaa]
[czlab.rygel.tictactoe.gui :as bbb]
[czlab.rygel.tictactoe.board :as ccc]
[czlab.rygel.tictactoe.core :as zzz]

            [clojure.string :as cs]
            [czlab.mcfud.afx.math
             :as m :refer [vec-neq? vec-eq? vec2 vec3 PI
                           mat-neq? mat-eq? mat* mat2 mat3 mat4]]
            [czlab.mcfud.afx.core
             :as c :refer [deftest if-some+ when-some+
                           ensure-thrown _1 _2 _3
                           defenum var-get var-set
                           with-js-vars runtest ensure?? raise!]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private TMPVAR (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for testing state monad
(defn- mult3 "" [x] (* 3 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add2 "" [x] (+ 2 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- exlog

  "wrapper so that the actual computation is inside
  a state-monadic value, together with the log msg"
  [expr log]

  (fn [s]
    (let [{v :value slog :log} s
          v' (expr v)
          log' (conj slog
                     (str log "(" v ")"))]
      [v' {:value v' :log log'}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenum CRAP A 1 B C D)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-core

  (ensure?? (= 5 (+ CRAP-A CRAP-D)) "enum")

  (ensure?? (= 23
               (let [obj #js {"a" 5
                              "f" (fn [p] (this-as this
                                                   (oset! this "a" (+ p (oget this "a")))))
                              "g" (fn [x y] (this-as this
                                                     (oset! this "a" (+ x y (oget this "a")))))}
                     r (c/jsto obj ["f" 3] ["g" 7 8])]
                 (oget r "a"))) "jsto")

  (ensure?? (= 1 (_1 [1 2])) "1st")
  (ensure?? (= 2 (_2 [1 2])) "2nd")
  (ensure?? (= 3 (_3 [1 2 3])) "3rd")

  (ensure?? (= (c/str->num "1.2") 1.2) "str->num:float")
  (ensure?? (= (c/str->num "12") 12) "str->num:int")

  (ensure?? (= 2 (c/last-index [1 2 3])) "last-index")
  (ensure?? (= 2 (nth (c/cdr [1 2]) 0)) "cdr")
  (ensure?? (= 1 (c/car [1 2])) "car")
  (ensure?? (= 2 (c/nexth [1 2] 0)) "nexth")

  (ensure?? (= {:a 5 :z 9 :b {:c 2 :d 2}}
               (c/deep-merge {:a 1 :b {:c 2}}
                              {:z 9 :a 5 :b {:d 2}})) "deepMerge,map")

  (ensure?? (= {:a 5 :z 9 :b #{ :c 2 :d 3 4}}
               (c/deep-merge {:a 1 :b #{ :c 2 4}}
                              {:z 9 :a 5 :b #{ :d 2 3}})) "deepMerge,set")

  (ensure?? (= {:a 5 :z 9 :b [1 3]}
               (c/deep-merge {:a 1 :b [4]}
                              {:z 9 :a 5 :b [1 3]})) "deepMerge,*")

  (ensure?? (= 3 (c/do-with [a (+ 1 2)]
                             (/ a 3))) "do-with")
  (ensure?? (= false (c/do->false (+ 1 2) (= 1 1))) "do->false")
  (ensure?? (= true (c/do->true (+ 1 2) (= 1 2))) "do->true")
  (ensure?? (= nil (c/do->nil (+ 1 2) 911)) "do->nil")
  (ensure?? (= js/undefined (c/do->undef (+ 1 2) 911)) "do->undef")

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

  (ensure?? (= 3 (with-js-vars [a 3 b 4] (var-get a))) "with-js-vars,var-get")

  (ensure?? (= 6 (with-js-vars [a 3 b 4] (var-set b 6) (var-get b))) "with-js-vars,var-set")

  (ensure?? (= 6 (do (reset! TMPVAR 0)
                     (c/each #(swap! TMPVAR + %) [1 2 3]) (deref TMPVAR))) "each")

  (ensure?? (and (c/nichts? nil)
                 (c/nichts? js/undefined)) "nichts?")

  (ensure?? (= "abc&lt;&gt;&amp;&quot;&apos;xyz"
               (c/esc-xml "abc<>&\"'xyz")) "escXml")

  (ensure?? (let [[x y]
                  (c/split-seq [1 2 3 4 5] 3)]
              (and (= [1 2 3] x)
                   (= [4 5] y))) "splitSeq")

  (ensure?? (= 50 (c/percent 20 40)) "percent")
  (ensure?? (= "3.333" (c/num->fixed (/ 10 3) 3)) "numToFixed")

  (ensure?? (= ["123" "456" "78"]
               (c/split-str 3 "12345678")) "splitStr")

  (ensure?? (= 698 (c/max-by identity [78 7 698 4 5 2 -1])) "maxBy")
  (ensure?? (= -1 (c/min-by identity [78 7 6 4 5 2 -1])) "minBy")

  (ensure?? (= 3 (c/domonad c/m-identity
                             [a 1 b (inc a)] (+ a b))) "identity monad")
  (ensure-thrown "any"
                 (c/domonad c/m-identity
                             [a nil
                              b a
                              c (.toString b)] (+ a b c)) "identity monad->boom")

  (ensure?? (= 3 (c/domonad c/m-maybe
                             [a 1 b (inc a)] (+ a b))) "maybe monad")

  (ensure?? (nil? (c/domonad c/m-maybe
                              [a 1
                               b (inc a)
                               c nil] (+ a b c))) "maybe monad->nil")

  (ensure?? (= [5, {:value 5 :log ["mult3(1)" "add2(3)"]}]
               ((c/domonad c/m-state
                            [c1 (exlog mult3 "mult3")
                             c2 (exlog add2 "add2")]
                            c2)
                {:value 1 :log []})) "state monad")

  (ensure?? (= 3 (c/run-cont
                   (c/domonad c/m-continuation
                               [x ((fn [v] (fn [c] (c v))) 1)
                                y ((fn [v] (fn [c] (c v))) 2)]
                               (+ x y)))) "continuation monad")

  (ensure?? (let [f (fn [v] (fn [s] [v s]))
                  lhs ((:bind c/m-state) ((:unit c/m-state) 911) f)
                  rhs (f 911)
                  lf (lhs "hello")
                  rt (rhs "hello")]
              (and (= (_1 lf)(_1 rt))
                   (= (last lf)(last rt))))
            "monad rule 1: bind(unit(x), f) ≡ f(x)")

  (ensure?? (let [mv (fn [s] [3 s])
                  lhs ((:bind c/m-state) mv (:unit c/m-state))
                  lf (lhs "hello")
                  rt (mv "hello")]
              (and (= (_1 lf)(_1 rt))
                   (= (last lf)(last rt))))
            "monad rule 2: bind(m, unit) ≡ m")

  (ensure?? (let [f (fn [v] (fn [s] [3 s]))
                  g (fn [v] (fn [s] [5 s]))
                  bb (:bind c/m-state)
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

  (ensure-thrown js/Error (raise! "hello" "world") "raise!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test ecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compA "" [] #js{:a 50 })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compB "" [] #js{:b 10 })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compC "" [] #js{:c 15 })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private compDPool (ecs/new-jspool (fn [] #js{:d 10}) identity 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compD "" [] (ecs/pool-take! compDPool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private tmpA {:components [:c1 :c3]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private tmpB {:components [:c2 :c3]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private _ecs_ (ecs/new-entity-store))
(def ^:private TEMP-VAR (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sys1 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sys2 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sys3 "" [e t] (if (pos? t) (swap! TEMP-VAR conj 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private pool1 (ecs/new-jspool (fn [] #js{:a 0}) identity 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-ecs

  (ensure?? (zero? (ecs/pool-size pool1)) "pool,size=0")

  (ensure?? (let [x (ecs/pool-take! pool1)
                  y (ecs/pool-take! pool1)
                  z (ecs/pool-take! pool1)]
              (= 3 (ecs/pool-count pool1))) "pool,used")

  (ensure?? (let [x (ecs/pool-take! pool1)
                  y (ecs/pool-take! pool1)
                  z (ecs/pool-take! pool1)]
              (ecs/pool-drop! pool1 z)
              (= 5 (ecs/pool-count pool1))) "pool,drop")

  (ensure?? (= 6 (ecs/pool-size pool1)) "pool,size>0")

  (ensure?? (let [x (ecs/pool-take! pool1)
                  y (ecs/pool-take! pool1)
                  z (ecs/pool-take! pool1)]
              (ecs/pool-drop! pool1 x)
              (and (= 7 (ecs/pool-count pool1))
                   (= 12 (ecs/pool-size pool1)))) "pool,grow")

  (ensure?? (= 1 (do (ecs/add-component! _ecs_ :c1 [compA identity])
                     (count (ecs/get-component-keys _ecs_)))) "addComponent,1")
  (ensure?? (= 3 (do (ecs/add-component! _ecs_ :c2 [compB identity] :c3 [compC identity])
                     (count (ecs/get-component-keys _ecs_)))) "addComponent,*")

  (ensure?? (= 2 (do (ecs/remove-component! _ecs_ :c1)
                     (count (ecs/get-component-keys _ecs_)))) "removeComponent,1")
  (ensure?? (= 0 (do (ecs/remove-component! _ecs_ :c2 :c3)
                     (count (ecs/get-component-keys _ecs_)))) "removeComponent,*")

  (ensure?? (= 4 (do (ecs/add-component! _ecs_
                                         :c2 [compB]
                                         :c3 [compC]
                                         :c4 [compD]
                                         :c1 [compA identity])
                     (count (ecs/get-component-keys _ecs_)))) "addComponent,**")

  (ensure?? (= 2 (do (ecs/add-template! _ecs_ :t1 tmpA :t2 tmpB)
                     (count (ecs/get-template-keys _ecs_)))) "addTemplate,*")

  (ensure?? (= 0 (do (ecs/remove-template! _ecs_ :t1 :t2)
                     (count (ecs/get-template-keys _ecs_)))) "removeTemplate,*")

  (ensure?? (= 2 (do (ecs/add-template! _ecs_ :t1 tmpA :t2 tmpB)
                     (count (ecs/get-template-keys _ecs_)))) "addTemplate,*")

  (ensure?? (let [x (ecs/new-entity! _ecs_ :c1 :c2 :c3)]
              (and (ecs/component-in-entity? _ecs_ x :c1 :c2 :c3)
                   (ecs/get-entity-data _ecs_ x :c1)
                   (ecs/get-entity-data _ecs_ x :c2)
                   (ecs/get-entity-data _ecs_ x :c3))) "createEntity,getData")

  (ensure?? (let [a (ecs/find-component _ecs_ :c1)
                  b (ecs/find-component _ecs_ :c2)
                  c (ecs/find-component _ecs_ :c3)
                  d (ecs/find-component _ecs_ :xxx)]
              (and a b c (c/nichts? d))) "engine,find")

  (ensure?? (let [x (ecs/new-template-entity _ecs_ :t2)]
              (and (ecs/component-in-entity? _ecs_ x :c2 :c3)
                   (c/nichts? (ecs/get-entity-data _ecs_ x :c1))
                   (ecs/get-entity-data _ecs_ x :c2)
                   (ecs/get-entity-data _ecs_ x :c3))) "newTemplateEntity,getData")

  (ensure?? (let [x (ecs/new-template-entity _ecs_ :t1)
                  y (ecs/get-entity-data _ecs_ x :c3)
                  _ (ecs/remove-entity! _ecs_ x)
                  z (ecs/get-entity-data _ecs_ x :c1)]
              (and (some? y)
                   (c/nichts? z))) "removeEntity")

  (ensure?? (let [x (ecs/new-entity! _ecs_ :c4)
                  ok (and (ecs/component-in-entity? _ecs_ x :c4)
                          (ecs/get-entity-data _ecs_ x :c4))]
              (ecs/remove-entity! _ecs_ x)
              (and ok
                   (not (ecs/component-in-entity? _ecs_ x :c4)))) "createEntity,pooled")

  (ensure?? (= [1 2 3]
               (do (reset! TEMP-VAR [])
                   (ecs/push-system! _ecs_ sys1 sys2 sys3)
                   (ecs/run-all-systems _ecs_ 10)
                   @TEMP-VAR)) "engine,addSystem"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cbbus

  ""
  [& xs]

  (swap! TMPVAR inc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-ebus

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-event-bus)]
              (bus/sub* b 12345 cbbus)
              (bus/pub b 12345 "yo!")
              (bus/pub b 12345 "yo!")
              (= 1 @TMPVAR)) "evbus, sub*(int)")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-event-bus)]
              (bus/sub* b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "evbus, sub*(str)")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-event-bus)]
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "evbus, sub+")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-event-bus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/pause! b s)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (bus/resume! b s)
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "evbus, pause!, resume!")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-event-bus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/unsub! b s)
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "evbus, unsub!")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-tibrv-bus)]
              (bus/sub* b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "rvbus, sub*")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-tibrv-bus)]
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "rvbus, sub+")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-tibrv-bus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/pause! b s)
              (bus/pub b "a.b.c" "yo!")
              (bus/pub b "a.b.c" "yo!")
              (bus/resume! b s)
              (bus/pub b "a.b.c" "yo!")
              (= 2 @TMPVAR)) "rvbus, pause!, resume!")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-tibrv-bus)
                  s (bus/sub+ b "a.b.c" cbbus)]
              (bus/pub b "a.b.c" "yo!")
              (bus/unsub! b s)
              (bus/pub b "a.b.c" "yo!")
              (= 1 @TMPVAR)) "rvbus, unsub!")

  (ensure?? (let [_ (reset! TMPVAR 0)
                  b (bus/new-tibrv-bus)]
              (bus/sub+ b "a.*.>" cbbus)
              (bus/sub+ b "a.*.c" cbbus)
              (bus/sub+ b "a.b.c" cbbus)
              (bus/pub b "a.b.c" "yo!")
              (= 3 @TMPVAR)) "rvbus, sub a.*.b")

  (ensure?? (let [b (bus/new-tibrv-bus) m (deref b)]
              (bus/sub+ b "a.*.>" cbbus)
              (bus/sub+ b "a.*.c" cbbus)
              (bus/sub+ b "a.b.c" cbbus)
              (bus/unsub-all! b)
              (= m @b)) "rvbus, unsubAll!")

  (ensure?? (= 3 (count (deref (bus/new-event-bus)))) "createEvBus")
  (ensure?? (= 3 (count (deref (bus/new-tibrv-bus)))) "createRvBus"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private CAESAR-DATA "hello, bonjour, blah!")
(def ^:private CAESAR-DLEN (count CAESAR-DATA))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-crypt

  (ensure?? (let [s (cas/encrypt CAESAR-DATA 178)]
              (and (= CAESAR-DLEN (count s)) (not= s CAESAR-DATA))) "crypt, encrypt")

  (ensure?? (let [s (cas/encrypt CAESAR-DATA 178)
                  s' (cas/decrypt s 178)] (= s' CAESAR-DATA)) "crypt, decrypt")
  (ensure?? (let [s (cas/encrypt CAESAR-DATA 178)
                  s' (cas/decrypt s 18)] (not= s' CAESAR-DATA)) "crypt, bad decrypt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-math

  (ensure?? (let [[x y] (vec2 1 2)]
              (and (= 1 x)(= 2 y))) "vec2 ctor")

  (ensure?? (let [[x y z] (vec3 1 2 3)]
              (and (= 1 x)(= 2 y)(= 3 z))) "vec3 ctor")

  (ensure?? (vec-neq? (vec3 2 3 0)(vec3 1 2 3)) "v3,v3 neq?")
  (ensure?? (vec-neq? (vec3 1 2 3)(vec2 2 3)) "v3 neq?")
  (ensure?? (vec-neq? (vec2 2 3)(vec3 1 2 3)) "v2 neq?")

  (ensure?? (vec-eq? (vec2 2 3)(vec2 2 3)) "v2 eq?")
  (ensure?? (vec-eq? (vec3 1 2 3)(vec3 1 2 3)) "v3 eq?")

  (ensure?? (vec-eq? (vec3 5 7 9)
                        (m/vec-add (vec3 1 2 3)(vec3 4 5 6))) "vec-add")

  (ensure?? (vec-eq? (vec2 1 3)
                        (m/vec-sub (vec2 2 3)(vec2 1 0))) "vec-sub")

  (ensure?? (vec-eq? (vec2 4 15)
                        (m/vec-mult (vec2 2 3)(vec2 2 5))) "vec-mult")

  (ensure?? (vec-eq? (vec2 3 4)
                        (m/vec-div (vec2 9 8)(vec2 3 2))) "vec-div")

  (ensure?? (vec-eq? (vec2 27 24)
                        (m/vec-scale (vec2 9 8) 3 )) "vec-scale")
  (ensure?? (vec-eq? (vec3 27 24 -21)
                        (m/vec-scale (vec3 9 8 -7) 3 )) "vec-scale")

  (ensure?? (vec-eq? (vec2 6 5)
                        (m/vec-minus (vec2 9 8) 3 )) "vec-minus")
  (ensure?? (vec-eq? (vec2 12 11)
                        (m/vec-plus (vec2 9 8) 3 )) "vec-plus")

  (ensure?? (= -42 (m/vec-dot (vec2 -9 8)(vec2 2 -3))) "v2-dot")
  (ensure?? (= 49 (m/vec-dot (vec3 9 8 -7)(vec3 -2 4 -5))) "v3-dot")

  (ensure?? (= 11 (m/vec-xss (vec2 -9 8)(vec2 2 -3))) "v2-xss")
  (ensure?? (vec-eq? (vec3 -12 59 52)
                        (m/vec-xss (vec3 9 8 -7)(vec3 -2 4 -5))) "v3-xss")

  (ensure?? (= 14 (m/vec-lensq (vec3 1 2 -3))) "v3-lensq")
  (ensure?? (= 13 (m/vec-lensq (vec2 2 3))) "v2-lensq")
  (ensure?? (= 5 (m/vec-len (vec2 3 4))) "vec-len")

  (ensure?? (= 116 (m/vec-distsq (vec3 7 6 5)(vec3 1 2 -3))) "v3-distsq")
  (ensure?? (= 202 (m/vec-distsq (vec2 2 3)(vec2 -9 -6))) "v2-distsq")

  (ensure?? (vec-eq? (vec2 0.6 0.8)
                        (m/vec-unit (vec2 3 4))) "vec-unit")

  (ensure?? (vec-eq? (vec2 -4 3)
                        (m/vec-rot (vec2 3 4) (/ PI 2))) "v2-rot")

  (ensure?? (= 90 (m/rad->deg
                    (m/vec-angle (vec2 3 4)(vec2 -4 3)))) "vec-angle")

  (ensure?? (vec-eq? (vec2 3 -4)
                        (m/vec-neg (vec2 -3 4))) "v2-neg")

  (ensure?? (vec-eq? (vec2 -4 3)
                        (m/vec-normal (vec2 3 4) :left)) "vec-normal")
  (ensure?? (vec-eq? (vec2 4 -3)
                        (m/vec-normal (vec2 3 4))) "vec-normal")

  (ensure?? (vec-eq? (vec2 -1 4)
                        (m/vec-min (vec2 5 4)(vec2 -1 7))) "vec-min")
  (ensure?? (vec-eq? (vec3 9 3 4)
                     (m/vec-max (vec3 8 -3 4)(vec3 9 3 0))) "vec-max")

  (ensure?? (mat-eq? (mat3 1 0 0 0 1 0 0 0 1)
                     (m/mat-identity 3)) "mat-identity")

  (ensure?? (mat-eq? (mat3 0 0 0 0 0 0 0 0 0)
                     (m/mat-zero 3)) "mat-zero")

  (ensure?? (mat-neq? (mat3 1 0 0 0 1 0 1 0 1)
                      (mat3 1 0 1 0 1 0 0 0 1)) "mat-neq?")

  (ensure?? (mat-eq? (mat2 1 0 0 1)
                     (m/mat-identity 2)) "mat2")

  (ensure?? (mat-eq? (mat3 1 0 0 0 1 0 0 0 1)
                     (m/mat-identity 3)) "mat3")

  (ensure?? (mat-eq? (m/mat4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
                     (m/mat-identity 4)) "mat4")

  (ensure?? (mat-eq? (mat* [3 2] 1 4 2 5 3 6)
                     (m/mat-xpose (mat* [2 3] 1 2 3 4 5 6))) "mat-xpose")

  (ensure?? (mat-eq? (mat3 -4 -8 -4 -6 -12 -6 -2 -4 -2)
                     (m/mat-minor (mat3 1 2 3 3 4 5 7 8 9))) "mat-minor")

  (ensure?? (mat-eq? (mat3 -4 8 -4 6 -12 6 -2 4 -2)
                     (m/mat-cofactor (mat3 1 2 3 3 4 5 7 8 9))) "mat-cofactor")

  (ensure?? (= -2 (m/mat-det (mat2 1 2 3 4))) "mat-det")
  (ensure?? (= -64 (m/mat-det (mat3 1 2 3 3 4 5 7 -8 9))) "mat-det")

  (ensure?? (mat-eq? (mat* [2 3]
                           -12 10 8 -6 -4 2)
                     (m/mat-scale
                       (mat* [2 3] 6 -5 -4 3 2 -1) -2)) "mat-scale")

  (ensure?? (mat-eq? (mat3 12 9 6 30 23 16 48 37 26)
                     (m/mat-mult (mat* [3 2] 1 2 3 4 5 6)
                                 (mat* [2 3] 6 5 4 3 2 1))) "mat-mult")

  (ensure?? (mat-eq? (mat3 93 42 3 -6 -30 18 67 6 -13)
                     (m/mat-adjugate
                       (mat3 1 2 3 4 -5 -6 7 8 -9))) "mat-adjugate")

  (ensure?? (mat-eq? (mat3 (/ 31 94) (/ 7 47) (/ 1 94)
                           (/ -1 47) (/ -5 47) (/ 3 47)
                           (/ 67 282) (/ 1 47) (/ -13 282))
                     (m/mat-inverse
                       (mat3 1 2 3 4 -5 -6 7 8 -9))) "mat-inverse")

  (ensure?? (mat-eq? (mat2 0.6 -0.7 -0.2 0.4)
                     (m/mat-inverse (mat2 4 7 2 6))) "mat-inverse")

  (ensure?? (mat-eq? (mat4 1 0 0 0 0 1 0 0 0 0 1 0 4 7 2 1)
                     (m/mat4-txlate (vec3 4 7 2))) "mat4-txlate")

  (ensure?? (vec-eq? (vec3 4 7 2)
                     (m/get-translation
                       (mat4 1 0 0 0 0 1 0 0 0 0 1 0 4 7 2 1))) "getTranslation")

  (ensure?? (mat-eq? (mat4 4 7 2 0 5 6 7 0 9 0 3 0 0 0 0 1)
                     (m/mat-fromMX (mat3 4 7 2 5 6 7 9 0 3))) "mat-fromMX")

  (ensure?? (mat-eq? (mat4 4 0 0 0 0 7 0 0 0 0 2 0 0 0 0 1)
                     (m/mat-fromVX (vec3 4 7 2))) "mat-fromVX")

  (ensure?? (vec-eq? (vec3 4 7 2)
                     (m/get-scale-fromMX
                       (mat4 4 0 0 0 0 7 0 0 0 0 2 0 0 0 0 1))) "getScaleFromMX")

  (ensure?? (vec-eq? (vec2 -23 42)
                     (m/mat-vmult (mat2 3 -5 7 2) (vec2 4 7))) "mat-vmult")

  (ensure?? (mat-eq? (mat2 0 -1 1 0)
                     (m/rotation2x2 (/ PI 2))) "rotation2x2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(js/console.log (runtest test-core "mcfud test-core"))
(js/console.log (runtest test-math "mcfud test-math"))
(js/console.log (runtest test-crypt "mcfud test-crypt"))
(js/console.log (runtest test-ebus "mcfud test-ebus"))
(js/console.log (runtest test-ecs "mcfud test-ecs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


