;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.core

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 car defmonad]])
  (:require [clojure.string :as cs]
            [goog.string :as gs]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn raise! "" [& args] (throw (js/Error. (apply str args ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-string "" [n x] (gs/repeat x n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toFloat "A string into float." [x] (gs/toNumber x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toInt "A string into int." [x] (gs/toNumber x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?
  "True if object is
  either null of undefined"
  [obj] (or (undefined? obj) (nil? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn escXml "Escape XML special chars" [s]
  (let [out (atom [])]
    (doseq [c s]
      (swap! out
             conj
             (cond (= c "&") "&amp;"
                   (= c ">") "&gt;"
                   (= c "<") "&lt;"
                   (= c "\"") "&quot;"
                   (= c "'") "&apos;" :else c)))
    (cs/join "" @out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-seq "Split a collection into 2 parts" [coll cnt]
  (if (< cnt (count coll))
    [(take cnt coll) (drop cnt coll)] [(concat [] coll) []]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent "" [numerator denominator] (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toFixed "" [num & [digits]]
  (ocall (js/Number. num) "toFixed"  (or digits 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-str

 "Returns a sequence of strings of n characters each."
  [n string]

  (map #(cs/join "" %) (partition-all n string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn minBy "" [func coll]
  (if (not-empty coll)
    (reduce (fn [a b]
              (if (< (func a) (func b)) a b))
            (_1 coll) (rest coll))
    js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn maxBy "" [func coll]
  (if (not-empty coll)
    (reduce (fn [a b] (if (< (func a) (func b)) b a))
            (car coll) (rest coll))
    js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensureTest "" [cnd msg]
  (try (str (if cnd "passed:" "FAILED:") " " msg)
       (catch js/Error e (str "FAILED: " msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensureTestThrown "" [expected error msg]
  (if (nil? error)
    (str "FAILED: " msg)
    (cond
      (string? expected)
      (if (or (= expected "any")
              (= expected error))
        (str "passed: " msg)
        (str "FAILED: " msg))
      (instance? expected error)
      (str "passed: " msg)
      :else
      (str "FAILED: " msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runtest "" [test & [title]]
  (let
    [title' (or title "test")
     now (system-time)
     results (test)
     sum (count results)
     ok (count (filter #(cs/starts-with? % "p") results))]
    (->> [(repeat-string 78 "+")
          title now
          (repeat-string 78 "+")
          (cs/join "\n" results)
          (cs/join "" (repeat-string 78 "="))
          (str "Passed: " ok "/" sum " [" (int (* 100 (/ ok sum))) "%]")
          (str "Failed: " (- sum ok))
          (str "CPU Time: " (- (system-time) now) "ms")]
         (cs/join "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-identity

  "Monad describing plain computations. This monad does in fact nothing
  at all. It is useful for testing, for combination with monad
  transformers, and for code that is parameterized with a monad."

  [:bind (fn [mv mf] (mf mv)) :unit (fn [x] x)] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-maybe

  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."

  [ :bind (fn [mv mf]
            (if-not (nil? mv) (mf mv))) :unit (fn [x] x) :zero nil] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-list

  "Monad describing multi-valued computations, i.e. computations
  that can yield multiple values. Any object implementing the seq
  protocol can be used as a monadic value."

  [ :bind (fn [mv mf] (flatten (map mf mv)))
    :unit (fn [x] (concat [] x))
    :zero []
    :plus (fn [& xs] (flatten xs))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-state

  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."

  [ :bind (fn [mv mf]
          (fn [state]
            (let [[value newState] (mv state)]
              ((mf value) newState))))
   :unit (fn [v] (fn [x] [v x]))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-continuation

  "Monad describing computations in continuation-passing style. The monadic
  values are functions that are called with a single argument representing
  the continuation of the computation, to which they pass their result."

  [ :bind (fn [mv mf]
          (fn [cont]
            (mv (fn [value] ((mf value) cont)))))
    :unit (fn [value] (fn [cont] (cont value))) ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-cont
  "Execute the computation cont
  in the cont monad and return its result."
  [cont]
  (cont identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

(defn compA "" [] #js{:a 50 })
(defn compB "" [] #js{:b 10 })
(defn compC "" [] #js{:c 15 })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def compDPool
        (ecs/createPool
          (fn [] #js{:d 10}) (fn [x] x) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compD "" []
  (ecs/takeFromPool! compDPool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def tmpA
  {:components [[:c1] [:c3]] :initor (fn []) })

(def tmpB
  {:components [[:c2] [:c3]] :initor (fn []) })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(const- *ecs* (ecs/createECS))
(def- TEMP-VAR nil)

(defn- sys1 "" [e t] (if (pos? t) (conj! TEMP-VAR 1)))
(defn- sys2 "" [e t] (if (pos? t) (conj! TEMP-VAR 2)))
(defn- sys3 "" [e t] (if (pos? t) (conj! TEMP-VAR 3)))

(const- pool1 (ecs/createPool (fn [] `{:a 0})
                              (fn [x] x) 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest ecs-test

  (ensure (zero? (ecs/getPoolSize pool1)) "pool,size=0")
  (ensure (let [x (ecs/takeFromPool pool1)
                y (ecs/takeFromPool pool1)
                z (ecs/takeFromPool pool1)]
            (= 3 (ecs/getPoolUsed pool1))) "pool,used")
  (ensure (let [x (ecs/takeFromPool pool1)
                y (ecs/takeFromPool pool1)
                z (ecs/takeFromPool pool1)]
            (ecs/returnToPool pool1 z)
            (= 5 (ecs/getPoolUsed pool1))) "pool,drop")
  (ensure (= 6 (ecs/getPoolSize pool1)) "pool,size>0")
  (ensure (let [x (ecs/takeFromPool pool1)
                y (ecs/takeFromPool pool1)
                z (ecs/takeFromPool pool1)]
            (ecs/returnToPool pool1 x)
            (and (= 7 (ecs/getPoolUsed pool1))
                 (= 12 (ecs/getPoolSize pool1)))) "pool,grow")

  (ensure (= 1 (do (ecs/addComponent *ecs* :c1 compA)
                   (count (ecs/getComponentKeys *ecs*)))) "addComponent,1")
  (ensure (= 3 (do (ecs/addComponent *ecs* :c2 compB :c3 compC)
                   (count (ecs/getComponentKeys *ecs*)))) "addComponent,*")

  (ensure (= 2 (do (ecs/removeComponent *ecs* :c1)
                   (count (ecs/getComponentKeys *ecs*)))) "removeComponent,1")
  (ensure (= 0 (do (ecs/removeComponent *ecs* :c2 :c3)
                   (count (ecs/getComponentKeys *ecs*)))) "removeComponent,*")

  (ensure (= 4 (do (ecs/addComponent *ecs* :c1 compA :c2 compB :c3 compC :c4 compD)
                   (count (ecs/getComponentKeys *ecs*)))) "addComponent,**")

  (ensure (= 2 (do (ecs/addTemplate *ecs* :t1 tmpA :t2 tmpB)
                   (count (ecs/getTemplateKeys *ecs*)))) "addTemplate,*")

  (ensure (= 0 (do (ecs/removeTemplate *ecs* :t1 :t2)
                   (count (ecs/getTemplateKeys *ecs*)))) "removeTemplate,*")

  (ensure (= 2 (do (ecs/addTemplate *ecs* :t1 tmpA :t2 tmpB)
                   (count (ecs/getTemplateKeys *ecs*)))) "addTemplate,*")

  (ensure (let [x (ecs/createEntity *ecs* [:c1] [:c2] [:c3])]
            (and (ecs/componentInEntity? *ecs* x :c1 :c2 :c3)
                 (ecs/getEntityData *ecs* x :c1)
                 (ecs/getEntityData *ecs* x :c2)
                 (ecs/getEntityData *ecs* x :c3))) "createEntity,getData")

  (ensure (let [a (ecs/findComponent *ecs* :c1)
                b (ecs/findComponent *ecs* :c2)
                c (ecs/findComponent *ecs* :c3)
                d (ecs/findComponent *ecs* :xxx)]
            (and a b c (nichts? d))) "engine,find")

  (ensure (let [x (ecs/createTemplateEntity *ecs* :t2)]
            (and (ecs/componentInEntity? *ecs* x :c2 :c3)
                 (nichts? (ecs/getEntityData *ecs* x :c1))
                 (ecs/getEntityData *ecs* x :c2)
                 (ecs/getEntityData *ecs* x :c3))) "createTemplateEntity,getData")

  (ensure (let [x (ecs/createTemplateEntity *ecs* :t1)
                y (ecs/getEntityData *ecs* x :c3)
                _ (ecs/removeEntity *ecs* x)
                z (ecs/getEntityData *ecs* x :c1)]
            (and (some? y)
                 (nichts? z))) "removeEntity")

  (ensure (let [x (ecs/createEntity *ecs* [:c4])
                ok (and (ecs/componentInEntity? *ecs* x :c4)
                        (ecs/getEntityData *ecs* x :c4))]
            (ecs/removeEntity *ecs* x)
            (and ok
                 (not (ecs/componentInEntity? *ecs* x :c4)))) "createEntity,pooled")

  (ensure (ky/eq? [1 2 3]
                 (do (set! TEMP-VAR [])
                     (ecs/addSystem *ecs* sys1 sys2 sys3)
                     (ecs/updateECS *ecs* 10)
                     TEMP-VAR)) "engine,addSystem"))


