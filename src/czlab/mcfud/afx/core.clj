;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.core

  (:refer-clojure :exclude [with-local-vars var-get var-set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defvoid-

  "Void function (private)."
  [name doc args & body]

  `(defn- ~name ~doc [~@args] ~@body nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defvoid

  "Void function."
  [name doc args & body]

  `(defn ~name ~doc [~@args] ~@body nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro clog

  "Log to console."
  [msgs]

  `(js/console.log (apply str ~msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _2

  "Refer to second."
  [x]

  `(second ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1

  "Refer to first."
  [x]

  `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _3

  "Refer to 3rd."
  [x]

  `(nth ~x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _E

  "Refer to last."
  [x]

  `(last ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nzero?

  "Not zero?"
  [n]

  `(not (zero? ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n#

  "Count size of collection."
  [c]

  `(count ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car

  "Refer to first."
  [x]

  `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cdr

  "Refer to rest."
  [x]

  `(rest ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dissoc!!

  "Mutable dissoc (atom)."
  [a & args]

  (let [X (gensym)]
    `(swap! ~a (fn [~X] (dissoc ~X ~@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assoc!!

  "Mutable assoc (atom)."
  [a & args]

  (let [X (gensym)]
    `(swap! ~a (fn [~X] (assoc ~X ~@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro var-set

  "Set a javascript var."
  [p v]

  (let [y (symbol "____vars")
        x (symbol (str ".-" (name p)))] `(set! (~x ~y) ~v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro var-get

  "Get a javascript var."
  [p]

  (let [y (symbol "____vars")
        x (symbol (str ".-" (name p)))] `(~x ~y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-local-vars

  "A code block supporting js objects as vars."
  [bindings & more]

  (let [X (->> (partition 2 bindings)
               (mapcat (fn [[n v]] [(name n) v])))]
    `(let [~'____vars (cljs.core/js-obj ~@X)] ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro f#*

  "Wrap code into a function which takes in var-args."
  [& forms]

  `(fn [& ~'____xs] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro half**

  "Half all the arguments."
  [& args]

  `(mapv #(* % 0.5) [~@args]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro half*

  "Half the argument."
  [x]

  `(* 0.5 ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mapScalarOp

  "Apply the binary math-op to the value and the forms."
  [op v & forms]

  `(mapv #(~op % ~v) [~@forms]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro js-prop?

  "True if js object has this property."
  [obj prop]

  `(goog.object/containsKey ~obj ~prop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!

  "Wrap try around code and eat exception."
  [& xs]

  `(try ~@xs
        (catch ~'js/Error ~'e1 nil)
        (catch ~'js/Object ~'e2 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro last-index

  "Length -1."
  [coll]

  `(- (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth

  "Get the next item after i."
  [coll i]

  `(nth ~coll (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with

  "varbinding=> symbol init-expr
  Executes the body in a context in which the symbol is always the
  returned value."
  [bindings & xs]

  (let [sz (n# bindings)
        _ (assert (or (= sz 1)
                      (= sz 2))
                  "too many in binding")
        f (_1 bindings)]
    (if (= sz 1)
      `(let [~f ~f] ~@xs ~f)
      `(let [~f ~(_2 bindings)] ~@xs ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->false

  "Executes the exprs in a context
  in which false is always the returned value."
  [& xs]

  `(do ~@xs false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->true

  "Executes the exprs in a context
  in which true is always the returned value."
  [& xs]

  `(do ~@xs true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->nil

  "Executes the exprs in a context
  in which null is always the returned value."
  [& xs]

  `(do ~@xs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->undef

  "Executes the exprs in a context
  in which undefined is always the returned value."
  [& xs]

  `(do ~@xs js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+

  "bindings => binding-form test
  When test is not empty, evaluates body with binding-form bound to the
  value of test"
  [bindings & xs]

  (let [sz (n# bindings)
        tst (gensym)
        _ (assert (= 2 sz) "too many")]
    `(let [~tst ~(_2 bindings)
           ~(_1 bindings) ~tst] (when (> (count ~tst) 0) ~@xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+

  "bindings => binding-form test
  If test is not empty, evaluates then with binding-form bound to the
  value of test, if not, yields else"
  [bindings then & [else]]

  (let [sz (n# bindings)
        tst (gensym)
        _ (assert (= 2 sz) "too many")]
    `(let [~tst ~(_2 bindings)
           ~(_1 bindings) ~tst] (if (> (count ~tst) 0) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number

  "bindings => binding-form test
  If test is a number, evaluates then with binding-form
  bound to the value of test."
  [bindings then]

  (let [sz (n# bindings)
        tst (gensym)
        _ (assert (= 2 sz) "too many")]
    `(let [~tst ~(_2 bindings)
           ~(_1 bindings) ~tst] (if (number? ~tst) ~then))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*

  "Executes a provided function once for each array element, indexed."
  [func coll]

  (let [c (gensym)
        t (gensym)
        i (gensym)]
    `(let [~c ~coll ~t (count ~c)]
       (dotimes [~i ~t] (~func (nth ~c ~i) ~i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each

  "Executes a provided function once for each array element."
  [func coll]

  (let [x (gensym)]
    `(doseq [~x ~coll] (~func ~x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private monad

  "Define a monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."
  [docstring operations]

  `(merge {:bind nil
           :unit nil
           :zero nil
           :plus nil}
          (into {} (mapv #(vec %) (partition 2 ~operations)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private dobind

  "Run the bind operator."
  [mbind steps expr]

  (let [[a1 mv] (take 2 steps)
        more (drop 2 steps)]
    `(~mbind ~mv
             (fn [~a1]
               ~(if (not-empty more)
                  `(dobind ~mbind ~more ~expr) `(do ~expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad

  "Define a named monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."
  [name & [docs operations]]

  (let [[ds ps]
        (cond (string? docs) [docs operations]
              (vector? docs) ["" docs]
              :else nil)]
    (assert (not-empty ps)
            "no monad ops")
    `(def ~name (monad ~ds ~ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro domonad

  "Monad comprehension. Takes the name of a monad, a vector of steps
   given as binding-form, and a result value
   specified by body."
  [monad steps body]

  (let [ret (gensym)]
    `((fn [~'mo]
        (let
          [~ret #(if (and (not (some? %))
                          (not (nil?  (get ~'mo :zero))))
                   (get ~'mo :zero)
                   ((get ~'mo :unit) %))]
          (dobind (get ~'mo :bind) ~steps (~ret ~body)))) ~monad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftest

  "Define a test group."
  [name & body]

  `(def ~name (fn [] (filter #(not (nil? %)) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??

  "Assert that this test ok."
  [form msg]

  `(czlab.mcfud.afx.core/ensureTest ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensureThrown

  "Assert that this error was thrown."
  [expected form msg]

  `(try ~form
        (czlab.mcfud.afx.core/ensureTestThrown ~expected nil ~msg)
        (catch ~'js/Error ~'e
          (czlab.mcfud.afx.core/ensureTestThrown ~expected ~'e ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


