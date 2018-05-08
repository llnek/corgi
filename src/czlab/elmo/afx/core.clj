;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.core)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro f#* "" [& forms] `(fn [& ~'____xs] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro trye!
  "Wrap try around code and eat exception."
  [& xs]
  `(try ~@xs (catch ~'js/Error ~'e1 nil) (catch ~'js/Object ~'e2 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n# "" [c] `(count ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1 "Refer to first." [x] `(first ~x))
(defmacro _2 "Refer to second." [x] `(second ~x))
(defmacro _3 "Refer to 3rd." [x] `(nth ~x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car "Refer to first." [x] `(first ~x))
(defmacro cdr "Refer to rest." [x] `(rest ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro toStr "Call toString." [n] `(.toString ~n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro numStr "" [n] `(toStr (js/Number ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro last-index "Length -1." [coll] `(- (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth "Get the next item after i." [coll i] `(nth ~coll (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with
  "varbinding=> symbol init-expr
  Executes the body in a context in which the symbol is always the
  returned value."
  [bindings & xs]
  (let [sz (count bindings)
        _ (assert (= sz 2)
                   "expected only 2 in form")
        f (first bindings)]
    `(let [~f ~(second bindings)] ~@xs ~f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->false
  "Executes the exprs in a context in which false is always the returned value."
  [& xs] `(do ~@xs false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->true
  "Executes the exprs in a context in which true is always the returned value."
  [& xs] `(do ~@xs true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->nil
  "Executes the exprs in a context in which null is always the returned value."
  [& xs] `(do ~@xs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->undef
  "Executes the exprs in a context in which undefined is always the returned value."
  [& xs] `(do ~@xs js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+
  "bindings => binding-form test
  If test is not empty, evaluates then with binding-form bound to the
  value of test, if not, yields else"
  [bindings then & [else]]
  (let [sz (count bindings)
        _ (assert (= 2 sz)
                  "expected binary form")
        tst (gensym)]
    `(let [~tst ~(second bindings)
           ~(first bindings) ~tst]
       (if (> (count ~tst) 0) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+
  "bindings => binding-form test
  When test is not empty, evaluates body with binding-form bound to the
  value of test"
  [bindings & xs]
  (let [sz (count bindings)
        _ (assert (= 2 sz)
                  "expected binary form")
        tst (gensym)]
    `(let [~tst ~(second bindings)
           ~(first bindings) ~tst]
       (when (> (count ~tst) 0) ~@xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each
  "Executes a provided function once for each array element."
  [func coll]
  (let [x (gensym)]
    `(doseq [~x ~coll] (~func ~x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro monad

  "Define a monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."

  [docstring operations]

  `(merge {:bind nil
           :unit nil
           :zero nil
           :plus nil}
          (into {} (vec (map #(vec %) (partition 2 ~operations))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad

  "Define a named monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."

  [name & [docs operations]]

  (let [ds (if (string? docs) docs "")
        ps (if (string? docs)
             operations
             (if (vector? docs) docs))
        _ (assert (vector? ps)
                  "no macro operations")]
    `(def ~name (monad ~ds ~ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dobind "" [mbind steps expr]
  (let
    [mv (second steps)
     a1 (first steps)
     more (rest (rest steps))]
    `(~mbind ~mv
             (fn [~a1]
               ~(if (not-empty more)
                  `(dobind ~mbind ~more ~expr) `(do ~expr))))))

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
  "" [name & body] `(def ~name (fn [] [~@body])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??
  "" [form msg] `(czlab.elmo.afx.core/ensureTest ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensureThrown
  "" [expected form msg]
  `(try ~form
        (czlab.elmo.afx.core/ensureTestThrown ~expected nil ~msg)
        (catch js/Error ~'e
          (czlab.elmo.afx.core/ensureTestThrown ~expected ~'e ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


