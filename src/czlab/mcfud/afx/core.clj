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

  (:refer-clojure :exclude [var-get var-set]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro debug* "Log to console." [& msgs] `(js/console.log (str ~@msgs)))
(defmacro warn* "Log to console." [& msgs] `(js/console.log (str ~@msgs)))
(defmacro info* "Log to console." [& msgs] `(js/console.log (str ~@msgs)))
(defmacro error* "Log to console." [& msgs] `(js/console.log (str ~@msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defenum
  "Enum definition. e.g. (defenum xyz a 1 b c) will
  generate
  (def xyz-a 1)
  (def xyz-b 2)
  (def xyz-c 3)"

  [name_ & args]
  (let [[e1 n] (take 2 args)
        more (concat [e1] (drop 2 args))]
    (assert (number? n) "expecting a number")
    `(do ~@(loop [v n [m & ms] more out []]
            (if (nil? m)
              out
              (recur (+ 1 v)
                     ms
                     (conj out
                           `(def ~(symbol (str (name name_)
                                               "-" (name m))) ~v))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(defmacro defv
  "Void function - returns nil always."
  [name doc args & body]
  `(defn ~name ~doc [~@args] ~@body nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro not-nil? [x] `(not (nil? ~x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro atom? [x] `(instance? ~'cljs.core/Atom ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro is?
  "Alias for instance?"
  [& more] `(instance? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro in?
  "Alias for contains?"
  [& more] `(contains? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro =?
  "Alias for identical?"
  [& more] `(identical? ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro map->
  "Alias for into {} ..."
  [& more] `(into {} ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+1
  "Alias for concat"
  [a & more] `(concat [~a] ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc+
  "Alias for concat"
  [& more] `(concat ~@more))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _2 "Alias for second." [x] `(second ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _1 "Alias for first." [x] `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _3 "Alias for 3rd." [x] `(nth ~x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro _E "Alias for last." [x] `(last ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nzero? "Not zero?" [n] `(not (zero? ~n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n# "Alias for count." [c] `(count ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro car "Alias for first." [x] `(first ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cdr "Alias for rest." [x] `(rest ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro dissoc!!
  "Mutable dissoc (atom)."
  [a & args]
  `(swap! ~a #(dissoc % ~@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro assoc!!
  "Mutable assoc (atom)."
  [a & args]
  `(swap! ~a #(assoc % ~@args)))

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
(defmacro with-js-vars
  "A code block supporting js-objects as vars."
  [bindings & more]
  (let [X (->> (partition 2 bindings)
               (mapcat (fn [[n v]] [(name n) v])))]
    `(let [~'____vars (cljs.core/js-obj ~@X)] ~@more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_*
  "Wrap code into a function which takes in var-args."
  [& forms]
  `(fn [& ~'____xs] ~@forms))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_3
  "Wrap code into a function which takes in 3 arg."
  [& forms]
  `(fn [~'____1 ~'____2 ~'____3] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_2
  "Wrap code into a function which takes in 2 arg."
  [& forms]
  `(fn [~'____1 ~'____2] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_1
  "Wrap code into a function which takes in 1 arg."
  [& forms]
  `(fn [~'____1] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro fn_0
  "Wrap code into a function which takes in 0 args."
  [& forms]
  `(fn [] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro vtbl*
  "Find this method in a vtable and then run it."
  [vtbl method & args]
  (let [X (gensym)]
    `(czlab.mcfud.afx.core/if-func [~X (get ~vtbl ~method)] (~X ~@args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tmap*
  "New transient map." [& [x]] `(transient (or ~x {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro tvec*
  "New transient vector." [& [x]] `(transient (or ~x [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro pert!
  "Alias for persistent!." [c] `(persistent! ~c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mapfv
  "Apply a binary-op to the value over the forms."
  [op v & forms]
  ;`(mapv #(~op % ~v) [~@forms]))
  `(vector ~@(map (fn [f] `(~op ~f ~v)) forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro js-prop?
  "If js-object has this property."
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
  "Length - 1."
  [coll] `(- (count ~coll) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nexth
  "The next item after i."
  [coll i] `(nth ~coll (+ 1 ~i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defmacro fnjs [args & forms] `(fn [~@args] (cljs.core/this-as ~'this ~@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro jsto
  "Like doto but for js-object, calls methods on the object.
  e.g. (jsto obj [f1 1 2 3] [f2 4 5 6] ...)
  generates
  (oops.core/ocall!+ obj f1 1 2 3)
  (oops.core/ocall!+ obj f2 4 5 6)"
  [obj & calls]
  `(do ~@(map #(let [[f & args] %1]
                 `(~'oops.core/ocall!+ ~obj ~f ~@args)) calls) ~obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-atom
  "varbinding=> symbol init-expr
  Eval the body in a context in which the symbol is always the
  returned value deref'ed.
  e.g. (do-with-atom [a (atom 0)] ... (deref a))"
  [bindings & xs]
  (let [sz (count bindings)
        _ (assert (or (= sz 1)
                      (= sz 2))
                  "too many in bindings")
        f (first bindings)]
    (if (= sz 1)
      `(let [~f ~f] ~@xs (deref ~f))
      `(let [~f ~(_2 bindings)] ~@xs (deref ~f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with-transient
  "varbinding=> symbol init-expr
  Evals the body in a context in which the symbol is always the
  returned value with persistent! applied."
  [bindings & xs]
  (let [sz (count bindings)
        _ (assert (or (= sz 1)
                      (= sz 2))
                  "too many in bindings")
        f (first bindings)]
    (if (= sz 1)
      `(let [~f ~f] ~@xs (persistent! ~f))
      `(let [~f ~(_2 bindings)] ~@xs (persistent! ~f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do-with
  "varbinding=> symbol init-expr
  Evals the body in a context in which the symbol is always the
  returned value."
  [bindings & xs]
  (let [sz (count bindings)
        _ (assert (or (= sz 1)
                      (= sz 2))
                  "too many in bindings")
        f (first bindings)]
    (if (= sz 1)
      `(let [~f ~f] ~@xs ~f)
      `(let [~f ~(_2 bindings)] ~@xs ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->false
  "Evals forms, returns false."
  [& forms] `(do ~@forms false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->true
  "Evals forms, returns true."
  [& forms] `(do ~@forms true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->nil
  "Evals forms, returns nil."
  [& forms] `(do ~@forms nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let->nil
  "Like let, return nil." [& args] `(let ~@args nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro do->undef
  "Evals forms, returns undefined."
  [& forms] `(do ~@forms js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro when-some+
  "bindings => binding-form test
  When test is not empty, evaluates body with binding-form bound to the
  value of test"
  [bindings & forms]
  (let [sz (count bindings)
        X (gensym)
        _ (assert (= 2 sz) "too many in bindings")]
    `(let [~X ~(_2 bindings)
           ~(_1 bindings) ~X] (when (> (count ~X) 0) ~@forms))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-some+
  "bindings => binding-form test
  If test is not empty, evaluates then with binding-form bound to the
  value of test, if not, yields else"
  [bindings then & [else]]
  (let [sz (count bindings)
        X (gensym)
        _ (assert (= 2 sz) "too many in bindings")]
    `(let [~X ~(_2 bindings)
           ~(_1 bindings) ~X] (if (> (count ~X) 0) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-number
  "bindings => binding-form test
  If test is a number, evaluates then with binding-form
  bound to the value of test."
  [bindings then & [else]]
  (let [sz (count bindings)
        X (gensym)
        _ (assert (= 2 sz) "too many in bindings")]
    `(let [~X ~(_2 bindings)
           ~(_1 bindings) ~X] (if (number? ~X) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-string
  "bindings => binding-form test
  If test is a string, evaluates then with binding-form
  bound to the value of test."
  [bindings then & [else]]
  (let [sz (count bindings)
        X (gensym)
        _ (assert (= 2 sz) "too many in bindings")]
    `(let [~X ~(_2 bindings)
           ~(_1 bindings) ~X] (if (string? ~X) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro if-func
  "bindings => binding-form test
  If test is a function, evaluates then with binding-form
  bound to the value of test."
  [bindings then & [else]]
  (let [sz (count bindings)
        X (gensym)
        _ (assert (= 2 sz) "too many in bindings")]
    `(let [~X ~(_2 bindings)
           ~(_1 bindings) ~X] (if (fn? ~X) ~then ~else))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro n-times
  "A simpler dotimes. loop n times."
  [n & forms] `(dotimes [~'_ ~n] ~@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each*
  "Evals function for each element, indexed."
  [func coll]
  (let [C (gensym)
        T (gensym)
        I (gensym)]
    `(let [~C ~coll ~T (count ~C)]
       (dotimes [~I ~T] (~func (nth ~C ~I) ~I)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro each
  "Evals function for each element."
  [func coll]
  (let [X (gensym)] `(doseq [~X ~coll] (~func ~X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private run-bind
  "Run the bind operator."
  [binder steps expr]
  (let [[a1 mv] (take 2 steps) more (drop 2 steps)]
    `(~binder ~mv
              (fn [~a1]
                ~(if (not-empty more)
                   `(run-bind ~binder ~more ~expr) `(do ~expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defmonad
  "Define a named monad by defining the monad operations. The definitions
   are written like bindings to the monad operations bind and
   unit (required) and zero and plus (optional)."
  [name & [docs ops]]
  (let [[ds ps] (cond
                  (string? docs) [docs ops]
                  (vector? docs) ["" docs] :else nil)]
    (assert (not-empty ps) "no monad ops!")
    `(def ~name (merge {:bind nil
                        :unit nil
                        :zero nil
                        :plus nil}
                       (into {} (map #(vec %) (partition 2 ~ops)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro domonad
  "Monad comprehension. Takes the name of a monad, a vector of steps
   given as binding-form, and a result value
   specified by body."
  [monad steps & [body]]
  (let [E (gensym)
        B (gensym)
        U (gensym)
        Z (gensym)]
    `(let [{~B :bind ~U :unit ~Z :zero} ~monad
           ~E #(if (and (nil? %)
                        (some? ~Z)) ~Z (~U %))] (run-bind ~B ~steps (~E ~body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro deftest
  "A test group."
  [name & body]
  `(def ~name (fn_0 (filter #(not-nil? %) [~@body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure??
  "Assert this test ok."
  [form msg]
  `(czlab.mcfud.afx.core/ensure-test ~form ~msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ensure-thrown
  "Assert error was thrown."
  [expected form msg]
  `(try ~form
        (czlab.mcfud.afx.core/ensure-test-thrown ~expected nil ~msg)
        (catch ~'js/Error ~'e (czlab.mcfud.afx.core/ensure-test-thrown ~expected ~'e ~msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

