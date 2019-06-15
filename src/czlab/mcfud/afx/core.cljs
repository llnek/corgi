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

  (:require-macros [czlab.mcfud.afx.core
                    :as c :refer [n# _1 cc+ in?
                                  if-func fn_1 fn_2 fn_* atom?
                                  do-with-transient do-with defmonad]])

  (:require [clojure.string :as cs]
            [clojure.set :as cst]
            [goog.string :as gs]
            [goog.crypt.base64 :as b64]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def POS-INF js/Number.POSITIVE_INFINITY)
(def NEG-INF js/Number.NEGATIVE_INFINITY)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn abs* "Absolute value." [n] (js/Math.abs n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sqrt* "Square root." [n] (js/Math.sqrt n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sqr* "Square a number." [n] (* n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn raise!
  "Throw an exception."
  [& args]
  (throw (js/Error. (cs/join "" args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def fn-undef (constantly js/undefined))
(def fn-false (constantly false))
(def fn-true (constantly true))
(def fn-nil (constantly nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-str
  "Repeat string n times."
  [times s] (gs/repeat s times))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn zero??
  "Safe test zero?"
  [n] (and (number? n) (zero? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pos??
  "Safe test pos?"
  [n] (and (number? n) (pos? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn neg??
  "Safe test neg?"
  [n] (and (number? n) (neg? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num-flip
  "Invert number if not zero."
  [x]
  (if (number? x) (if (zero? x) 0 (/ 1 x)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nneg? "Not neg?" [x] (not (neg?? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num??
  "If n is not a number, return other."
  [n other] (if (number? n) n other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num-sign
  "Sign of the number."
  [n]
  (cond (zero?? n) 0 (pos?? n) 1 :else -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn str->num "String to number." [s] (gs/toNumber s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nestr?
  "Non empty string?"
  [s] (and (string? s) (not-empty s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn estr? "Empty string?" [s] (not (nestr? s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?
  "Object is null or undefined?"
  [obj] (or (undefined? obj) (nil? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn esc-xml
  "Escape XML special chars."
  [s]
  (loop [i 0 SZ (n# s) ret (c/tvec*)]
    (if (>= i SZ)
      (cs/join "" (c/ps! ret))
      (let [c (nth s i)]
        (recur (+ 1 i) SZ (conj! ret
                                 (condp = c
                                   "&" "&amp;"
                                   ">" "&gt;"
                                   "<" "&lt;"
                                   "\"" "&quot;"
                                   "'" "&apos;" c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-seq
  "Split a collection into 2 parts"
  [coll cnt]
  (if (< cnt (n# coll))
    (vector (take cnt coll)
            (drop cnt coll))
    (vector (cc+ [] coll) [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent
  "Return the percentage."
  [numerator denominator]
  (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num->fixed
  "Print number to n decimals."
  [n & [digits]]
  (ocall (js/Number. n) "toFixed" (num?? digits 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn split-str
  "Split a string into n chars each."
  [n string]
  (map #(cs/join "" %) (partition-all n string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-asc*
  "A generic compare function."
  [f]
  (fn_2 (cond (< (f ____1) (f ____2)) -1
              (> (f ____1) (f ____2)) 1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compare-des*
  "A generic compare function."
  [f]
  (fn_2 (cond (< (f ____1) (f ____2)) 1
              (> (f ____1) (f ____2)) -1 :else 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- xxx-by
  "Used by min-by & max-by - internal."
  [cb coll]
  (if (not-empty coll)
    (reduce cb (_1 coll) (rest coll)) js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn min-by
  "Find item with minimum value as defined by the function."
  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %1 %2) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn max-by
  "Find item with maximum value as defined by the function."
  [f coll]
  (xxx-by #(if (< (f %1) (f %2)) %2 %1) coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-range
  "Pick a random number between 2 limits."
  [from to]
  (js/Math.floor (+ from (* (rand) (+ 1 (- to from))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-ssl?
  "If the browser url is secured?"
  []
  (and js/window
       js/window.location
       (in? js/window.location.protocol "https")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-websock-protocol
  "Websocket protocol prefix."
  []
  (if (is-ssl?) "wss://" "ws://"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmt-url
  "Format a URL based on the current web address host."
  [scheme uri]
  (if (and js/window
           js/window.location)
      (str scheme js/window.location.host uri) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn str->js
  "String to js-object."
  [s] (if (string? s) (js/JSON.parse s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn str->clj
  "String to clj-object."
  [s] (js->clj (str->js s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jsonize
  "To json string."
  [obj]
  (condp = obj
    js/undefined nil
    nil "null"
    (js/JSON.stringify (if (or (array? obj)
                               (object? obj)) obj (clj->js obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fill-array
  "JS-array with length (len) filled with value."
  [value len]
  (do-with [out (array)] (dotimes [_ len] (.push out value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn copy-array
  "Copy data into another JS-array."
  [src des]
  {:pre [(= (n# src)(n# des))
         (and (array? src)(array? des))]}
  (do-with [des]
    (dotimes [n (n# src)] (aset des n (nth src n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-mobile?
  "If client is a mobile device."
  [navigator]
  (if (some? navigator)
    (-> #"(?i)Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini"
        (re-matches (oget navigator "?userAgent")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-safari?
  "If client is Safari browser."
  [navigator]
  (if (some? navigator)
    (and (re-matches #"Safari"
                     (oget navigator "?userAgent"))
         (re-matches #"Apple Computer"
                     (oget navigator "?vendor")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pde
  "Prevent default propagation of this event."
  [e]
  (if-func [f (oget e "?preventDefault")]
    (ocall e "preventDefault") (oset! e "returnValue" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clamp
  "Clamp a value between 2 limits."
  [low high v]
  (if (< v low) low (if (> v high) high v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-sign
  "Randomly pick positive or negative."
  []
  (if (zero? (rem (js/Math.floor (rand 10)) 2)) -1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn crand
  "Randomly choose an item from an array."
  [coll]
  (let [sz (n# coll)]
    (condp = sz
      0 js/undefined
      1 (_1 coll)
      (nth coll (js/Math.floor (* (rand) sz))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn prand
  "Randomly choose a percentage in step of 10."
  []
  (crand [0.1 0.9 0.3 0.7 0.6 0.5 0.4 0.8 0.2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn basic-auth-header
  "Format input into HTTP Basic Authentication."
  [user pwd]
  ["Authorization" (str "Basic "
                        (b64/encodeString (str "" user ":" pwd) true))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn merge+
  "Merge (deep) of clojure data."
  [a b & more]
  (do-with-transient [tmp (c/tmap* a)]
    (doseq [[k vb] b
            :let [va (get a k)]]
      (assoc! tmp
              k
              (if-not (in? a k)
                vb
                (cond (and (map? vb)
                           (map? va))
                      (merge+ va vb)
                      (and (set? vb)
                           (set? va))
                      (cst/union va vb)
                      :else vb))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private t-bad "FAILED")
(def ^:private t-ok "PASSED")
(defn ensure-test
  "Assert a test condition, returning a message."
  [cnd msg]
  (str (try (if cnd t-ok t-bad)
            (catch js/Error e t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensure-test-thrown
  "Assert an exception is thrown during test."
  [expected error msg]
  (str (if (nichts? error)
         t-bad
         (cond (string? expected)
               (if (or (= expected "any")
                       (= expected error)) t-ok t-bad)
               (instance? expected error)
               t-ok
               :else
               t-bad)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn runtest
  "Run a test group, returning the summary."
  [test & [title]]
  {:pre [(fn? test)]}
  (let [f #(cs/starts-with? % "P")
        mark (system-time)
        results (test)
        sum (n# results)
        ok (n# (filter f results))
        diff (- (system-time) mark)
        perc (int (* 100 (/ ok sum)))]
    (cs/join "\n"
             [(repeat-str 78 "+")
              (or title "test")
              (js/Date.)
              (repeat-str 78 "+")
              (cs/join "\n" results)
              (repeat-str 78 "=")
              (cs/join "" ["Passed: " ok "/" sum " [" perc  "%]"])
              (str "Failed: " (- sum ok))
              (cs/join "" ["cpu-time: " diff "ms"])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-identity
  "Monad describing plain computations. This monad does in fact nothing
  at all. It is useful for testing, for combination with monad
  transformers, and for code that is parameterized with a monad."
  (vector :bind (fn [mv mf] (mf mv)) :unit identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-maybe
  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."
  (vector :unit identity
          :bind (fn [mv mf] (if-not (nichts? mv) (mf mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-list
  "Monad describing multi-valued computations, i.e. computations
  that can yield multiple values. Any object implementing the seq
  protocol can be used as a monadic value."
  (vector :bind (fn [mv mf] (flatten (map mf mv)))
          :unit (fn_1 (vector ____1))
          :zero []
          :plus (fn_* (flatten ____xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-state
  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."
  (vector :unit (fn [v] (fn [s] [v s]))
          :bind (fn [mv mf]
                  (fn [s]
                    (let [[v s'] (mv s)] ((mf v) s'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad m-continuation
  "Monad describing computations in continuation-passing style. The monadic
  values are functions that are called with a single argument representing
  the continuation of the computation, to which they pass their result."
  (vector :unit (fn [v] (fn [cont] (cont v)))
          :bind (fn [mv mf]
                  (fn [cont]
                    (mv (fn [v] ((mf v) cont)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-cont
  "Execute the computation cont
  in the cont monad and return its result."
  [cont]
  (cont identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;in memory store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-memset
  "New in-memory object store. Object must be an atom."
  [& [batch]]
  (atom {:batch (num?? batch 10) :size 0 :next 0 :slots #js[]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add->set! "" [store obj]
  {:pre [(atom? obj)]}
  (do-with [obj]
    (swap! store
           (fn [{:keys [next size
                        batch slots] :as root}]
             (let [g #(do (c/n-times batch (.push slots nil))
                          (+ size batch))
                   next1 (+ 1 next)
                   size' (if (< next size) size (g))]
               (swap! obj #(assoc % :____slot next))
               (aset slots next obj)
               (assoc root :next next1 :size size'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop->set!
  "Free the object from the store."
  [store obj]
  (if (atom? obj)
    (swap! store
           (fn [{:keys [next slots] :as root}]
             (let [next1 (- next 1)
                   tail (aget slots next1)
                   slot' (:____slot @tail)
                   epos' (:____slot @obj)]
               ;move the tail to old slot
               (aset slots next1 nil)
               (aset slots epos' tail)
               (swap! tail #(assoc % :____slot epos'))
               (swap! obj #(dissoc % :____slot))
               (merge root {:next next1}))))) store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

