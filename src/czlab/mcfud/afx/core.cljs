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
                    :as ec :refer [do-with defvoid
                                   defvoid-
                                   n# _1 defmonad clog]])

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
(defn sqrt*

  "Square root of a number."
  [n]

  (js/Math.sqrt n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn abs*

  "Absolute value of a number."
  [n]

  (js/Math.abs n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sqr*

  "Square a number."
  [n]

  (* n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn raise!

  "Throw an exception."
  [& args]

  (throw (js/Error. (cs/join "" args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def undef-fn (constantly js/undefined))
(def false-fn (constantly false))
(def true-fn (constantly true))
(def nil-fn (constantly nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn debug*

  "Debug log to console."
  [& msgs]

  (clog msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn info*

  "Info log to console."
  [& msgs]

  (clog msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn warn*

  "Warning log to console."
  [& msgs]

  (clog msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn error*

  "Error log to console."
  [& msgs]

  (clog msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn x->str

  "Call toString on the argument."
  [x]

  (.toString (if (number? x) (js/Number x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clj->json

  "Clojure object to string."
  [obj]

  (js/JSON.stringify (clj->js obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeatStr

  "Repeat the string N times."
  [times s]

  (gs/repeat s times))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn zero??

  "Safe test if zero."
  [n]

  (and (number? n) (zero? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pos??

  "Safe test if positive."
  [n]

  (and (number? n) (pos? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn neg??

  "Safe test if negative."
  [n]

  (and (number? n) (neg? n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn numFlip

  "Invert a number if not zero."
  [x]

  (if (number? x) (if (zero?? x) 0 (/ 1 x)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nneg?

  "True if not negative."
  [x]

  (not (neg?? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn num??

  "If arg is not a number, return something else."
  [arg n]

  (if (number? arg) arg n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn numSign

  "Return the sign of the number."
  [n]

  (cond (zero?? n) 0 (pos?? n) 1 :else -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->float

  "A string into float."
  [x]

  (gs/toNumber x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn s->int

  "A string into int."
  [x]

  (gs/toNumber x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nestr?

  "Non empty string"
  [s]

  (and (string? s) (not-empty s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn estr?

  "Empty string"
  [s]

  (not (nestr? s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nichts?

  "True if object is either null of undefined"
  [obj]

  (or (undefined? obj) (nil? obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn escXml

  "Escape XML special chars"
  [s]

  (let [out (array)]
    (doseq [c s]
      (.push out (condp = c
                   "&" "&amp;"
                   ">" "&gt;"
                   "<" "&lt;"
                   "\"" "&quot;"
                   "'" "&apos;" c))) (cs/join "" out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splitSeq

  "Split a collection into 2 parts"
  [coll cnt]

  (if (< cnt (n# coll))
    (vector (take cnt coll)
            (drop cnt coll))
    (vector (concat [] coll) [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn percent

  "Return the percentage."
  [numerator denominator]

  (* 100.0 (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn numToFixed

  "Print number to n decimals."
  [n & [digits]]

  (ocall (js/Number. n) "toFixed" (num?? digits 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splitStr

  "Returns a sequence of strings of n characters each."
  [n string]

  (map #(cs/join "" %) (partition-all n string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn minBy

  "Find item with minimum value as defined by the function."
  [f coll]

  (if (not-empty coll)
    (reduce #(if (< (f %1)
                    (f %2)) %1 %2) (_1 coll) (rest coll)) js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn maxBy

  "Find item with maximum value as defined by the function."
  [f coll]

  (if (not-empty coll)
    (reduce #(if (< (f %1)
                    (f %2)) %2 %1) (_1 coll) (rest coll)) js/undefined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randRange

  "Return a random number between this 2 numbers."
  [from to]

  (js/Math.floor (+ from (* (rand) (+ 1 (- to from))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isSSL?

  "True if the browser url is secured."
  []

  (and js/window
       js/window.location
       (contains? js/window.location.protocol "https")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWebSockProtocol

  "Return the websocket protocol."
  []

  (if (isSSL?) "wss://" "ws://"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmtUrl

  "Format a URL based on the current web address host."
  [scheme uri]

  (if (and js/window
           js/window.location)
      (str scheme js/window.location.host uri) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn objectize

  "Parse json string into js-object."
  [s]

  (if (string? s) (js/JSON.parse s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jsonize

  "JS object into json string."
  [obj]

  (if (some? obj) (js/JSON.stringify obj) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jsonize*

  "Clojure object into json string."
  [obj]

  (jsonize (clj->js obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fillArray

  "Return js-array with length (len) and filled with value."
  [value len]

  (do-with [arr (array)] (dotimes [_ len] (.push arr value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn copyArray

  "Copy data into another js-array."
  [src des]
  {:pre [(= (n# src)(n# des))
         (and (array? src)(array? des))]}

  (do-with [des]
           (dotimes [n (n# src)] (aset des n (nth src n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isMobile?

  "Test if the client is a mobile device."
  [navigator]

  (if (some? navigator)
    (-> #"(?i)Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini"
        (re-matches (oget navigator "?userAgent")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isSafari?

  "Test if the client is Safari browser."
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

  (if (fn? (oget e "?preventDefault"))
    (ocall e "preventDefault")
    (oset! e "returnValue" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clamp

  "Clamp a value between 2 limits."
  [low high a]

  (if (< a low) low (if (> a high) high a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randSign

  "Randomly pick positive or negative."
  []

  (if (zero? (rem (js/Math.floor (rand 10)) 2)) -1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randItem

  "Randomly choose an item from this array."
  [coll]

  (let [sz (n# coll)]
    (condp = sz
      0 js/undefined
      1 (_1 coll)
      (nth coll (js/Math.floor (* (rand) sz))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randPercent

  "Randomly choose a percentage in step of 10."
  []

  (randItem [0.1 0.9 0.3 0.7 0.6 0.5 0.4 0.8 0.2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toBasicAuthHeader

  "Format input into HTTP Basic Authentication."
  [user pwd]

  ["Authorization" (str "Basic "
                        (b64/encodeString (str "" user ":" pwd) true))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deepMerge

  "Merge (deep) of clojure data."
  [a b & more]

  (let [tmp (transient a)]
    (doseq [[k vb] b
            :let [va (get a k)]]
      (assoc! tmp k (if-not (contains? a k)
                      vb
                      (cond (and (map? vb) (map? va))
                            (deepMerge va vb)
                            (and (set? vb) (set? va))
                            (cst/union va vb)
                            :else vb))))
    (persistent! tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private T_OKAY "PASSED")
(def ^:private T_EROR "FAILED")
(defn ensureTest

  "Assert a test condition, returning a message."
  [cnd msg]

  (str (try (if cnd T_OKAY T_EROR)
            (catch js/Error e T_EROR)) ": " msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ensureTestThrown

  "Assert a exception thrown during test."
  [expected error msg]

  (str (if (nichts? error)
         T_EROR
         (cond (string? expected)
               (if (or (= expected "any")
                       (= expected error)) T_OKAY T_EROR)
               (instance? expected error)
               T_OKAY
               :else
               T_EROR)) ": " msg))

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
             [(repeatStr 78 "+")
              (or title "test")
              (js/Date.)
              (repeatStr 78 "+")
              (cs/join "\n" results)
              (repeatStr 78 "=")
              (cs/join "" ["Passed: " ok "/" sum " [" perc  "%]"])
              (str "Failed: " (- sum ok))
              (cs/join "" ["cpu-time: " diff "ms"])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad monad-identity

  "Monad describing plain computations. This monad does in fact nothing
  at all. It is useful for testing, for combination with monad
  transformers, and for code that is parameterized with a monad."

  (vector :bind (fn [mv mf] (mf mv)) :unit (fn [x] x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad monad-maybe

  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."

  (vector :unit (fn [x] x)
          :bind (fn [mv mf] (if-not (nichts? mv) (mf mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad monad-list

  "Monad describing multi-valued computations, i.e. computations
  that can yield multiple values. Any object implementing the seq
  protocol can be used as a monadic value."

  (vector :bind (fn [mv mf] (flatten (map mf mv)))
          :unit (fn [x] (vector x))
          :zero []
          :plus (fn [& xs] (flatten xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad monad-state

  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."

  (vector :unit (fn [v] (fn [x] [v x]))
          :bind (fn [mv mf]
                  (fn [s]
                    (let [[v s'] (mv s)] ((mf v) s'))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmonad monad-continuation

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
;;EOF

