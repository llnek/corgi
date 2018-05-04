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
            [goog.crypt.base64 :as b64]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn raise! "" [& args] (throw (js/Error. (apply str args ))))
(defn noopy "" [& xs] js/undefined)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn debug* "" [& msgs] (js/console.log (apply str msgs)))
(defn info* "" [& msgs] (js/console.log (apply str msgs)))
(defn warn* "" [& msgs] (js/console.log (apply str msgs)))
(defn error* "" [& msgs] (js/console.log (apply str msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn repeat-string "" [n x] (gs/repeat x n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xmod "Propert modulo." [x N]
  (if (< x 0)
    (- x (* -1 (+ N (* (js/Math.floor (/ (- x) N)) N)))) (rem x N)))

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
(defn randRange "" [from to]
  (js/Math.floor (+ from (* (js/Math.random) (inc (- to from))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isSSL? "" []
  (and js/window
       js/window.location
       (contains? js/window.location.protocol "https")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getWebSockProtocol "" [] (if (isSSL?) "wss://" "ws://"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fmtUrl
  "Format a URL based on the current web address host."
  [scheme uri]
  (if (and js/window js/window.location)
      (str scheme js/window.location.host uri) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn objectize "" [s] (if (string? s) (js/JSON.parse s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn jsonize "" [obj] (if (some? obj) (js/JSON.stringify obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn isMobile?
  "Test if the client is a mobile device."
  [navigator]
  (if (some? navigator)
    (re-matches #"(?i)Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini"
                (oget navigator "?userAgent"))))

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
  "Prevent default propagation of this event." [e]
  (if (fn? (oget e "?preventDefault"))
    (ocall e "preventDefault")
    (oset! e "returnValue" false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randSign
  "Randomly pick positive or negative."
  [] (if (zero? (rem (rand 10) 2)) -1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randItem
  "Randomly choose an item from this array." [coll]
  (cond
    (empty? coll) nil
    (= 1 (count coll)) (_1 coll)
    :else (nth coll (js/Math.floor (* (js/Math.random) (count coll))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn randPercent
  "Randomly choose a percentage in step of 10." []
  (randItem [0.1 0.9 0.3 0.7 0.6 0.5 0.4 0.8 0.2]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn toBasicAuthHeader
  "Format input into HTTP Basic Authentication."
  [user pwd]
  ["Authorization" (str "Basic "
                        (b64/encodeString (str "" user ":" pwd) true))])

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
          title (js/Date.)
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

