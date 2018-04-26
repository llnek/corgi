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

  (:require-macros czlab.elmo.afx.core)
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
;;EOF


