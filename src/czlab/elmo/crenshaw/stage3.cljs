;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.crenshaw.stage3

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [_1 _2 _3 do-with assoc!!]])

  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core
             :as ec :refer [EPSILON sqrt* abs* sqr* n# num?? invert]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private TAB "\t")
(def ^:private DIGITS #{"0" "1" "2" "3" "4"
                        "5" "6" "7" "8" "9"})
(def ^:private UPPER "ZYXWVUTSRQPONMLKJIHGFEDCBA")
(def ^:private LOWER "zyxwvutsrqponmlkjihgfedcba")
(def ^:private *ch* "")
(def ^:private VARS (atom {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read "" [] 0)
(defn- getChar "" [] (set! *ch* (read)))
(defn- halt "" [])
(defn- write "" [& args])
(defn- writeLn "" [& args])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- error "" [s]
  (writeLn)
  (writeLn "Error: " s "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- abort "" [s]
  (error s)
  (halt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expected "" [s]
  (abort (str s " Expected")))

(defn- upCase "" [c]
  (js/String.prototype.toUpperCase.call c))

(defn- isAlpha? "" [c]
  (>= (js/String.prototype.indexOf.call UPPER (upCase c))) 0)

(defn- isDigit? "" [c] (contains? DIGITS c))

(defn- isAlNum? "" [c]
   (or (isAlpha? c) (isDigit? c)))

(defn- isAddop? "" [c] (contains? #{"+" "-"} c))
(defn- isMulop? "" [c] (contains? #{"*" "/"} c))

(defn- isWhite? "" [c]
   (contains? #{" " "\t"} c))

(defn- skipWhite "" []
  (while (isWhite? *ch*) (getChar)))

(defn- match "" [x]
  (if (not= *ch* x) (expected (str "```" x "```")))
  (getChar)
  (skipWhite))

(defn- getName "" []
  (if-not (isAlpha? *ch*) (expected "Name"))
  (loop [token "" dummy nil]
    (if-not (isAlNum? *ch*)
      (do (skipWhite) token)
      (recur (str token (upCase *ch*)) (getChar)))))

(defn- getNum "" []
  (if-not (isDigit? *ch*) (expected "Integer"))
  (loop [token "" dummy nil]
    (if-not (isDigit? *ch*)
      (do (skipWhite) token)
      (recur (str token *ch*) (getChar)))))

(defn- emit "" [s] (write TAB s))

(defn- emitLn "" [s]
  (emit s)
  (writeLn))


(defn- other "" []
  (emitLn (getName)))


(defn- ident "" []
  (let [name (getName)]
    (if (= *ch* "(")
      (do (match "(")
          (match ")")
          (emitLn (str "BSR " name)))
      (emitLn (str "MOVE " name "(PC),D0")))))

(declare expression)

(defn- factor "" []
  (cond
    (= *ch* "(")
    (do (match "(")
        (expression)
        (match ")"))
    (isAlpha? *ch*)
    (ident)
    :else
    (emitLn (str "MOVE #" (getNum) ",D0"))))


(defn- mult "" []
  (match "*")
  (factor)
  (emitLn "MULS (SP)+,D0"))

(defn- div "" []
  (match "/")
  (factor)
  (emitLn "MOVE (SP)+,D1")
  (emitLn "EXS.L D0")
  (emitLn "DIVS D1,D0"))


(defn- term "" []
  (factor)
  (while (isMulop? *ch*)
    (emitLn "MOVE D0,-(SP)")
    (case *ch*
      "*" (mult)
      "/" (div)
      nil)))

(defn- add "" []
  (match "+")
  (term)
  (emitLn "ADD (SP)+,D0"))

(defn- sub "" []
  (match "-")
  (term)
  (emitLn "SUB (SP)+,D0")
  (emitLn "NEG D0"))


(defn- expression "" []
  (if (isAddop? *ch*)
    (emitLn "CLR D0")
    (term))
  (while (isAddop? *ch*)
    (emitLn "MOVE D0,-(SP)")
    (case *ch*
      "+" (add)
      "-" (sub)
      nil)))

(defn- assignment "" []
  (let [name (getName)]
    (match "=")
    (expression)
    (emitLn (str "LEA " name "(PC),A0"))
    (emitLn "MOVE D0,(A0)")))


(defn- newLabel "" [] "xxx")

(defn- postLabel "" [s]
  (writeLn (str s ":")))

(defn- condition "" []
  (emitLn "<condition>"))


(defn- block "" []
  (while (not (contains? #{"e"} *ch*))
    (case *ch*
      "i" (doIf)
      "o" (other)
      nil)))


(defn- doIf "" []
  (match "i")
  (condition)
  (let [L1 (newLabel)
        L2 L1]
    (emitLn (str "BEQ " L1))
    (block)
    (when (= *ch* "l")
      (match "l")
      (let [L2 (newLabel)]
        (emitLn (str "BRA " L2))
        (postLabel L1)
        (block)))
    (match "e")
    (postLabel L2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doProgram "" []
  (block)
  (if (not= *ch* "e") (expected "End"))
  (emitLn "END"))

(defn- init "" []
  (getChar)
  (skipWhite))

(defn- main "" []
  (init)
  (other))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



