;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.crenshaw.stage2

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
(defn- initVars "" []
  (let [c (filterv #(> (count %) 0)
                   (cs/split UPPER #""))
        z (mapv (fn [_] 0) c)]
    (reset! VARS (zipmap c z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read "" [] 0)
(defn- getChar "" [] (set! *ch* (read)))

(defn- halt "" [])

(defn- write "" [& args])
(defn- writeLn "" [& args])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- newLine "" []
  (when (= *ch* "\r")
    (getChar)
    (if (= *ch* "\n") (getChar))))

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
      (do (skipWhite) (js/parseInt token))
      (recur (str token *ch*) (getChar)))))



(declare expression)

(defn- factor "" []
  (cond
    (= *ch* "(")
    (let [_ (match "(")
          ret (expression)
          _ (match ")")] ret)
    (isAlpha? *ch*)
    (get @VARS (getName))
    :else
    (getNum)))



(defn- term "" []
  (loop [ret (factor)]
    (if-not (isMulop? *ch*)
      ret
      (case *ch*
        "*" (do (match "*") (* ret (factor)))
        "/" (do (match "/") (/ ret (factor))) ret))))



(defn- expression "" []
  (loop [ret (if (isAddop? *ch*) 0 (term))]
    (if-not (isAddop? *ch*)
      ret
      (recur (case *ch*
               "+" (do (match "+") (+ ret (term)))
               "-" (do (match "-") (- ret (term))) ret)))))


(defn- assignment "" []
  (let [name (getName)]
    (match "=")
    (swap! VARS #(assoc % name (expression)))))

(defn- init "" []
  (getChar)
  (skipWhite))

(defn- input "" []
  (match "?")
  (swap! VARS #(assoc % (getName) (read))))


(defn- output "" []
  (match "!")
  (writeLn (get @VARS (getName))))


(defn- main "" []
  (init)
  (loop []
    (case *ch*
      "?" (input)
      "!" (output)
      (assignment))
    (newLine)
    (if (not= *ch* ".") (recur))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



