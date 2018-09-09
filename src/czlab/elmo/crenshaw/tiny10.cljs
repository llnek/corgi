;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.crenshaw.tiny10

  (:require-macros [czlab.elmo.afx.core
                    :as ec :refer [_1 _2 _3 do-with assoc!!]])

  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core
             :as ec :refer [EPSILON sqrt* abs* sqr* n# num?? invert]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private DIGITS #{"0" "1" "2" "3" "4"
                        "5" "6" "7" "8" "9"})
(def ^:private UPPER "ZYXWVUTSRQPONMLKJIHGFEDCBA")
(def ^:private LOWER "zyxwvutsrqponmlkjihgfedcba")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *keywords* {"IF" "i"
                 "ELSE" "l"
                 "ENDIF" "e"
                 "WHILE" "w"
                 "ENDWHILE" "e"
                 "READ" "R"
                 "WRITE" "W"
                 "VAR" "v"
                 "BEGIN" "b"
                 "END" "e"
                 "PROGRAM" "p"})

(def MAX-ENTRY 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *token* "")
(def *value* "")
(def *ch* "")
(def *NEnrty* 0)
(def *symtypes* (make-array js/String 256))
(def *symbols* (make-array js/String 256))
(def *NEntry* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare expression block)

(defn- halt "" [])

(defn- writeLn "" [& args])
(defn- write "" [& args])

(defn- readLn "" [])
(defn- read "" [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- upCase "" [c]
  (js/String.prototype.toUpperCase.call c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getChar "" [] (set! *ch* (read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- error "" [s]
  (writeLn)
  (writeLn "Error: " s ".")
  (readLn)
  (readLn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- abort!!
  "" [s] (error s) (halt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- undefined!!
  "" [n]
  (abort!! (str "Undefined Identifier: " n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expected!! "" [s]
  (abort!! (str s " Expected")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isAlpha? "" [c]
  (>= (js/String.prototype.indexOf.call UPPER (upCase c)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isDigit? "" [c] (contains? DIGITS c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isAlNum?
  "" [c] (or (isAlpha? c) (isDigit? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isWhite?
  "" [c] (contains? #{" " "\t"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isMulop?
  "" [c] (contains? #{"*" "/"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isOrop?
  "" [c] (contains? #{"|" "~"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isRelop?
  "" [c] (contains? #{"=" "#" "<" ">"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- skipWhite
  "" [] (while (isWhite? *ch*) (getChar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- newLine "" []
  (while (= *ch* "\r")
    (getChar)
    (if (= *ch* "\n") (getChar))
    (skipWhite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- match "" [x]
  (newLine)
  (if (= *ch* x)
    (getChar)
    (expected!! (str "`"  x "`")))
  (skipWhite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getName "" []
  (newLine)
  (if-not (isAlpha? *ch*) (expected!! "Name"))
  (loop [s "" dummy nil]
    (if-not (isAlNum? *ch*)
      (do (skipWhite) (set! *value* s) s)
      (recur (str s (upCase *ch*)) (getChar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getNum "" []
  (newLine)
  (if-not (isDigit? *ch*) (expected!! "Integer"))
  (loop [v "" dummy nil]
    (if-not (isDigit? *ch*)
      (do (skipWhite) (js/parseInt v))
      (recur (str v *ch*) (getChar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lookup?? "" [arr s]
  (loop [i 0
         SZ (count arr) found? false]
    (if (or (>= i SZ) found?)
      (if found? (- i 1) -1)
      (recur (+ 1 i)
             SZ
             (= s (nth arr i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- inTable? "" [n]
  (>= (lookup?? *symbols* n) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addEntry "" [N T]
  (if (inTable? N)
    (abort!! (str "Duplicate Identifier: " N)))
  (if (>= *NEntry* MAX-ENTRY)
    (abort!! (str "Symbol Table Full")))
  (aset *symbols* *NEntry* N)
  (aset *symtypes* *NEntry* T)
  (set! *NEntry* (inc *NEntry*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- scan "" []
  (getName)
  (set! *token*
        (or (get *keywords* *value*) ""))
  *token*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- matchString "" [x]
  (if (not= *value* x)
    (expected!! (str "`" x "`"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- isAddop? "" [c]
  (contains? #{"+" "-"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- newLabel "" [] "@L0000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- emit "" [s] (write "\t" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- emitLn
  "" [s] (emit s) (writeLn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init "" []
  (doseq [n (range MAX-ENTRY)]
    (aset *symbols* n "")
    (aset *symtypes* n " "))
  (getChar)
  (scan))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- fin "" []
  (if (= *ch* "\r") (getChar))
  (if (= *ch* "\n") (getChar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- postLabel
  "" [L] (writeLn L ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- header "" []
  (writeLn "Place-holder for MASM start-up code")
  (emitLn "LIB TINYLIB"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prolog "" []
  (postLabel "MAIN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- epilog "" []
  (emitLn "Place-holder for epilog"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- alloc "" [N]
  (if (inTable? N)
    (abort!! (str "Duplicate Variable Name: " N)))
  (addEntry N "v")
  (write "var " N  " : integer = ")
  (if (= *ch* "=")
    (do (match "=")
        (when (= *ch* "-")
          (write *ch*)
          (match "-"))
        (writeLn (getNum) ";"))
    (writeLn "0;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- decl "" []
  (alloc (getName))
  (while (= *ch* ",")
    (match ",")
    (alloc (getName))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- topDecls "" []
  (loop [token (scan)]
    (if (not= "b" token)
      (if (= "v" token)
        (decl)
        (abort!! (str "Unrecognized Keyword: " *value*)))
      (recur (scan)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clear "" []
  (emitLn "XOR EAX, EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negate "" []
  (emitLn "NEG EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- loadConst "" [n]
  (emit "MOV EAX, ")
  (writeLn n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- loadVar "" [name]
  (if-not (inTable? name) (undefined!! name))
  (emitLn (str "MOV EAX, " name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- push "" []
  (emitLn "PUSH EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popAdd "" []
  (emitLn "POP EDX")
  (emitLn "ADD EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popSub "" []
  (emitLn "POP EDX")
  (emitLn "SUB EAX, EDX")
  (emitLn "NEG EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popMul "" []
  (emitLn "POP EDX")
  (emitLn "IMUL EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popDiv "" []
  (emitLn "MOV ECX, EAX")
  (emitLn "POP EAX")
  (emitLn "XOR EDX, EDX") ;;Clear EDX
  (emitLn "IDIV ECX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- store "" [name]
  (if-not (inTable? name) (undefined!! name))
  (emitLn (str "MOV " name ", EAX")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notIt "" [] (emitLn "NOT EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popAnd "" []
  (emitLn "POP EDX")
  (emitLn "AND EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popOr "" []
  (emitLn "POP EDX")
  (emitLn "OR EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popXor "" []
  (emitLn "POP EDX")
  (emitLn "XOR EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- popCompare "" []
  (emitLn "POP EDX")
  (emitLn "CMP EDX, EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setEqual "" []
  (emitLn "CMOVE EAX, T")
  (emitLn "CMOVNE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setNEqual "" []
  (emitLn "CMOVE EAX, F")
  (emitLn "CMOVNE EAX, T"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setGreater "" []
  (emitLn "CMOVG EAX, T")
  (emitLn "CMOVLE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setLess "" []
  (emitLn "CMOVL EAX, T")
  (emitLn "CMOVGE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setLessOrEqual "" []
  (emitLn "CMOVLE EAX, T")
  (emitLn "CMOVG EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- setGreaterOrEqual "" []
  (emitLn "CMOVGE EAX, T")
  (emitLn "CMOVL EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- branch "" [L]
  (emitLn (str "JMP " L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- branchFalse "" [L]
  (emitLn "TEST EAX, -1")
  (emitLn (str "JE " L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- equals "" []
  (match "=")
  (expression)
  (popCompare)
  (setEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lessOrEqual "" []
  (match "=")
  (expression)
  (popCompare)
  (setLessOrEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notEqual "" []
  (match ">")
  (expression)
  (popCompare)
  (setNEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- less "" []
  (match "<")
  (case *ch*
    "=" (lessOrEqual)
    ">" (notEqual)
    (do (expression)
        (popCompare)
        (setLess))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- greater "" []
  (match ">")
  (if (= *ch* "=")
    (do (match "=")
        (expression)
        (popCompare)
        (setGreaterOrEqual))
    (do (expression)
        (popCompare)
        (setGreater))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- relation "" []
  (expression)
  (when (isRelop? *ch*)
    (push)
    (case *ch*
      "=" (equals)
      "#" (notEqual)
      "<" (less)
      ">" (greater)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notFactor "" []
  (if (= *ch* "!")
    (do (match "!")
        (relation)
        (notIt))
    (relation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolTerm "" []
  (newLine)
  (notFactor)
  (while (= *ch* "&")
    (push)
    (match "&")
    (notFactor)
    (popAnd)
    (newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolOr "" []
  (match "|")
  (boolTerm)
  (popOr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolXor "" []
  (match "~")
  (boolTerm)
  (popXor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolExpression "" []
  (newLine)
  (boolTerm)
  (while (isOrop? *ch*)
    (push)
    (case *ch*
      "|" (boolOr)
      "~" (boolXor) nil)
    (newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doIf "" []
  (boolExpression)
  (let [L1 (newLabel)
        _ (branchFalse L1)
        _ (block)
        X (if (= *token* "l")
            (let [L2 (newLabel)]
              (branch L2)
              (postLabel L1)
              (block) L2) L1)]
    (postLabel X)
    (matchString "ENDIF")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doWhile "" []
  (let [L1 (newLabel)
        L2 (newLabel)]
    (postLabel L1)
    (boolExpression)
    (branchFalse L2)
    (block)
    (matchString "ENDWHILE")
    (branch L1)
    (postLabel L2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- factor "" []
  (cond
    (= *ch* "(")
    (do (match "(")
        (boolExpression)
        (match ")"))
    (isAlpha? *ch*)
    (loadVar (getName))
    :else
    (loadConst (getNum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negFactor "" []
  (match "-")
  (if (isDigit? *ch*)
    (loadConst (- (getNum)))
    (do (factor)
        (negate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- firstFactor "" []
  (case *ch*
    "+" (do (match "+")
            (factor))
    "-" (negFactor)
    (factor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mult "" []
  (match "*")
  (factor)
  (popMul))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- div "" []
  (match "/")
  (factor)
  (popDiv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- term1 "" []
  (newLine)
  (while (isMulop? *ch*)
    (push)
    (case *ch*
      "*" (mult)
      "/" (div)
      nil)
    (newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- term "" []
  (factor)
  (term1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- firstTerm "" []
  (firstFactor)
  (term1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add "" []
  (match "+")
  (term)
  (popAdd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subtract "" []
  (match "-")
  (term)
  (popSub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expression "" []
  (newLine)
  (firstTerm)
  (while (isAddop? *ch*)
    (push)
    (case *ch*
      "+" (add)
      "-" (subtract)
      nil)
    (newLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- assignment "" []
  (let [name *value*]
    (match "=")
    (boolExpression)
    (store name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- readVar "" []
  (emitLn "CALL READ")
  (store *value*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- writeVar "" []
  (emitLn "CALL WRITE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doRead "" []
  (match "(")
  (getName)
  (readVar)
  (while (= *ch* ",")
    (match ",")
    (getName)
    (readVar))
  (match ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doWrite "" []
  (match "(")
  (expression)
  (writeVar)
  (while (= *ch* ",")
    (match ",")
    (expression)
    (writeVar))
  (match ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- block "" []
  (scan)
  (while (not (contains? #{"e" "l"} *token*))
    (case *token*
      "i" (doIf)
      "w" (doWhile)
      "R" (doRead)
      "W" (doWrite)
      (assignment))
    (scan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- main* "" []
  (matchString "BEGIN")
  (prolog)
  (block)
  (matchString "END")
  (epilog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prog "" []
  (matchString "PROGRAM")
  (header)
  (topDecls)
  (main*)
  (match "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn main "" []
  (init)
  (prog)
  (if (not= *ch* "\r")
    (abort!! (str "Unexpected data after .")))
  (readLn)
  (readLn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

