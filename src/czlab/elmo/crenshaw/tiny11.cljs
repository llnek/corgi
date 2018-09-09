;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.crenshaw.tiny11

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
(defn- duplicate!! "" [n]
  (abort!! (str "Duplicate Identifier: " n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkIdent "" []
  (if (not= *token* "")
    (expected!! "Identifier")))

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
  "" [c] (contains? #{" " "\t" "\r" "\n"} c))

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
(defn- getName "" []
  (skipWhite)
  (if-not (isAlpha? *ch*)
    (expected!! "Identifier"))
  (set! *token* "")
  (set! *value* "")
  (loop [v "" dummy nil]
    (if-not (isAlNum? *ch*)
      (set! *value* v)
      (recur (str v (upCase *ch*)) (getChar))))
  *value*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getNum "" []
  (skipWhite)
  (if-not (isDigit? *ch*)
    (expected!! "Number"))
  (set! *token* "#")
  (set! *value* "")
  (loop [v "" dummy nil]
    (if-not (isDigit? *ch*)
      (set! *value* v)
      (recur (str v *ch*) (getChar))))
  *value*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getOp "" []
  (skipWhite)
  (set! *token* *ch*)
  (set! *value* *ch*)
  (getChar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nextToken "" []
  (skipWhite)
  (cond
    (isAlpha? *ch*)
    (getName)
    (isDigit? *ch*)
    (getNum)
    :else (getOp)))

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
(defn- checkTable
  "" [n] (if-not (inTable? n) (undefined!! n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- checkDup
  "" [n] (if (inTable? n) (duplicate!! n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- addEntry "" [N T]
  (checkDup N)
  (if (>= *NEntry* MAX-ENTRY)
    (abort!! "Symbol Table Full"))
  (aset *symtypes* *NEntry* T)
  (aset *symbols* *NEntry* N)
  (set! *NEntry* (+ 1 *NEntry*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- scan "" []
  (if (= *token* "")
    (set! *token*
          (or (get *keywords* *value*) ""))) *token*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- matchString "" [x]
  (if (not= *value* x) (expected!! (str "`" x "`")))
  (nextToken))

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
  (nextToken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- postLabel "" [L] (writeLn L ":"))

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
(defn- allocate "" [name val]
  (writeLn "Var " name " : integer = " val ";"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- alloc "" []
  (nextToken)
  (if (not= *token* "")
    (expected!! "Variable Name"))
  (checkDup *value*)
  (addEntry *value* "v")
  (allocate *value* "0")
  (nextToken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- topDecls "" []
  (scan)
  (while (= *token* "v") (alloc))
  (while (= *token* ",") (alloc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clear "" []
  (emitLn "XOR EAX, EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negate "" []
  (emitLn "NEG EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- loadConst "" [n]
  (emitLn (str "MOV EAX, " n)))

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
  (emitLn (str "MOV " name ", EAX")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notIt "" [] (emitLn "NOT EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- readIt
  "" [name]
  (emitLn "CALL READ") (store name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- writeIt "" [] (emitLn "CALL WRITE"))

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
(defn- compareExpression "" []
  (expression)
  (popCompare))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nextExpression "" []
  (nextToken)
  (compareExpression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- equals "" []
  (nextExpression)
  (setEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lessOrEqual "" []
  (nextExpression)
  (setLessOrEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notEqual "" []
  (nextExpression)
  (setNEqual))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- less "" []
  (nextToken)
  (case *token*
    "=" (lessOrEqual)
    ">" (notEqual)
    (do (compareExpression)
        (setLess))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- greater "" []
  (nextToken)
  (if (= *token* "=")
    (do (nextExpression) (setGreaterOrEqual))
    (do (compareExpression) (setGreater))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- relation "" []
  (expression)
  (when (isRelop? *token*)
    (push)
    (case *token*
      "=" (equals)
      "<" (less)
      ">" (greater)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- notFactor "" []
  (if (= *token* "!")
    (do (nextToken)
        (relation)
        (notIt))
    (relation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolTerm "" []
  (notFactor)
  (while (= *token* "&")
    (push)
    (nextToken)
    (notFactor)
    (popAnd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolOr "" []
  (nextToken)
  (boolTerm)
  (popOr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolXor "" []
  (nextToken)
  (boolTerm)
  (popXor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- boolExpression "" []
  (boolTerm)
  (while (isOrop? *token*)
    (push)
    (case *token* "|" (boolOr) "~" (boolXor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doIf "" []
  (nextToken)
  (boolExpression)
  (let [L1 (newLabel)
        _ (branchFalse L1)
        _ (block)
        L2 (if (= *token* "l")
             (let [_ (nextToken)
                   L2 (newLabel)]
               (branch L2)
               (postLabel L1)
               (block) L2) L1)]
    (postLabel L2)
    (matchString "ENDIF")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doWhile "" []
  (nextToken)
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
  (if (= *token* "(")
    (do (nextToken)
        (boolExpression)
        (matchString ")"))
    (do (case *token*
          "" (loadVar *value*)
          "#" (loadConst *value*)
          (expected!! "Math Factor"))
        (nextToken))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mult "" []
  (nextToken)
  (factor)
  (popMul))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- div "" []
  (nextToken)
  (factor)
  (popDiv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- term "" []
  (factor)
  (while (isMulop? *token*)
    (push)
    (case *token* "*" (mult) "/" (div) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add "" []
  (nextToken)
  (term)
  (popAdd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subtract "" []
  (nextToken)
  (term)
  (popSub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expression "" []
  (if (isAddop? *token*) (clear) (term))
  (while (isAddop? *token*)
    (push)
    (case *token* "+" (add) "-" (subtract) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- assignment "" []
  (checkTable *value*)
  (let [name *value*]
    (nextToken)
    (matchString "=")
    (boolExpression)
    (store name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- readVar "" []
  (checkIdent)
  (checkTable *value*)
  (readIt *value*)
  (nextToken))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- writeVar "" [] (emitLn "CALL WRITE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doRead "" []
  (nextToken)
  (matchString "(")
  (readVar)
  (while (= *token* ",")
    (nextToken)
    (readVar))
  (matchString ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- doWrite "" []
  (nextToken)
  (matchString "(")
  (expression)
  (writeIt)
  (while (= *token* ",")
    (nextToken)
    (expression)
    (writeIt))
  (matchString ")"))

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
(defn main "" []
  (init)
  (matchString "PROGRAM")
  (header)
  (topDecls)
  (matchString "BEGIN")
  (prolog)
  (block)
  (matchString "END")
  (epilog)
  (readLn)
  (readLn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


