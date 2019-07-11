;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.crenshaw.tiny11

  (:require [czlab.mcfud.afx.math :as m]
            [clojure.string :as cs]
            [czlab.mcfud.afx.core
             :as ec :refer [n# o- o+ _1 _2 _3 do-with]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private DIGITS #{"0" "1" "2" "3" "4"
                        "5" "6" "7" "8" "9"})
(def ^:private UPPER "ZYXWVUTSRQPONMLKJIHGFEDCBA")
(def ^:private LOWER "zyxwvutsrqponmlkjihgfedcba")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private KEYWORDS {"IF" "i"
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
(def ^:private MAX-ENTRY 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private SYMTYPES (make-array js/String 256))
(def ^:private SYMBOLS (make-array js/String 256))
(def ^:private TOKEN "")
(def ^:private VALUE "")
(def ^:private CH "")
(def ^:private NEntry 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare expression block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- halt [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- writeln [& args])
(defn- write [& args])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- readln [])
(defn- read [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- upcase [c] (cs/upper-case c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-char [] (set! CH (read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- error [s]
  (writeln) (writeln "Error: " s ".") (readln) (readln))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- abort!! [s] (error s) (halt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- undefined!! [n] (abort!! (str "Undefined Identifier: " n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expected!! [s] (abort!! (str s " Expected")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- duplicate!! [n] (abort!! (str "Duplicate Identifier: " n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-ident []
  (if (not= TOKEN "") (expected!! "Identifier")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-alpha? [c]
  (>= (cs/index-of UPPER (upcase c)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-digit? [c] (contains? DIGITS c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-alnum?
  "" [c] (or (is-alpha? c) (is-digit? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-white?
  "" [c] (contains? #{" " "\t" "\r" "\n"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-mulop?
  "" [c] (contains? #{"*" "/"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-orop?
  "" [c] (contains? #{"|" "~"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-relop?
  "" [c] (contains? #{"=" "#" "<" ">"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- skip-white
  "" [] (while (is-white? CH) (get-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-name []
  (skip-white)
  (if-not (is-alpha? CH)
    (expected!! "Identifier"))
  (set! TOKEN "")
  (set! VALUE "")
  (loop [v "" dummy nil]
    (if-not (is-alnum? CH)
      (set! VALUE v)
      (recur (str v (upcase CH)) (get-char)))) VALUE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-num []
  (skip-white)
  (if-not (is-digit? CH)
    (expected!! "Number"))
  (set! TOKEN "#")
  (set! VALUE "")
  (loop [v "" dummy nil]
    (if-not (is-digit? CH)
      (set! VALUE v)
      (recur (str v CH) (get-char)))) VALUE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-op []
  (skip-white) (set! TOKEN CH) (set! VALUE CH) (get-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- next-token []
  (skip-white)
  (cond
    (is-alpha? CH) (get-name)
    (is-digit? CH) (get-num)
    :else (get-op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- lookup?? [arr s]
  (loop [i 0
         SZ (n# arr) found? false]
    (if (or (>= i SZ) found?)
      (if found? (o- i) -1)
      (recur (o+ i) SZ (= s (nth arr i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- in-table? [n]
  (>= (lookup?? SYMBOLS n) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-table
  [n] (if-not (in-table? n) (undefined!! n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-dup
  "" [n] (if (in-table? n) (duplicate!! n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add-entry [N T]
  (check-dup N)
  (if (>= NEntry MAX-ENTRY)
    (abort!! "Symbol Table Full"))
  (aset SYMTYPES NEntry T)
  (aset SYMBOLS NEntry N)
  (set! NEntry (o+ NEntry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- scan "" []
  (if (= TOKEN "")
    (set! TOKEN
          (or (KEYWORDS VALUE) ""))) TOKEN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- match-string [x]
  (if (not= VALUE x) (expected!! (str "`" x "`"))) (next-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- is-addop? [c]
  (contains? #{"+" "-"} c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- new-label [] "@L0000")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- emit [s] (write "\t" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- emitln [s] (emit s) (writeln))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- init []
  (doseq [n (range MAX-ENTRY)]
    (aset SYMBOLS n "")
    (aset SYMTYPES n " ")) (get-char) (next-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- post-label [L] (writeln L ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- header []
  (writeln "Place-holder for MASM start-up code")
  (emitln "LIB TINYLIB"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prolog [] (post-label "MAIN"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- epilog [] (emitln "Place-holder for epilog"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- allocate [name val]
  (writeln "Var " name " : integer = " val ";"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- alloc []
  (next-token)
  (if (not= TOKEN "")
    (expected!! "Variable Name"))
  (check-dup VALUE)
  (add-entry VALUE "v")
  (allocate VALUE "0")
  (next-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- top-decls []
  (scan)
  (while (= TOKEN "v") (alloc))
  (while (= TOKEN ",") (alloc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clear [] (emitln "XOR EAX, EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- negate [] (emitln "NEG EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- load-const [n] (emitln (str "MOV EAX, " n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- load-var [name]
  (if-not (in-table? name) (undefined!! name))
  (emitln (str "MOV EAX, " name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- push [] (emitln "PUSH EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-add []
  (emitln "POP EDX")
  (emitln "ADD EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-sub []
  (emitln "POP EDX")
  (emitln "SUB EAX, EDX")
  (emitln "NEG EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-mul []
  (emitln "POP EDX")
  (emitln "IMUL EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-div []
  (emitln "MOV ECX, EAX")
  (emitln "POP EAX")
  (emitln "XOR EDX, EDX") ;;Clear EDX
  (emitln "IDIV ECX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- store [name] (emitln (str "MOV " name ", EAX")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- not-it [] (emitln "NOT EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read-it [name] (emitln "CALL READ") (store name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- write-it [] (emitln "CALL WRITE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-and []
  (emitln "POP EDX")
  (emitln "AND EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-or []
  (emitln "POP EDX")
  (emitln "OR EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-xor []
  (emitln "POP EDX")
  (emitln "XOR EAX, EDX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pop-compare []
  (emitln "POP EDX")
  (emitln "CMP EDX, EAX"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-equal []
  (emitln "CMOVE EAX, T")
  (emitln "CMOVNE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-nequal []
  (emitln "CMOVE EAX, F")
  (emitln "CMOVNE EAX, T"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-greater "" []
  (emitln "CMOVG EAX, T")
  (emitln "CMOVLE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-less "" []
  (emitln "CMOVL EAX, T")
  (emitln "CMOVGE EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-less-or-equal []
  (emitln "CMOVLE EAX, T")
  (emitln "CMOVG EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-greater-or-equal []
  (emitln "CMOVGE EAX, T")
  (emitln "CMOVL EAX, F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- branch [L] (emitln (str "JMP " L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- branch-false [L]
  (emitln "TEST EAX, -1")
  (emitln (str "JE " L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- compare-expression []
  (expression)
  (pop-compare))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- next-expression []
  (next-token)
  (compare-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- equals []
  (next-expression)
  (set-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- less-or-equal []
  (next-expression)
  (set-less-or-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- not-equal []
  (next-expression)
  (set-nequal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- less []
  (next-token)
  (case TOKEN
    "=" (less-or-equal)
    ">" (not-equal)
    (do (compare-expression) (set-less))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- greater []
  (next-token)
  (if (= TOKEN "=")
    (do (next-expression) (set-greater-or-equal))
    (do (compare-expression) (set-greater))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- relation []
  (expression)
  (when (is-relop? TOKEN)
    (push)
    (case TOKEN
      "=" (equals)
      "<" (less)
      ">" (greater)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- not-factor []
  (if (= TOKEN "!")
    (do (next-token) (relation) (not-it)) (relation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bool-term []
  (not-factor)
  (while (= TOKEN "&")
    (push)
    (next-token)
    (not-factor)
    (pop-and)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bool-or []
  (next-token)
  (bool-term)
  (pop-or))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bool-xor []
  (next-token)
  (bool-term)
  (pop-xor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bool-expression []
  (bool-term)
  (while (is-orop? TOKEN)
    (push)
    (case TOKEN "|" (bool-or) "~" (bool-xor) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- do-if []
  (next-token)
  (bool-expression)
  (let [L1 (new-label)
        _ (branch-false L1)
        _ (block)
        L2 (if (= TOKEN "l")
             (let [_ (next-token)
                   L2 (new-label)]
               (branch L2)
               (post-label L1)
               (block) L2) L1)]
    (post-label L2)
    (match-string "ENDIF")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- do-while []
  (next-token)
  (let [L1 (new-label)
        L2 (new-label)]
    (post-label L1)
    (bool-expression)
    (branch-false L2)
    (block)
    (match-string "ENDWHILE")
    (branch L1)
    (post-label L2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- factor []
  (if (= TOKEN "(")
    (do (next-token)
        (bool-expression)
        (match-string ")"))
    (do (case TOKEN
          "" (load-var VALUE)
          "#" (load-const VALUE)
          (expected!! "Math Factor")) (next-token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mult "" []
  (next-token)
  (factor)
  (pop-mul))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- div "" []
  (next-token)
  (factor)
  (pop-div))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- term "" []
  (factor)
  (while (is-mulop? TOKEN)
    (push)
    (case TOKEN "*" (mult) "/" (div) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- add []
  (next-token)
  (term)
  (pop-add))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- subtract []
  (next-token)
  (term)
  (pop-sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- expression []
  (if (is-addop? TOKEN) (clear) (term))
  (while (is-addop? TOKEN)
    (push)
    (case TOKEN "+" (add) "-" (subtract) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- assignment []
  (check-table VALUE)
  (let [name VALUE]
    (next-token)
    (match-string "=")
    (bool-expression)
    (store name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- read-var []
  (check-ident)
  (check-table VALUE)
  (read-it VALUE)
  (next-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- write-var [] (emitln "CALL WRITE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- do-read []
  (next-token)
  (match-string "(")
  (read-var)
  (while (= TOKEN ",")
    (next-token)
    (read-var))
  (match-string ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- do-write "" []
  (next-token)
  (match-string "(")
  (expression)
  (write-it)
  (while (= TOKEN ",")
    (next-token)
    (expression)
    (write-it))
  (match-string ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- block "" []
  (scan)
  (while (not (contains? #{"e" "l"} TOKEN))
    (case TOKEN
      "i" (do-if)
      "w" (do-while)
      "R" (do-read)
      "W" (do-write)
      (assignment))
    (scan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn main "" []
  (init)
  (match-string "PROGRAM")
  (header)
  (top-decls)
  (match-string "BEGIN")
  (prolog)
  (block)
  (match-string "END")
  (epilog)
  (readln)
  (readln))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


