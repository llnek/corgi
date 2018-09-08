;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.crenshaw.tiny

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

;Symbol = string[8];
;SymTab = array[1 .. 1000] of Symbol;
;TabPtr = ^SymTab;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def NKW 11)
(def NKW1 12)
(def KWlist #{"IF" "ELSE" "ENDIF" "WHILE"
              "ENDWHILE" "READ"
              "WRITE" "VAR" "BEGIN"
              "END" "PROGRAM"})
(def KWcode "xileweRWvbep")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *ch* "")
(def *token* "")
;Value : string[16];      { Unencoded token     }
;ST : array[1 .. MaxEntry] of Symbol;
;SType: array[1 .. MaxEntry] of char;
;LCount : integer;
;NEntry : integer = 0;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare expression)


(defn- getChar "" [] (set! *ch* (read)))


(defn- error "" [s]
  (writeLn)
  (writeLn "Error: " s ".")
  (readLn)
  (readLn))



(defn- abort "" [s]
  (error s)
  (halt))


(defn- undefined "" [n]
  (abort (str "Undefined Identifier " n)))



(defn- expected "" [s]
  (abort (str s " Expected")))



(defn- isAlpha? "" [c]
  (>= (js/String.prototype.indexOf.call UPPER (upCase c)) 0))


(defn- isDigit? "" [c] (contains? DIGITS c))


(defn- isAlNum? "" [c]
  (or (isAlpha? c) (isDigit? c)))



(defn- isWhite? "" [c]
  (contains? #{" " "\t"} c))


(defn- isMulop? "" [c]
  (contains? #{"*" "/"} c))


(defn- isOrop? "" [c]
  (contains? #{"|" "~"} c))


{ Recognize a relop }
function IsRelop(c : char) : boolean;
begin
  IsRelop := c in ['=', '#', '<', '>'];
end;

{ Skip over leading white space }
procedure SkipWhite;
begin
  while IsWhite(Look) do
    GetChar;
end;

{ Skip over an end-of-line }
procedure NewLine;
begin
  while Look = CR do begin
    GetChar;
    if Look = LF then
      GetChar;
    SkipWhite;
  end;
end;

{ Match a specific input character }
procedure Match(x : char);
begin
  NewLine;
  if Look = x then
    GetChar
  else
    Expected('''' + x + '''');
  SkipWhite;
end;


{ Get an Identifier }
procedure GetName;
begin
  NewLine;
  if not IsAlpha(Look) then
    Expected('Name');
  Value := '';
  while IsAlNum(Look) do
    begin
      Value := Value + UpCase(Look);
      GetChar;
    end;
  SkipWhite;
end;

{ Get a number }
function GetNum : integer;
var
  Val : integer;
begin
  NewLine;
  Val := 0;
  if not IsDigit(Look) then
    Expected('Integer');
  while IsDigit(Look) do
    begin
      Val := 10 * Val + Ord(Look) - Ord('0');
      GetChar;
    end;
  GetNum := Val;
  SkipWhite;
end;

{ Table lookup }
function Lookup(T : TabPtr; s : string; n : integer) : integer;
var
  i : integer;
  found : Boolean;
begin
  found := false;
  i := n;
  while (i > 0) and not found do
    if s = T^[i] then
      found := true
    else
      dec(i);
  Lookup := i;
end;

{ Look for symbol in table }
function InTable(n : Symbol) : Boolean;
begin
  InTable := Lookup(@ST, n, MaxEntry) <> 0;
end;

{ Add entry to table }
procedure AddEntry(N : Symbol; T : char);
begin
  if InTable(N) then
    Abort('Duplicate Identifier ' + N);
  if NEntry = MaxEntry then
    Abort('Symbol Table Full');
  Inc(NEntry);
  ST[NEntry] := N;
  SType[NEntry] := T;
end;

{ Get an identifier and scan it for keywords }
procedure Scan;
begin
  GetName;
  Token := KWcode[Lookup(Addr(KWlist), Value, NKW) + 1];
end;

{ Match a specific input string }
procedure MatchString(x : string);
begin
  if Value <> x then
    Expected('''' + x + '''');
end;

{ Recognize an addop }
function IsAddop(c : char): boolean;
begin
  IsAddop := c in ['+', '-'];
end;

{ Generate a unique label }
function NewLabel : string;
var
  S : string;
begin
  Str(LCount, S);
  NewLabel := '@L' + S;
  Inc(LCount);
end;

{ Output a string with tab }
procedure Emit(s : string);
begin
  Write(TAB, s);
end;

{ Output a string with tab and CRLF }
procedure EmitLn(s : string);
begin
  Emit(s);
  WriteLn;
end;

{ Initialize }
procedure Init;
var
  i : integer;
begin
   LCount := 0;
   for i := 1 to MaxEntry do
     begin
       ST[i] := '';
       SType[i] := ' ';
     end;
  GetChar;
  Scan;
end;

{ Skip a CRLF }
procedure Fin;
begin
  if Look = CR then
    GetChar;
  if Look = LF then
    GetChar;
end;

{ Post a label To output }
procedure PostLabel(L : string);
begin
  WriteLn(L, ':');
end;

{ Write header info }
procedure Header;
begin
  WriteLn('Place-holder for MASM start-up code');
  EmitLn('LIB TINYLIB');
end;

{ Write the prolog }
procedure Prolog;
begin
  PostLabel('MAIN');
end;

{ Write the epilog }
procedure Epilog;
begin
  EmitLn('Place-holder for epilog');
end;

{ Allocate storage for a variable }
procedure Alloc(N : Symbol);
begin
  if InTable(N) then
    Abort('Duplicate Variable Name ' + N);
  AddEntry(N, 'v');
  Write('var ' + N  + ' : integer = ');
  if Look = '=' then
    begin
      Match('=');
      if Look = '-' then
        begin
          Write(Look);
          Match('-');
        end;
      WriteLn(GetNum, ';');
    end
  else
    WriteLn('0;');
end;

{ Parse and translate a data declaration }
procedure Decl;
begin
  GetName;
  Alloc(Value);
  while Look = ',' do
    begin
      Match(',');
      GetName;
      Alloc(Value);
    end;
end;

{ Parse and translate global declarations }
procedure TopDecls;
begin
  Scan;
  while Token <> 'b' do
    begin
      case Token of
        'v' : Decl;
      else
        Abort('Unrecognized Keyword ' + Value);
      end;
      Scan;
    end;
end;

{ Clear the primary register }
procedure Clear;
begin
  EmitLn('XOR EAX, EAX');
end;

{ Negate the primary register }
procedure Negate;
begin
  EmitLn('NEG EAX');
end;

{ Load a constant value to primary register }
procedure LoadConst(n: integer);
begin
  Emit('MOV EAX, ');
  WriteLn(n);
end;

{ Load a variable to primary register }
procedure LoadVar(Name : string);
begin
  if not InTable(Name) then
    Undefined(Name);
  EmitLn('MOV EAX, ' + Name);
end;

{ Push primary onto stack }
procedure Push;
begin
  EmitLn('PUSH EAX');
end;

{ Add top of stack to primary }
procedure PopAdd;
begin
  EmitLn('POP EDX');
  EmitLn('ADD EAX, EDX');
end;

{ Subtract primary from top of stack }
procedure PopSub;
begin
  EmitLn('POP EDX');
  EmitLn('SUB EAX, EDX');
  EmitLn('NEG EAX');
end;

{ Multiply top of stack by primary }
procedure PopMul;
begin
  EmitLn('POP EDX');
  EmitLn('IMUL EDX');
end;

{ Divide top of stack by primary }
procedure PopDiv;
begin
  EmitLn('MOV ECX, EAX');
  EmitLn('POP EAX');
  EmitLn('XOR EDX, EDX'); //Clear EDX
  EmitLn('IDIV ECX');
end;

{ Store primary to variable }
procedure Store(Name : string);
begin
  if not InTable(Name) then
    Undefined(Name);
  EmitLn('MOV ' + Name + ', EAX');
end;

{ Complement the primary register }
procedure NotIt;
begin
  EmitLn('NOT EAX');
end;

{ AND top of stack with primary }
procedure PopAnd;
begin
  EmitLn('POP EDX');
  EmitLn('AND EAX, EDX');
end;

{ OR top of stack with primary }
procedure PopOr;
begin
  EmitLn('POP EDX');
  EmitLn('OR EAX, EDX');
end;

{ XOR top of stack with primary }
procedure PopXor;
begin
  EmitLn('POP EDX');
  EmitLn('XOR EAX, EDX');
end;

{ Compare top of stack with primary }
procedure PopCompare;
begin
  EmitLn('POP EDX');
  EmitLn('CMP EDX, EAX');
end;

{ Set EAX if compare was = }
procedure SetEqual;
begin
  EmitLn('CMOVE EAX, T');
  EmitLn('CMOVNE EAX, F');
end;

{ Set EAX if compare was != }
procedure SetNEqual;
begin
  EmitLn('CMOVE EAX, F');
  EmitLn('CMOVNE EAX, T');
end;

{ Set EAX if compare was > }
procedure SetGreater;
begin
  EmitLn('CMOVG EAX, T');
  EmitLn('CMOVLE EAX, F');
end;

{ Set EAX if compare was < }
procedure SetLess;
begin
  EmitLn('CMOVL EAX, T');
  EmitLn('CMOVGE EAX, F');
end;

{ Set EAX if compare was <= }
procedure SetLessOrEqual;
begin
  EmitLn('CMOVLE EAX, T');
  EmitLn('CMOVG EAX, F');
end;

{ Set EAX if compare was >= }
procedure SetGreaterOrEqual;
begin
  EmitLn('CMOVGE EAX, T');
  EmitLn('CMOVL EAX, F');
end;

{ Branch unconditional }
procedure Branch(L : string);
begin
  EmitLn('JMP ' + L);
end;

{ Branch False }
procedure BranchFalse(L : string);
begin
  EmitLn('TEST EAX, -1');
  EmitLn('JE ' + L);
end;

{ Recognize and translate a relational "Equals" }
procedure Equals;
begin
  Match('=');
  Expression;
  PopCompare;
  SetEqual;
end;

{ Recognize and translate a relational "Less Than or Equal" }
procedure LessOrEqual;
begin
  Match('=');
  Expression;
  PopCompare;
  SetLessOrEqual;
end;

{ Recognize and translate a relational "Not Equals" }
procedure NotEqual;
begin
  Match('>');
  Expression;
  PopCompare;
  SetNEqual;
end;

{ Recognize and translate a relational "Less Than" }
procedure Less;
begin
  Match('<');
  case Look of
    '=' : LessOrEqual;
    '>' : NotEqual;
  else
    begin
      Expression;
      PopCompare;
      SetLess;
    end;
  end;
end;

{ Recognize and translate a relational "Greater Than" }
procedure Greater;
begin
  Match('>');
  if Look = '=' then
    begin
      Match('=');
      Expression;
      PopCompare;
      SetGreaterOrEqual;
    end
  else
    begin
      Expression;
      PopCompare;
      SetGreater;
    end;
end;

{ Parse and translate a relation }
procedure Relation;
begin
  Expression;
  if IsRelop(Look) then
    begin
      Push;
      case Look of
        '=' : Equals;
        '#' : NotEqual;
        '<' : Less;
        '>' : Greater;
      end;
    end;
end;

{ Parse and translate a Boolean factor with leading NOT }
procedure NotFactor;
begin
  if Look = '!' then
    begin
      Match('!');
      Relation;
      NotIt;
    end
  else
    Relation;
end;

{ Parse and translate a Boolean Term }
procedure BoolTerm;
begin
  NewLine;
  NotFactor;
  while Look = '&' do
    begin
      Push;
      Match('&');
      NotFactor;
      PopAnd;
      NewLine;
    end;
end;

{ Recognize and translate a Boolean OR }
procedure BoolOr;
begin
  Match('|');
  BoolTerm;
  PopOr;
end;

{ Recognize and translate an exclusive Or }
procedure BoolXor;
begin
  Match('~');
  BoolTerm;
  PopXor;
end;

{ Parse and translate a Boolean expression }
procedure BoolExpression;
begin
  NewLine;
  BoolTerm;
  while IsOrOp(Look) do
    begin
      Push;
      case Look of
        '|' : BoolOr;
        '~' : BoolXor;
      end;
      NewLine;
    end;
end;

{ Recognize and translate an IF construct }
procedure Block; Forward;
procedure DoIf;
var
  L1, L2 : string;
begin
  BoolExpression;
  L1 := NewLabel;
  L2 := L1;
  BranchFalse(L1);
  Block;
  if Token = 'l' then
    begin
      L2 := NewLabel;
      Branch(L2);
      PostLabel(L1);
      Block;
    end;
  PostLabel(L2);
  MatchString('ENDIF');
end;

{ Parse and translate a WHILE statement }
procedure DoWhile;
var
  L1, L2 : string;
begin
  L1 := NewLabel;
  L2 := NewLabel;
  PostLabel(L1);
  BoolExpression;
  BranchFalse(L2);
  Block;
  MatchString('ENDWHILE');
  Branch(L1);
  PostLabel(L2);
end;

{ Parse and translate a maths factor }
procedure Factor;
begin
  if Look = '(' then
    begin
      Match('(');
      BoolExpression;
      Match(')');
    end
  else if IsAlpha(Look) then
    begin
      GetName;
      LoadVar(Value);
    end
  else
    LoadConst(GetNum);
end;

{ Parse and translate a negative factor }
procedure NegFactor;
begin
  Match('-');
  if IsDigit(Look)
    then
      LoadConst(-GetNum)
  else
    begin
      Factor;
      Negate;
    end;
end;

{ Parse and translate a leading factor }
procedure FirstFactor;
begin
  case Look of
    '+' : begin
            Match('+');
            Factor;
          end;
    '-' : NegFactor;
  else
    Factor;
  end;
end;

{ Recognize and translate a multiply }
procedure Multiply;
begin
  Match('*');
  Factor;
  PopMul;
end;

{ Recognize and translate a divide }
procedure Divide;
begin
  Match('/');
  Factor;
  PopDiv;
end;

{ Common code used by Term and FirstTerm }
procedure Term1;
begin
  NewLine;
  while IsMulop(Look) do
    begin
      Push;
      case Look of
        '*' : Multiply;
        '/' : Divide;
      end;
      NewLine;
   end;
end;

{ Parse and translate a maths term }
procedure Term;
begin
  Factor;
  Term1;
end;

{ Parse and translate a leading term }
procedure FirstTerm;
begin
  FirstFactor;
  Term1;
end;

{ Recognize and translate an add }
procedure Add;
begin
  Match('+');
  Term;
  PopAdd;
end;

{ Recognize and translate a subtract }
procedure Subtract;
begin
  Match('-');
  Term;
  PopSub;
end;

{ Parse and translate an expression }
procedure Expression;
begin
  NewLine;
  FirstTerm;
  while IsAddop(Look) do
    begin
      Push;
      case Look of
        '+' : Add;
        '-' : Subtract;
      end;
      NewLine;
    end;
end;

{ Parse and translate an assignment statement }
procedure Assignment;
var
  Name : string ;
begin
  Name := Value;
  Match('=');
  BoolExpression;
  Store(Name);
end;

{ Read variable to primary register }
procedure ReadVar;
begin
  EmitLn('CALL READ');
  Store(Value);
end;

{ Write variable from primary register }
procedure WriteVar;
begin
  EmitLn('CALL WRITE');
end;

{ Process a read statement }
procedure DoRead;
begin
  Match('(');
  GetName;
  ReadVar;
  while Look = ',' do
    begin
      Match(',');
      GetName;
      ReadVar;
    end;
  Match(')');
end;

{ Process a write statement }
procedure DoWrite;
begin
  Match('(');
  Expression;
  WriteVar;
  while Look = ',' do
    begin
      Match(',');
      Expression;
      WriteVar;
    end;
  Match(')');
end;

{ Parse and translate a block of statements }
procedure Block;
begin
  Scan;
  while not (Token in ['e', 'l']) do
    begin
      case Token of
        'i' : DoIf;
        'w' : DoWhile;
        'R' : DoRead;
        'W' : DoWrite;
      else
        Assignment;
      end;
      Scan;
   end;
end;

{ Parse and translate a main program }
procedure Main;
begin
  MatchString('BEGIN');
  Prolog;
  Block;
  MatchString('END');
  Epilog;
end;

{ Parse and translate a program }
procedure Prog;
begin
  MatchString('PROGRAM');
  Header;
  TopDecls;
  Main;
  Match('.');
end;

{ Main program }
begin
  Init;
  Prog;
  if Look <> CR then
    Abort('Unexpected data after ''.''');
  ReadLn;
  ReadLn;
end.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

