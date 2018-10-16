
function Token (type, lexeme) {
  return {
    type: type,
		lexeme: lexeme
  };
}


function Node (type, value) {
  return {
    children: [],
		type: type,
    value: value,
    addChild: function(c) {
		  this.children.push(c);
    }
  };
}

function Terminal(t) {
  return Node(t.type, t.lexeme);
}

function NonTerminal(t) {
  return Node(t.type);
}

const PRIMARY=0;

const VAR=1;
const NUM=2;
const PLUS=3;
const MINUS=4;
const MULT=5;
const DIV=6;
const POW=7;
const NOT=8;
const COMMA=9;



const LPAREN=100;
const RPAREN=101;

const EOF=Token(-911);


function toToken(s) {
  let ret;
  if (/^[a-zA-Z_][a-zA-Z_0-9]+$/.test(s)) {
    ret=Token(VAR,s);
  } else if (/^\d+$/.test(s)) {
    ret=Token(NUM, parseInt(s));
  } else if ("+" === s) {
    ret=Token(PLUS, "+");
  } else if ("-" === s) {
    ret=Token(MINUS, "-");
  } else if ("*" === s) {
    ret=Token(MULT, "*");
  } else if ("/" === s) {
    ret=Token(DIV, "/");
  } else if ("!" === s) {
    ret=Token(NOT, "!");
  } else if ("**" === s) {
    ret=Token(POW, "**");
  } else if ("," === s) {
    ret=Token(COMMA, ",");
  } else if ("("===s) {
    ret=Token(LPAREN, "(");
  } else if (")"===s) {
    ret=Token(RPAREN, ")");
  }else {
    throw "Bad Lexeme: " + s;
  }
  return ret;
}

function Lexer (source) {
  let a = (source || "").
    split(/\s+/g).
    filter(x => { return x && x.length > 0; }).
    map(toToken);
  //console.log(a);
  return {
    tokens: a,
		cur: 0,
    next: function () {
      let ret= EOF;
      if (this.cur < this.tokens.length) {
        let i= this.cur;
        this.cur++;
		    ret= this.tokens[i];
      }
      return ret;
    },
    peek: function () {
      return (this.cur < this.tokens.length) ? this.tokens[this.cur] : EOF;
    },
    poo:function() {
      while (true) {
        let x=this.peek();
        if (x===EOF) { break;}
        console.log("peek=");
        console.log(x);
        x=this.next();
        console.log("next=");
        console.log(x);
      }
    }
  };
}

function Parser (lexer) {
  return {
    lexer: lexer,
    parse: function () {
      return this.parseExpr({}, 0);
    },
    atom: function() {
      let x= this.lexer.peek();
      let ret=undefined;
      if (x) {
        switch (x.type) {
          case VAR: ret= x.lexeme; break;
          case NUM: ret= parseInt(x.lexeme); break;
        }
        if (ret !== undefined) {
          this.lexer.next();
          return ret;
        }
      }
      //console.log("x == " + x ? x.lexeme : "null!");
      throw "POO!";
    },
    parseGroup:function(state) {
      this.lexer.next(); // skip lparen
      let result = this.parseExpr(state,0);
      let op = this.lexer.peek();
      if (op.type === RPAREN) {
        this.lexer.next(); // skip rparen
      } else {
        throw "Missing RPAREN!";
      }
      return result;
    },
    parseFuncCall: function(id, env) {
      this.lexer.next(); // skip lparen
      let params=[];
      let t;
      let p= this.parseExpr(env,0);
      while (p !== undefined) {
        params.push(p);
        t=this.lexer.peek();
        if (t.type === RPAREN) { break; }
        if (t.type !== COMMA) {
          throw "Missing comma in func params: " + t.lexeme;
        }
        this.lexer.next();
        p= this.parseExpr(env,0);
      }
      t=this.lexer.peek();
      if (t.type !== RPAREN) {
        throw "BAD FUNC CALL, no RPAREN";
      }
      this.lexer.next();
      console.log("func= " + id);
      console.log(params);
      return 100;
    },
    parseExpr: function(state, min_prec) {
      let result=undefined;

      while (true) {
        let t= this.lexer.peek();
        let op,v,prec;

        if (t.type===RPAREN) { break; }
        if (t.type===COMMA) { break; }

        if (t.type===LPAREN) {
          result = this.parseGroup(state);
          op = this.lexer.peek();
        } else if (this.isOperator(t)) {
          if (!this.isUnary(t)) {
            throw "SHIT";
          }
          op=t;
        } else if (this.isAtom(t)) {
          if (t.type===VAR) {
            let id= this.lexer.next();
            let n= this.lexer.peek();
            if (n.type === LPAREN) {
              result=this.parseFuncCall(id, state);
            } else {
            }
          } else {
            result=this.atom();
          }
          op = this.lexer.peek();
        } else if (t === EOF) {
          break;
        } else {
          throw "Expecting ???, got " + t.lexeme;
        }

        if (t=== EOF || op === EOF) {break;}

        if (op.type===RPAREN ) { break; }
        if (op.type===COMMA ) { break; }

        if (!this.isOperator(op)) {
          throw "BAD OP: " + op.lexeme;
        }

        prec= this.prec(op);
        if (prec >= min_prec) {
          this.lexer.next(); // skip the op
          rhs= this.parseExpr(state, this.assocLeft(op) ? prec+1 : prec);
          result = this.eval(op, result, rhs);
        } else {
          break;
        }
      }

      return result;
    },
    eval: function(op, lhs, rhs) {
      switch (op.type) {
        case PLUS: return lhs + rhs;
        case MINUS: return lhs - rhs;
        case MULT: return lhs * rhs;
        case DIV: return lhs / rhs;
        case POW: return lhs ** rhs;
        case NOT: return !rhs;
      }
      throw "EVAL FAILED!";
    },
    isUnary: function(t) {
      switch (t.type) {
        case NOT: return true;
      }
      return false;
    },
    isAtom: function (t) {
      switch (t.type) {
        case VAR: case NUM: return true;
      }
      return false;
    },
    isOperator :function (t) {
		  switch (t.type) {
        case POW: case NOT:
        case PLUS: case MINUS: case MULT: case DIV: return true;
      }
      return false;
    },
    prec: function(t) {
      switch (t.type) {
        case PLUS: case MINUS: return 10;
        case MULT: case DIV: return 20;
        case POW: return 30;
        case NOT: return 50;
      }
      throw "WTF!";
    },
    assocLeft: function(t) {
      switch (t.type) {
        case PLUS:
        case MINUS:
        case MULT:
        case DIV: return true;

        case NOT:
        case POW: return false;
      }
      throw "POO ASSOC!";
    }
  };
}

//let lexer = Lexer("  1 + 2  ** ( 1 + 2 )");
//let lexer = Lexer("  ! ( ! ( 1  ) ) ");
//let lexer = Lexer("  foo ( 1 + 2 , 3 + 4 ) ");
let lexer = Lexer("  foo (  poo (  ) , 3 + 4 ) ");
let parser = Parser(lexer);
let tree = parser.parse();
console.log("result = ", tree);


