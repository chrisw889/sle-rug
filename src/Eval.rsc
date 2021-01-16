module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f)
  = ( n: typeDef(t) | /AQ(_, id(str n), AType t) <- f )
  + ( n: typeDef(t) | /AQAssign(_, id(str n), AType t, _) <- f );

Value typeDef(integer()) = vint(0);

Value typeDef(boolean()) = vbool(false);

Value typeDef(string()) = vstr("");

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv)
  = ( venv | eval(q, inp, it) | q <- f.questions );

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  switch (q){
    case AQ(str question, id(str name), _):
      if (true){
      	return venv + (name: inp.\value | inp.question == question);
      }
    case AQAssign(_, id(str name), _, AExpr expr):
      return venv + (name: eval(expr, venv));
    case AQIf(AExpr guard, list[AQuestion] ifYes):
      if (vbool(true) := eval(guard, venv)) {
        return ( venv | eval(ify, inp, it) | ify <- ifYes );
      } else {
      	return venv;
      }
    case AQIfElse(AExpr guard, list[AQuestion] ifYes, list[AQuestion] ifNo):
      if (vbool(true) := eval(guard, venv)) {
        return ( venv | eval(ify, inp, it) | ify <- ifYes );
      } else {
        return ( venv | eval(ify, inp, it) | ify <- ifNo );
      }
  }
  return venv;
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case boolean(bool b): return vbool(b);
    case number(int n): return vint(n);
    case not(AExpr ex): return vbool(!eval(ex, venv).b);
    case mult(AExpr lh, AExpr rh): return vint(eval(lh, venv).n * eval(rh, venv).n);
    case div(AExpr lh, AExpr rh): return vint(eval(lh, venv).n / eval(rh, venv).n);
    case add(AExpr lh, AExpr rh): return vint(eval(lh, venv).n + eval(rh, venv).n);
    case sub(AExpr lh, AExpr rh): return vint(eval(lh, venv).n - eval(rh, venv).n);
    case gt(AExpr lh, AExpr rh): return vbool(eval(lh, venv).n > eval(rh, venv).n);
    case lt(AExpr lh, AExpr rh): return vbool(eval(lh, venv).n < eval(rh, venv).n);
    case geq(AExpr lh, AExpr rh): return vbool(eval(lh, venv).n >= eval(rh, venv).n);
    case leq(AExpr lh, AExpr rh): return vbool(eval(lh, venv).n <= eval(rh, venv).n);
    case equ(AExpr lh, AExpr rh): {
    	evalL = eval(lh, venv);
    	switch(evalL){
    		case vint(int n): return vbool(n == eval(rh, venv).n);
    		case vbool(bool b): return vbool(b == eval(rh, venv).b);
    		default: return vbool(evalL.s == eval(rh, venv).s);
    	}
    }
    case neq(AExpr lh, AExpr rh): {
    	evalL = eval(lh, venv);
    	switch(evalL){
    		case vint(int n): return vbool(n != eval(rh, venv).n);
    		case vbool(bool b): return vbool(b != eval(rh, venv).b);
    		default: return vbool(evalL.s != eval(rh, venv).s);
    	}
    }
    case and(AExpr lh, AExpr rh): return vbool(eval(lh, venv).b && eval(rh, venv).b);
    case or(AExpr lh, AExpr rh): return vbool(eval(lh, venv).b || eval(rh, venv).b);
    
    default: throw "Unsupported expression <e>";
  }
}