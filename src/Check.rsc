module Check


import AST;
import Resolve;
import Message; // see standard library
import Set;

// new Type ast used for static error check functions
data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// The type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// Function to collect type information of all questions, returned as type environment relation
TEnv collect(AForm f) {
  TEnv tenv = {};
  
  visit (f){
    case AQ(str label, AId id, AType dt):
    	tenv += { <id.src, id.name, label, typeOf(dt)> };
    case AQAssign(str label, AId id, AType dt, AExpr _):
    	tenv += { <id.src, id.name, label, typeOf(dt)> };
  }

  return tenv; 
}


/*
 * Static error check function to find errors based on ast AForm
 */

// Errors returned in set structure of error messages
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  return ( {} | it + check(q, tenv, useDef) | AQuestion q <- f.questions); // reduce statement to compile multiple check calls
}

// Static error check function for AQuestion ast
// Error produced if there are declared questions with the same name but different types.
// Error produced if there are duplicate labels
// Error produced if the declared type of a computed questions does not match the type of the expression.
// Error produced if the guard of an if or if-else if is not a boolean type
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  switch (q){
    case AQ(str label, i:id(str n), AType _):
      return {error("Duplicate question", i.src) | size({t | <_, n, _, Type t> <- tenv}) > 1}
      + {error("Duplicate question label", q.src) | size({l | <loc l, _, label, _> <- tenv}) > 1};
    case AQAssign(str label, i:id(str n), AType dt, AExpr expr):
      return {error("Duplicate question", i.src) | size({t | <_, n, _, Type t> <- tenv}) > 1}
      + {error("Duplicate question label", q.src) | size({l | <loc l, _, label, _> <- tenv}) > 1}
      + {error("Expression type does not match question type", expr.src) | typeOf(expr, tenv, useDef) != typeOf(dt)}
      + check(expr, tenv, useDef);
    case AQIf(AExpr guard, list[AQuestion] ifYes):
      return {error("If condition expression is not a boolean type", guard.src)| typeOf(guard, tenv, useDef) != tbool()}
      + ( {} | it + check(iy, tenv, useDef) | AQuestion iy <- ifYes );
    case AQIfElse(AExpr guard, list[AQuestion] ifYes, list[AQuestion] ifNo):
      return {error("If condition expression is not a boolean type", guard.src)| typeOf(guard, tenv, useDef) != tbool()}
      + ( {} | it + check(ify, tenv, useDef) | AQuestion ify <- ifYes )
      + ( {} | it + check(ifn, tenv, useDef) | AQuestion ifn <- ifNo );
      
    default: return {};
  }
}

// Static error check function for AExpr ast
// Error produced if id used with no question to declare it
// Error produced if operand types do not match with their operator and/or other operand
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(AId x):
      	return { error("Undeclared question", x.src) | useDef[x.src] == {} };
    case not(AExpr expr): 
    	return {error("Not expression input is not a boolean type", e.src) | typeOf(expr, tenv, useDef) != tbool()}
    	+ check(expr, tenv, useDef);
  	case mult(AExpr exprL, AExpr exprR):
  		return {error("Inputs of multiplication expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
    case div(AExpr exprL, AExpr exprR):
  		return {error("Inputs of division expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
    case add(AExpr exprL, AExpr exprR):
  		return {error("Inputs of addition expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case sub(AExpr exprL, AExpr exprR):
  		return {error("Inputs of subtraction expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case gt(AExpr exprL, AExpr exprR):
  		return {error("Inputs of greater than expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case lt(AExpr exprL, AExpr exprR):
  		return {error("Inputs of less than expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case geq(AExpr exprL, AExpr exprR):
  		return {error("Inputs of greater than or equal expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case leq(AExpr exprL, AExpr exprR):
  		return {error("Inputs of less than or euqal expression are not both integers", e.src) | typeOf(exprL, tenv, useDef) != tint() || typeOf(exprR, tenv, useDef) != tint()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case equ(AExpr exprL, AExpr exprR):
  		return {error("Inputs of equality expression are not the same type", e.src) | typeOf(exprL, tenv, useDef) !=  typeOf(exprR, tenv, useDef)}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case neq(AExpr exprL, AExpr exprR):
  		return {error("Inputs of not equal expression are not the same type", e.src) | typeOf(exprL, tenv, useDef) != typeOf(exprR, tenv, useDef)}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case and(AExpr exprL, AExpr exprR):
  		return {error("Inputs of and expression are not both booleans", e.src) | typeOf(exprL, tenv, useDef) != tbool() || typeOf(exprR, tenv, useDef) != tbool()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	case or(AExpr exprL, AExpr exprR):
  		return {error("Inputs of or expression are not both booleans", e.src) | typeOf(exprL, tenv, useDef) != tbool() || typeOf(exprR, tenv, useDef) != tbool()}
  		+ check(exprL, tenv, useDef)
  		+ check(exprR, tenv, useDef);
  	
  	default: return {};
  }
}

// Type checking function evaluates AExpr ast to find type
// In the case of a variable the useDef graph is used to find the type of the defined question
Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):
      if (<u, loc d> <- useDef, <d, _, _, Type t> <- tenv) {
        return t;
      }
    case boolean(_): return tbool();
    case number(_): return tint();
    case not(_): return tbool();
    case mult(_, _): return tint();
    case div(_, _): return tint();
  	case add(_, _): return tint();
  	case sub(_, _): return tint();
  	case gt(_, _): return tbool();
  	case lt(_, _): return tbool();
  	case geq(_, _): return tbool();
  	case leq(_, _): return tbool();
  	case equ(_, _): return tbool();
  	case neq(_, _): return tbool();
  	case and(_, _): return tbool();
  	case or(_, _): return tbool();
  	
  	default:
  	  return tunknown();
  }
  
  return tunknown();
}

// converting AType ast to Type ast used for check functions
Type typeOf(AType t){
  switch (t) {
  	case integer(): return tint();
  	case boolean(): return tbool();
  	case string(): return tstr();
  	
    default: return tunknown();
  }
}
 
 

