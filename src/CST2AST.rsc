module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.name>", [ cst2ast(q) | Question q <- f.qs ], src=f@\loc); 
}

AQuestion cst2ast(qu: Question q) {
  switch (q){
    case (Question)`<Str text> <Id def> : <Type typ>`:
      return AQ("<text>", id("<def>", src=def@\loc), cst2ast(typ), src=qu@\loc);
    case (Question)`<Str text> <Id def> : <Type typ> = <Expr x>`:
      return AQAssign("<text>", id("<def>", src=def@\loc), cst2ast(typ), cst2ast(x), src=qu@\loc);
	case (Question)`if ( <Expr x> ) { <Question* qs> }`: 
      return AQIf(cst2ast(x), [ cst2ast(iq) | iq <- qs ], src=qu@\loc);
    case (Question)`if ( <Expr x> ) { <Question* qs1> } else { <Question* qs2> }`: 
      return AQIfElse(cst2ast(x), [ cst2ast(iq) | iq <- qs1 ], [ cst2ast(iq) | iq <- qs2 ], src=qu@\loc);
  	default: throw "Unsupported Question Form: <q>";
  }
}

AExpr cst2ast(expr: Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=e@\loc);
    case (Expr)`<Bool x>`: return boolean((Bool)`true` := x ? true : false, src=x@\loc);
    case (Expr)`<Int x>`: return number(toInt("<x>"), src=x@\loc);
    case (Expr)`( <Expr x> )`: return cst2ast(x);
    case (Expr)`! <Expr x>`: return not(cst2ast(x), src=expr@\loc);
    case (Expr)`<Expr x> * <Expr y>`: return mult(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> / <Expr y>`: return div(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> + <Expr y>`: return add(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> - <Expr y>`: return sub(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> \> <Expr y>`: return gt(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> \< <Expr y>`: return lt(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> \>= <Expr y>`: return geq(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> \<= <Expr y>`: return leq(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> == <Expr y>`: return equ(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> != <Expr y>`: return neq(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> && <Expr y>`: return and(cst2ast(x), cst2ast(y), src=expr@\loc);
    case (Expr)`<Expr x> || <Expr y>`: return or(cst2ast(x), cst2ast(y), src=expr@\loc);
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
  	case (Type)`boolean`: return boolean();
  	case (Type)`integer`: return integer();
  	case (Type)`string`: return string();
  	
  	default: throw "Unhandled Type: <t>";
  }
}
