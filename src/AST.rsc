module AST

/*
 * Abstract Syntax for QL
 */

// Root of abstract syntax tree of form
data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ;

// Abstract syntax of question, computed question, block, if-then-else and if-then
data AQuestion(loc src = |tmp:///|)
  = AQ(str question, AId def, AType dataType)
  | AQAssign(str question, AId def, AType typ, AExpr expr)
  | AQIf(AExpr guard, list[AQuestion] ifYes)
  | AQIfElse(AExpr guard, list[AQuestion] ifYes, list[AQuestion] ifNo)
  ;

// Abstract syntax of exressions of +, -, *, /, &&, ||, !, >, <, <=, >=, ==, != and literals (bool, int, str)
data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | boolean(bool boolVal)
  | number(int numVal)
  | string(str strVal)
  | not(AExpr expr)
  | mult(AExpr exprL, AExpr exprR)
  | div(AExpr exprL, AExpr exprR)
  | add(AExpr exprL, AExpr exprR)
  | sub(AExpr exprL, AExpr exprR)
  | gt(AExpr exprL, AExpr exprR)
  | lt(AExpr exprL, AExpr exprR)
  | geq(AExpr exprL, AExpr exprR)
  | leq(AExpr exprL, AExpr exprR)
  | equ(AExpr exprL, AExpr exprR)
  | neq(AExpr exprL, AExpr exprR)
  | and(AExpr exprL, AExpr exprR)
  | or(AExpr exprL, AExpr exprR)
  ;

// Abstract syntax of available literal types
data AType(loc src = |tmp:///|)
  = integer()
  | boolean()
  | string();

// Abstract syntax of varible ID
data AId(loc src = |tmp:///|)
  = id(str name);