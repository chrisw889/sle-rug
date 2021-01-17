module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ;

data AQuestion(loc src = |tmp:///|)
  = AQ(str question, AId def, AType dataType)
  | AQAssign(str question, AId def, AType typ, AExpr expr)
  | AQIf(AExpr guard, list[AQuestion] ifYes)
  | AQIfElse(AExpr guard, list[AQuestion] ifYes, list[AQuestion] ifNo)
  ;

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

data AType(loc src = |tmp:///|)
  = integer()
  | boolean()
  | string();

data AId(loc src = |tmp:///|)
  = id(str name);