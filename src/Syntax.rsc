module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id name "{" Question* qs "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str text Id def ":" Type type
  | Str text Id def ":" Type type "=" Expr assign
  | "if" "(" Expr guard ")" "{"Question* ifYes "}"
  | "if" "(" Expr guard ")" "{" Question* ifYes "}" "else" "{" Question* ifNo "}";

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = \id: Id ref \ "true" \ "false" \ "form" \ "if" \ "else" \ "boolean" \ "integer" \ "string"  // reserved keywords.
  | \bool: Bool bool
  | \num: Int num
  > left par: "(" Expr expr ")"
  > right not: "!" Expr expr
  > left mult: Expr lh "*" Expr rh
  > left div: Expr lh "/" Expr rh
  > left add: Expr lh "+" Expr rh
  > left sub: Expr lh "-" Expr rh
  > non-assoc (
    gt: Expr lh "\>" Expr rh
  | lt: Expr lh "\<" Expr rh
  | geq: Expr lh "\>=" Expr rh
  | leq: Expr lh "\<=" Expr rh
  )
  > left eq: Expr lh "==" Expr rh
  > left neq: Expr lh "!=" Expr rh
  > left and: Expr lh "&&" Expr rh
  > left or: Expr lh "||" Expr rh
  ;
  
syntax Type
  = "boolean"
  | "integer"
  | "string";  
  
lexical Str 
  = [\"] ![\"]* [\"];

lexical Int 
  = "-"?[1-9][0-9]*
  | [0];

lexical Bool = "true" | "false";