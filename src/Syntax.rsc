module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id ":" Type
  | Str Id ":" Type "=" Expr
  | "if" "(" Expr ")" "{" Question* "}"
  | "if" "(" Expr ")" "{" Question* "}" "else" "{" Question* "}";

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = \id: Id \ "true" \ "false" \ "form" \ "if" \ "else"  // reserved keywords.
  | \bool: Bool
  | \int: Int
  > left par: "(" Expr ")"
  > right not: "!" Expr
  > left mult: Expr "*" Expr
  > left div: Expr "/" Expr
  > left add: Expr "+" Expr
  > left sub: Expr "-" Expr
  > non-assoc (
    gt: Expr "\>" Expr
  | lt: Expr "\<" Expr
  | geq: Expr "\>=" Expr
  | leq: Expr "\<=" Expr
  )
  > left eq: Expr "==" Expr
  > left neq: Expr "!=" Expr
  > left and: Expr "&&" Expr
  > left or: Expr "||" Expr
  ;
  
syntax Type
  = "boolean"
  | "integer"
  | "string";  
  
lexical Str 
  = [\"] ![\"]* [\"];

lexical Int 
  = [1-9][0-9]*
  | [0];

lexical Bool = "true" | "false";