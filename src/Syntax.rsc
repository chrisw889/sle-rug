module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

// Concrete syntax root of the form
start syntax Form 
  = "form" Id name "{" Question* qs "}"; 

// Concrete syntax of question, computed question, block, if-then-else and if-then
syntax Question
  = Str text Id def ":" Type type
  | Str text Id def ":" Type type "=" Expr assign
  | "if" "(" Expr guard ")" "{"Question* ifYes "}"
  | "if" "(" Expr guard ")" "{" Question* ifYes "}" "else" "{" Question* ifNo "}";

// Concrete syntax of exressions of +, -, *, /, &&, ||, !, >, <, <=, >=, ==, != and literals (bool, int, str)
syntax Expr 
  = \id: Id ref \ "true" \ "false"  // reserved keywords.
  | \bool: Bool bool
  | \num: Int num
  | \str: Str str
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
  
// Concrete syntax of literal types
syntax Type
  = "boolean"
  | "integer"
  | "string";  
  
// String regular expression
lexical Str 
  = [\"] ![\"]* [\"];

// positive/negative integer value regular expression
lexical Int 
  = "-"?[1-9][0-9]*
  | [0];

// boolean regular expression
lexical Bool = "true" | "false";