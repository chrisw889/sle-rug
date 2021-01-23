module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

// relation of use and definition pairs
alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

/* 
 * reference graph construction from ast form.
 * calls use and definition generators then creates use-definition 
 * relation by relation composition
 */
RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);


// All uses found by visit in case of ref() ast object (used in an expression)
Use uses(AForm f) {
  Use u = {};
  
  visit(f){
  	case ref(AId id):
  	  u += { <id.src, id.name> };
  }

  return u; 
}

// All definitions found by visit in case of question or computed question
Def defs(AForm f) {
  Def d = {};
  
  visit (f){
    case AQ(_, AId id, _):
      d += { <id.name, id.src> };
    case AQAssign(_, AId id, _, _):
      d += { <id.name, id.src> };
  }
  
  return d;
}