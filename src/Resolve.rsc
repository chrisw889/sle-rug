module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f) {
  Use u = {};
  
  visit(f){
  	case ref(AId id):
  	  u += { <id.src, id.name> };
  }

  return u; 
}

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