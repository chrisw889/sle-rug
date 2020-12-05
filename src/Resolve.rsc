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

  visit (f){
    case q:AQ(_, AId id, _):
      u += { <q.src, id.name> };
    case q:AQAssign(_, AId id, _, _):
      u += { <q.src, id.name> };
  }

  return u; 
}

Def defs(AForm f) {
  Def d = {};
  
  visit(f){
  	case r:ref(AId id):
  	  d += { <id.name, r.src> };
  }
  
  return d;
}