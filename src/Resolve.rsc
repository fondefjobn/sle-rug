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

RefGraph resolve(AForm f)
  = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

//Use refers to state
Use uses(AForm f) 
  = {<id.src, id.name> | /ref(AId id) := f}; 

Def defs(AForm f)
  = {<id.name, id.src> | /question(_, AId id, _) := f}
  + {<id.name, id.src> | /assignQuestion(_, AId id, _, _) := f};
