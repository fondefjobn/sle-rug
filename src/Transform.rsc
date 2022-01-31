module Transform

import Syntax;
import Resolve;
import AST;
import ParseTree;

/* 
 * Transforming QL forms
 */
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f)
  = form(f.name, questions(flatten(f.q, boolean(true))));

list[AQuestion] flatten(AQuestion question, AExpr guard) {
  switch (question) {
    case q:question(_, _, _): return [ifstatement(guard, q)];
    case q:assignQuestion(_, _, _, _): return [ifstatement(guard, q)];
    case questions(list[AQuestion] qs): {
      list[AQuestion] toReturn = [];
      for (q <- qs)
        toReturn += flatten(q, guard);
      return toReturn;
    }
    case ifstatement(AExpr g, AQuestion ifPart):
      return flatten(ifPart, and(guard, g));
    case ifelse(AExpr guar, AQuestion ifPart, AQuestion elsePart):
      return flatten(ifPart, and(guard, g)) += flatten(elsePart, and(guard, not(g)));
  }
  return [];
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently
 * renames all occurrences of the same name.
 * Use the results of name resolution to find
 * the equivalence class of a name.
 *
 */
 
 start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
   set[loc] toRename = {useOrDef};
   
   toRename += {l | <loc l, useOrDef> <- useDef};
   toRename += {l | <useOrDef, loc l> <- useDef};
   
   return visit (f) {
     case Ident x => [Ident]newName
       when x@\loc in toRename
   } 
 }
 

