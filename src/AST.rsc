module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, AQuestion q)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str qq, AId id, AType t)
  | assignQuestion(str qq, AId id, AType t, AExpr expr)
  | questions(list[AQuestion] qs)
  | ifstatement(AExpr guard, AQuestion ifPart)
  | ifelse(AExpr guard, AQuestion ifPart, AQuestion elsePart)
  ;

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | boolean(bool b)
  | integer(int n)
  | string(str s)
  | not(AExpr expr)
  | times(AExpr left, AExpr right)
  | div(AExpr left, AExpr right)
  | plus(AExpr left, AExpr right)
  | minus(AExpr left, AExpr right)
  | l(AExpr left, AExpr right)
  | leq(AExpr left, AExpr right)
  | g(AExpr left, AExpr right)
  | geq(AExpr left, AExpr right)
  | eq(AExpr left, AExpr right)
  | neq(AExpr left, AExpr right)
  | and(AExpr left, AExpr right)
  | or(AExpr left, AExpr right)
  ;
  
data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = boolean()
  | integer()
  | string()
  ;
  