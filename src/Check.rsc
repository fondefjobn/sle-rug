module Check

import AST;
import Resolve;
import Message; // see standard library

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv collection = {};
  visit(f) {
    case q:question(str qq, AId id, AType t): 
      collection += <q.src, id.name, qq, ATypeToType(t)>;   
    case q:assignQuestion(str qq, AId id, AType t, _):
      collection += <q.src, id.name, qq, ATypeToType(t)>;
  };
  return collection; 
}

Type ATypeToType(AType t) {
  switch(t) {
    case boolean(): return tbool();
    case integer(): return tint();
    case string(): return tstr();
  }
  return tunknown();
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  TEnv seen = {};
  visit(f) {
    case q:question(str label, AId id, AType t): {
      msgs += checkQuestion(q.src, id.name, label, ATypeToType(t), seen);
      seen += {<q.src, id.name, label, ATypeToType(t)>};
    }
    case q:assignQuestion(str label, AId id, AType t, AExpr expr): {
      msgs += checkQuestion(q.src, id.name, label, ATypeToType(t), seen);
      msgs += checkExpressionType(ATypeToType(t), expr, tenv, useDef);
      msgs += check(expr, tenv, useDef);
      seen += {<q.src, id.name, label, ATypeToType(t)>}; 
    }
    case q:ifstatement(AExpr guard, _): {
      msgs += checkExpressionType(tbool(), guard, tenv, useDef);
      msgs += check(guard, tenv, useDef);
    }
    case q:ifelse(AExpr guard, _, _): {
      msgs += checkExpressionType(tbool(), guard, tenv, useDef);
      msgs += check(guard, tenv, useDef);
    }
    
  };
  
  return msgs;  
}


set[Message] checkQuestion(loc src, str name, str label, Type t, TEnv seen) {
  set[Message] msgs = {};
  visit(seen) {
    case <loc def, str n, str l, Type \type>: {
      if (name == n && \type != t)
      	msgs += { error("Multiple declarations of the same question with different type", src) };
      if (name == n && label != l)
	  	msgs += { warning("Different label for multiple occurences of the same question", src) };
	  if (name != n && label == l)
	  	msgs += { warning("Duplicate labels", src) };
    }
  }
  
  return msgs;
}

set[Message] checkExpressionType(Type t, AExpr expr, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  if (t != typeOf(expr, tenv, useDef))
  	msgs += { error("Expression type does not match expected type <t> <typeOf(expr, tenv, useDef)>", expr.src) };
  return msgs;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(AId x):
      msgs += { error("Undeclared question", x.src) | useDef[x.src] == {} };
	case not(AExpr expr):
	  if (typeOf(expr, tenv, useDef) != tbool())
	    msgs += { error("Incorrect type", expr.src) };
	case times(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tint(), tenv, useDef);
	case div(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tint(), tenv, useDef);
	case plus(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tint(), tenv, useDef);
	case minus(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tint(), tenv, useDef);
	case l(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case leq(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case g(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case geq(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case eq(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case neq(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tenv, useDef);
	case and(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tbool(), tenv, useDef);
	case or(AExpr left, AExpr right):
	  msgs += checkBinaryOperator(left, right, tbool(), tenv, useDef);
  }
  
  return msgs; 
}

set[Message] checkBinaryOperator(AExpr left, AExpr right, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  if (typeOf(left, tenv, useDef) != typeOf(right, tenv, useDef)) {
    msgs += { error("Type does not match the other operand", left.src) };
    msgs += { error("Type does not match the other operand", right.src) };
  }
  return msgs;
}

set[Message] checkBinaryOperator(AExpr left, AExpr right, Type expected, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  if (typeOf(left, tenv, useDef) != expected)
    msgs += { error("Incorrect type <typeOf(left, tenv, useDef)>", left.src) };
  if (typeOf(right, tenv, useDef) != expected)
    msgs += { error("Incorrect type <typeOf(right, tenv, useDef)>", right.src) };
  return msgs;
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(name)): 
      if (<_, name, _, Type t> <- tenv)
        return t;
    case boolean(_): return tbool();
    case integer(_): return tint();
    case string(_): return tstr();
    case not(_): return tbool();
    case times(_, _): return tint();
    case div(_, _): return tint();
    case plus(_, _): return tint();
    case minus(_, _): return tint();
    case l(_, _): return tbool();
    case leq(_, _): return tbool();
    case g(_, _): return tbool();
    case geq(_, _): return tbool();
    case eq(_, _): return tbool();
    case neq(_, _): return tbool();
    case and(_, _): return tbool();
    case or(_, _): return tbool();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
