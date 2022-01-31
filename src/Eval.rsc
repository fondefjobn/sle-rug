module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.

// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv venv = ();
  visit(f) {
    case question(_, AId id, AType t): 
      venv(id.name) = defaultValue(t);
    case assignQuestion(_, AId id, AType t, _):
      venv(id.name) = defaultValue(t);
  };
  return venv;
}

Value defaultValue(AType t) {
  switch(t) {
    case boolean():
      return vbool(false);
    case integer():
      return vint(0);
    case string():
      return vstr("");
  }
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  venv(inp.question) = inp.\value;
  return eval(f.q, inp, venv); 
}

// evaluate conditions for branching,
// evaluate inp and computed questions to return updated VEnv
VEnv eval(AQuestion q, Input inp, VEnv venv) {
  switch (q) {
    case assignQuestion(_, AId id, _, AExpr expr): 
      venv(id.name) = eval(expr);
    case questions(list[AQuestion] qs):
      for (qq <- qs)
        venv = eval(qq, inp, venv);
    case ifstatement(AExpr guard, AQuestion ifPart):
      if (vbool(true) := eval(guard))
        venv = eval(ifPart, inp, venv);
    case ifelse(AExpr guard, AQuestion ifPart, AQuestion elsePart):
      if (vbool(true) := eval(guard))
        venv = eval(ifPart, inp, venv);
      else
        venv = eval(elsePart, inp, venv);
  }
  return venv; 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case boolean(bool b): return vbool(b);
    case integer(int n): return vint(n);
    case string(str s): return vstr(s);
    case not(AExpr expr):
      if (vbool(b) := eval(expr, venv)) 
        return vbool(!b);
    case times(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vint(x1 * x2);
    case div(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vint(x1 / x2);
    case plus(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vint(x1 + x2);
    case minus(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vint(x1 - x2);
    case l(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 < 2);
    case leq(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 <= 2);
    case g(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 > 2);
    case geq(AExpr left, AExpr right):
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 >= 2);
    case eq(AExpr left, AExpr right): {
      if (vbool(x1) := eval(left, venv) && vbool(x2) := eval(right, venv))
        return vbool(x1 == x2);
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 == x2);
    }
    case neq(AExpr left, AExpr right): {
      if (vbool(x1) := eval(left, venv) && vbool(x2) := eval(right, venv))
        return vbool(x1 != x2);
      if (vint(x1) := eval(left, venv) && vint(x2) := eval(right, venv))
        return vbool(x1 != x2);
    }
    case and(AExpr left, AExpr right):
      if (vbool(x1) := eval(left, venv) && vbool(x2) := eval(right, venv))
        return vbool(x1 && x2);
    case or(AExpr left, AExpr right):
      if (vbool(x1) := eval(left, venv) && vbool(x2) := eval(right, venv))
        return vbool(x1 || x2);
    default: throw "Unsupported expression <e>";
  }
}