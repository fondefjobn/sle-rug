module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) 
  = cst2ast(sf.top);

AForm cst2ast(frm:(Form)`form <Ident f> <Question q>` )
  = form("<f>", cst2ast(q), src=frm@\loc);

AQuestion cst2ast(Question qst) {
  switch (qst) {
    case (Question)`<Str q> <Ident var> : <Type t>`:
    	return question("<q>", id("<var>", src=var@\loc), cst2ast(t), src=qst@\loc);
    case (Question)`<Str q> <Ident var> : <Type t> = <Expr e>`:
    	return assignQuestion("<q>", id("<var>", src=var@\loc), cst2ast(t), cst2ast(e), src=qst@\loc);
    case (Question)`{ <Question* qs> }`:
    	return questions([cst2ast(q) | Question q <- qs], src=qst@\loc);
	case (Question)`if ( <Expr guard> ) <Question q>`:
		return ifstatement(cst2ast(guard), cst2ast(q), src=qst@\loc);
    case (Question)`if ( <Expr guard> ) <Question qIf>  else <Question qElse>`:
    	return ifelse(cst2ast(guard), cst2ast(qIf), cst2ast(qElse), src=qst@\loc);
    default: throw "Unhandled question: <qst>";
  }
}

AExpr cst2ast(Expr ex) {
  switch (ex) {
    case (Expr)`<Ident x>`: 
    	return ref(id("<x>", src=x@\loc), src=x@\loc);
    case (Expr)`<Bool x>`:
    	return boolean("<x>" == "true", src=ex@\loc);
    case (Expr)`<Int x>`:
    	return integer(toInt("<x>"), src=ex@\loc);
    case (Expr)`<Str x>`:
    	return string("<x>", src=ex@\loc);
	case (Expr)`( <Expr ex> )`:
		return cst2ast(ex);
	case (Expr)`! <Expr ex>`:
		return not(cst2ast(ex), src=ex@\loc);
	case (Expr)`<Expr ex1> * <Expr ex2>`:
		return times(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> / <Expr ex2>`:
		return div(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> + <Expr ex2>`:
		return plus(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> - <Expr ex2>`:
		return minus(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> \< <Expr ex2>`:
		return l(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> \<= <Expr ex2>`:
		return leq(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> \> <Expr ex2>`:
		return g(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> \>= <Expr ex2>`:
		return geq(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> == <Expr ex2>`:
		return eq(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> != <Expr ex2>`:
		return neq(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> && <Expr ex2>`:
		return and(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
	case (Expr)`<Expr ex1> || <Expr ex2>`:
		return or(cst2ast(ex1), cst2ast(ex2), src=ex@\loc);
    default: throw "Unhandled expression: <ex>";
  }
}

AType cst2ast(t:(Type)`<Type x>`) {
  switch ("<x>") {
  	case "boolean": return boolean(src=t@\loc);
  	case "integer": return integer(src=t@\loc);
  	case "string": return string(src=t@\loc);
  	default: throw "Unhandled type: <x>";
  }
}
