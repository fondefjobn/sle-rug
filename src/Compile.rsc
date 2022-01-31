module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library
import Transform;
import String;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  f_flat = flatten(f);        //transform into flat structure, so there are less cases to consider
  str source = "<f.src[extension="js"].top>";
  int begin = findLast(source, "/") + 1;
  int end = findLast(source, "|");
  source = substring(source, begin, end);
  writeFile(f.src[extension="js"].top, form2js(f_flat));
  writeFile(f.src[extension="html"].top, toString(form2html(f_flat, source)));
}

HTML5Node form2html(AForm f, str source) {
  HTML5Node body_part = body();
  if (questions(list[AQuestion] qs) := f.q)
    body_part = body(onload("update();"),
                     form([onsubmit("return false")] + [form2html(q) | q <- qs] +
                     	  [input(\type("submit"), \value("Submit"), onclick("submitForm();"))]),
                          p(id("output")));                 
  return html(head(title(f.name), script(src(source))),
              body_part);
}

HTML5Node form2html(AQuestion question) {
  switch (question) {
    case question(str label, AId ident, AType t): {
      switch (t) {
        case boolean():
          return div(p(label), input(\type("checkbox"), id(ident.name),
                            onchange("update();")));
        case integer():
          return div(p(label), input(\type("number"), id(ident.name), \value(0),
                            onchange("update();")));
        case string():
          return div(p(label), input(\type("text"), id(ident.name), \value(""),
                            onchange("update();")));
      }
    }
    case assignQuestion(str label, AId ident, AType t, AExpr expr): {
      switch (t) {
        case boolean():
          return div(p(label), span(id(ident.name), html5attr("assign", Expr2str(expr)),
                      html5attr("data-value", "false"), "0"));
        case integer():
          return div(p(label), span(id(ident.name), html5attr("assign", Expr2str(expr)),
                      html5attr("data-value", 0), "0"));
        case string():
          return div(p(label), span(id(ident.name), html5attr("assign", Expr2str(expr)),
                      html5attr("data-value", ""), "0"));
      }
    }
    case ifstatement(_, AQuestion ifPart):
      return form2html(ifPart);
  }
}

str Expr2str(AExpr expr) {
  switch(expr){
    case ref(AId id): 
      return "getValue(&quot;" + id.name + "&quot;)"; 
    case boolean(bool b): 
      return "<b>";
    case integer(int n):
      return "<n>";
    case string(str s):
      return s;
    case not(AExpr expr): 
      return "!" + Expr2str(expr);
    case times(AExpr left, AExpr right):
      return Expr2str(left) + " * " + Expr2str(right);
    case div(AExpr left, AExpr right):
      return Expr2str(left) + " / " + Expr2str(right);
    case plus(AExpr left, AExpr right):
      return Expr2str(left) + " + " + Expr2str(right);
    case minus(AExpr left, AExpr right):
      return Expr2str(left) + " - " + Expr2str(right);
    case l(AExpr left, AExpr right):
      return Expr2str(left) + " \< " + Expr2str(right);
    case leq(AExpr left, AExpr right):
      return Expr2str(left) + " \<= " + Expr2str(right);
    case g(AExpr left, AExpr right):
      return Expr2str(left) + " \> " + Expr2str(right);
    case geq(AExpr left, AExpr right):
      return Expr2str(left) + " \>= " + Expr2str(right);
    case eq(AExpr left, AExpr right):
      return Expr2str(left) + " == " + Expr2str(right);
    case neq(AExpr left, AExpr right):
      return Expr2str(left) + " != " + Expr2str(right);
    case and(AExpr left, AExpr right):
      return Expr2str(left) + " && " + Expr2str(right);
    case or(AExpr left, AExpr right):
      return Expr2str(left) + " || " + Expr2str(right);
  }
}

str form2js(AForm f) {	
  return "<global_variables(f)>
         '<submit(f)>
         '<getValue()>
         '<update(f)>";
}

str global_variables(AForm f) {
  set[str] idents = {};
  
  visit(f) {
    case question(_, AId i, _):
      idents += i.name;
    case assignQuestion(_, AId i, _, _):
      idents += i.name;
  }
  
  str toReturn = "var idents = [";
  for (id <- idents)
    toReturn += "\"" + id + "\",";
    
  toReturn = replaceLast(toReturn, ",", "");
  toReturn += "];\n";
  return toReturn;
}

str submit(AForm f) {
  return "function submitForm() {
         '  var thank_you = \"Thank you for submitting the form. Your data will be processed.\";
         '  document.getElementById(\"output\").innerHTML = thank_you;
         '
         '  //if the data were to be processed (e.g. to put into a DB) it would be done over here 
         '}\n";
	return result;
}

str getValue() {
  return "function getValue(id) {
         '  elem = document.getElementById(id);
         '
         '  if (elem.hasAttribute(\"data-value\"))
         '    return elem.getAttribute(\"data-value\");
         '  
         '  if (elem.hasAttribute(\"type\") && elem.type == \"checkbox\")
         '	  return elem.checked;
         '	
         '  return elem.value;
         '}\n";
}

str update(AForm f) {
  return "function update() {
         '  for (var i in idents) {
         '    var elem = document.getElementById(idents[i]);
         '    //update assigned forms
         '    if (elem.hasAttribute(\"assign\")) {
         '      var value = eval(elem.getAttribute(\"assign\"));
         '      elem.setAttribute(\"data-value\", value);
         '      elem.innerHTML = value;
         '    }
         '  }
         '
         '  <update(f.q, true)>  for (i in idents) {
         '    var elem = document.getElementById(idents[i]);
         '    if (elem.visible) {
         '      if (elem.parentElement.hasAttribute(\"hidden\"))
         '        elem.parentElement.removeAttribute(\"hidden\");
         '    }
         '    else {
         '      elem.parentElement.setAttribute(\"hidden\", \"true\");
         '	  }
         '  }
         '}\n";
}

str update(AQuestion question, bool b) {
  switch(question) {
    case question(_, AId ident, _):
      return "document.getElementById(\"<ident.name>\").visible = <b>;";
    case assignQuestion(_, AId ident, _, _):
      return "document.getElementById(\"<ident.name>\").visible = <b>;";
    case questions(list[AQuestion] qs): {
      str toReturn = "";
      for (q <- qs)
        toReturn += update(q, b);
      return toReturn;
    }
    case ifstatement(AExpr guard, AQuestion ifPart): {
      return 
        "if (eval(<replaceAll(Expr2str(guard), "&quot;", "\"")>))
        '  <update(ifPart, b)>
        'else
        '  <update(ifPart, !b)>
        '\n";
    }
  }
}