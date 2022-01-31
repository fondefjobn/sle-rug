module Syntax

extend lang::std::Layout;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = @Foldable "form" Ident Question; 

// question, computed question, block, if-then-else, if-then
syntax Question
  = @Foldable Str Ident ":" Type ("=" Expr)?
  | @Foldable "{" Question* "}"
  | @Foldable "if" "(" Expr ")" Question ("else" Question)?
  ; 

// +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Ident
  | Bool
  | Int
  | Str
  | bracket "(" Expr ")"
  > "!" Expr
  > left Expr "*" Expr
  > left Expr "/" Expr
  > left Expr "+" Expr
  > left Expr "-" Expr
  > left Expr "\<" Expr
  > left Expr "\<=" Expr
  > left Expr "\>" Expr
  > left Expr "\>=" Expr
  > left Expr "==" Expr
  > left Expr "!=" Expr
  > left Expr "&&" Expr
  > left Expr "||" Expr
  ;
  
syntax Type
  = "boolean"
  | "integer"
  | "string"
  ;  
 
lexical Str 
  = "\"" ![\n\"]* "\""; 

lexical Ident
  = ([a-zA-Z][a-zA-Z0-9_]* !>> [a-zA-Z0-9_]) \ Reserved ;
  
lexical Int 
  = "0" !>> [0-9]
  | [1-9][0-9]* !>> [0-9]
  ;

lexical Bool
  = "true"
  | "false"
  ;
  
keyword Reserved
  = "if" | "else" | "true" | "false" | "boolean" | "integer" | "string" ;
  
