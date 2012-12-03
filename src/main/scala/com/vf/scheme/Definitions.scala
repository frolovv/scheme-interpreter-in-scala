package com.vf.scheme

sealed abstract class Result

case class IntResult(value: Int) extends Result

case class BoolResult(value: Boolean) extends Result

case class StringResult(value : String) extends Result

case class SymbolResult(value : String) extends Result

case class VoidResult() extends Result

case class PairResult(car : Result, cdr : Result) extends Result

case class NilResult() extends Result

case class NativeClosure(body: (List[Result]) => Result) extends Result

case class Closure(params: List[String], body: Expr, env: String => Result) extends Result

sealed abstract class Expr

case class NumExpr(value: Int) extends Expr
case class StringExpr(value : String) extends Expr

case class VoidExpr() extends Expr

case class TrueExpr() extends Expr

case class FalseExpr() extends Expr

case class IfExpr(condition: Expr, ift: Expr, iff: Expr) extends Expr

case class DefExpr(name : String, body : Expr) extends Expr

case class VarExpr(name: String) extends Expr

case class PairExpr(a : Expr, d : Expr) extends Expr

case class NilExpr() extends Expr

case class SymbolExpr(name : String) extends Expr

case class LambdaSimple(params: List[String], body: Expr) extends Expr

case class SeqExpr(exprs : List[Expr]) extends Expr

case class AppExpr(operator: Expr, args: List[Expr]) extends Expr

sealed abstract class Token

case class NumToken(value: Int) extends Token

case class BoolToken(value: Char) extends Token

case class SymbolToken(value: String) extends Token

case class StringToken(value : String) extends Token

case class Rparen() extends Token

case class Lparen() extends Token

case class DotToken() extends Token

case class QuoteToken() extends Token
