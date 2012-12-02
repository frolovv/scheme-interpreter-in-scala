package com.vf.scheme

object Parser {

  def extractExprsAndRest(tokens: List[Token]): (List[Expr], List[Token]) = {
    tokens match {
      case Nil => (Nil, Nil)
      case _ => extractExprAndRest(tokens) match {
        case (value: Expr, Rparen() :: rest) => (List(value), Rparen() :: rest)
        case (value: Expr, DotToken() :: rest) => (List(value), DotToken() :: rest)
        case (value: Expr, rest) => extractExprsAndRest(rest) match {
          case (values: List[Expr], rest2) => (value :: values, rest2)
          case (_, rest3) => throw new Exception("Could not extract exprs and rest from " + tokens)
        }
      }
    }
  }

  def extract(exprs: List[Expr]): List[String] = {
    exprs.map(e => e.asInstanceOf[VarExpr].name)
  }

  def convertToPairs(exprs: List[Expr]): Expr = {
    convertToPairs(exprs, NilExpr())
  }

  def convertToPairs(exprs: List[Expr], expr: Expr): Expr = {
    exprs match {
      case Nil => expr
      case head :: tail => PairExpr(head, convertToPairs(tail, expr))
    }
  }

  def extractExprAndRest(tokens: List[Token]): (Expr, List[Token]) = {
    tokens match {
      case NumToken(x) :: rest => (NumExpr(x), rest)
      case BoolToken('t') :: rest => (TrueExpr(), rest)
      case BoolToken('f') :: rest => (FalseExpr(), rest)
      case SymbolToken(x) :: rest => (VarExpr(x), rest)
      case StringToken(x) :: rest => (StringExpr(x), rest)

      // single-quote and list
      case QuoteToken() :: Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (exprs, Rparen() :: rest2) => (convertToPairs(exprs), rest2)
          case (exprs, DotToken() :: rest2) => extractExprAndRest(rest2) match {
            case (expr, Rparen() :: rest3) => (convertToPairs(exprs, expr), rest3)
            case _ => throw new Exception("Can't find expr after a dot token  in " + tokens)
          }
          case _ => throw new Exception("Unclosed list after quote in " + tokens)
        }
      // (let ((x 1) (y 2)) body)
      case Lparen() :: SymbolToken("let") :: rest =>
        extractExprAndRest(rest) match {
          case (AppExpr(head, tail), rest2) => extractExprAndRest(rest2) match {
            case (body, Rparen() :: rest3) => {
              val bindings = head :: tail
              val names = bindings map {
                case AppExpr(VarExpr(name), _) => name
              }
              val values = bindings map {
                case AppExpr(_, expr :: Nil) => expr
              }
              (AppExpr(Lambda(names, body), values), rest3)
            }
          }
        }
      // single-quote and symbols and other constants
      case QuoteToken() :: rest => extractExprAndRest(rest) match {
        case (VarExpr(x), rest2) => (SymbolExpr(x), rest2)
        case (expr: Expr, rest2) => (expr, rest2)
      }
      // if statement
      case Lparen() :: SymbolToken("if") :: rest =>
        extractExprsAndRest(rest) match {
          case (List(condition, ift, iff), Rparen() :: rest2) => (IfExpr(condition, ift, iff), rest2)
          case (List(condition, ift), Rparen() :: rest2) => (IfExpr(condition, ift, VoidExpr()), rest2)
          case _ => throw new Exception("invalid if statement " + tokens)
        }
      // lambda statement
      case Lparen() :: SymbolToken("lambda") :: Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (names, Rparen() :: rest2) => extractExprAndRest(rest2) match {
            case (body, Rparen() :: rest3) => (Lambda(extract(names), body), rest3)
            case _ => throw new Exception("Lambda's body is missing right paren " + tokens)
          }
          case _ => throw new Exception("Lambda's parameter list is malformed " + tokens)
        }
      // define statement
      case Lparen() :: SymbolToken("define") :: rest =>
        extractExprsAndRest(rest) match {
          case (List(VarExpr(name), value), Rparen() :: rest2) => (DefExpr(name, value), rest2)
          case _ => throw new Exception("wrong syntax after define " + tokens)
        }
      // begin statement
      case Lparen() :: SymbolToken("begin") :: rest =>
        extractExprsAndRest(rest) match {
          case (expr :: Nil, Rparen() :: rest2) => (expr, rest2)
          case (exprs, Rparen() :: rest2) => (SeqExpr(exprs), rest2)
          case _ => throw new Exception("missing rparen in begin statement " + tokens)
        }
      // function application
      case Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (operator :: operands, Rparen() :: rest2) => (AppExpr(operator, operands), rest2)
          case _ => throw new Exception("missing right paren " + tokens)
        }
      case _ => throw new Exception("Could not extract expr and rest from " + tokens + ", probably due to missing right paren")
    }
  }

  def parse(tokens: List[Token]): Expr = {
    extractExprsAndRest(tokens) match {
      case (expr :: Nil, Nil) => expr
      case (exprs, Nil) => SeqExpr(exprs)
      case (_, rest) => throw new Exception("Leftover tokens [" + rest + "] after parsing " + tokens)
    }
  }

  def parse(line: String): Expr = {
    val tokens = Scanner.scan(line)
    parse(tokens)
  }
}
