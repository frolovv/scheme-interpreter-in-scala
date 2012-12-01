package com.vf.scheme

/*
* Parses and evaluates simple scheme expressions like (+ 1 2)
*
* */


object Scanner {
  def scan(line: String): List[Token] = {
    val chars = line.toCharArray

    stateInit(chars.toList)
  }

  def implode(chars: List[Char]): NumToken = {
    val num = chars.foldRight(0)((char, res) => res * 10 + char.getNumericValue)
    NumToken(num)
  }

  def implode2string(chars: List[Char]): String = {
    chars.mkString
  }

  def implode2int(chars: List[Char]): Int = {
    implode2string(chars).toInt
  }

  def toNumOrSymbolToken(chars: List[Char]): Token = {
    try {
      NumToken(implode2int(chars reverse))
    }
    catch {
      case _: Throwable => SymbolToken(implode2string(chars reverse))
    }
  }

  def valid4symbol(char: Char): Boolean = {
    Character.isLetterOrDigit(char) || "*/+-".contains(char)
  }

  def stateNumber(chars: List[Char], partials: List[Char]): List[Token] = {
    chars match {
      case char :: Nil if valid4symbol(char) => List(toNumOrSymbolToken(char :: partials))
      case char :: rest if valid4symbol(char) => stateNumber(rest, char :: partials)
      case char :: rest if List(' ', ')', '(').contains(char) => toNumOrSymbolToken(partials) :: stateInit(chars)
      case _ => throw new Exception("invalid character in stateNumber [" + chars + "], partial number is " + partials)
    }
  }

  def stateHash(chars: List[Char]): List[Token] = {
    chars match {
      case 't' :: rest => BoolToken('t') :: stateInit(rest)
      case 'f' :: rest => BoolToken('f') :: stateInit(rest)
      case _ => throw new Exception("Uknown char in stateHash " + chars)
    }
  }

  def stateString(chars: List[Char], partials : List[Char]): List[Token] = {
    chars match {
      case '"' :: rest => StringToken(partials.reverse.mkString("")) :: stateInit(rest)
      case char :: rest => stateString(rest, char :: partials)
      case _ => throw new Exception("Uknown char in stateString " + chars)
    }
  }

  def stateInit(chars: List[Char]): List[Token] = {

    chars match {
      case Nil => Nil
      case '(' :: rest => Lparen() :: stateInit(rest)
      case ')' :: rest => Rparen() :: stateInit(rest)
      case ' ' :: rest => stateInit(rest)
      case '#' :: rest => stateHash(rest)
      case '"' :: rest => stateString(rest, List())
      case char :: rest if valid4symbol(char) => stateNumber(chars, List())
      case char :: rest => throw new Exception("Uknown char in stateInit [" + char + "], rest is " + rest)
    }
  }
}

object Parser {

  def extractExprsAndRest(tokens: List[Token]): (List[Expr], List[Token]) = {
    extractExprAndRest(tokens) match {
      case (value: Expr, Rparen() :: rest) => (List(value), Rparen() :: rest)
      case (value: Expr, rest) => extractExprsAndRest(rest) match {
        case (values: List[Expr], rest2) => (value :: values, rest2)
        case (_, rest3) => throw new Exception("Could not extract exprs and rest from " + tokens)
      }
    }
  }

  def extract(exprs: List[Expr]): List[String] = {
    exprs.map(e => e.asInstanceOf[VarExpr].name)
  }

  def extractExprAndRest(tokens: List[Token]): (Expr, List[Token]) = {
    tokens match {
      case NumToken(x) :: rest => (NumExpr(x), rest)
      case BoolToken('t') :: rest => (TrueExpr(), rest)
      case BoolToken('f') :: rest => (FalseExpr(), rest)
      case SymbolToken(x) :: rest => (VarExpr(x), rest)
      case StringToken(x) :: rest => (StringExpr(x), rest)

      case Lparen() :: SymbolToken("if") :: rest =>
        extractExprsAndRest(rest) match {
          case (List(condition, ift, iff), Rparen() :: rest2) => (IfExpr(condition, ift, iff), rest2)
          case (List(condition, ift), Rparen() :: rest2) => (IfExpr(condition, ift, VoidExpr()), rest2)
          case _ => throw new Exception("invalid if statement " + tokens)
        }

      case Lparen() :: SymbolToken("lambda") :: Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (names, Rparen() :: rest2) => extractExprAndRest(rest2) match {
            case (body, Rparen() :: rest3) => (Lambda(extract(names), body), rest3)
            case _ => throw new Exception("Lambda's body is missing right paren " + tokens)
          }
          case _ => throw new Exception("Lambda's parameter list is malformed " + tokens)
        }
      case Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (operator :: operands, Rparen() :: rest2) => (AppExpr(operator, operands), rest2)
          case _=> throw new Exception("missing right paren " + tokens)
        }
      case _ => throw new Exception("Could not extract expr and rest from " + tokens)
    }
  }

  def parse(tokens: List[Token]): Expr = {
    extractExprAndRest(tokens) match {
      case (x: Expr, Nil) => x
      case (_, rest) => throw new Exception("Leftover tokens " + rest)
    }
  }

  def parse(line: String): Expr = {
    val tokens = Scanner.scan(line)
    parse(tokens)
  }
}

object Interpreter {

  def plus(args : List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ + _)
    IntResult(res)
  }

  def minus(args : List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ - _)
    IntResult(res)
  }

  def div(args : List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ / _)
    IntResult(res)
  }

  def mult(args : List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ * _)
    IntResult(res)
  }

  def getIntsFrom(args : List[Result]) : List[Int] = {
    args.map(res => res match {case IntResult(x) => x})
  }

  def eval(line: String): Result = {
    val tokens = Scanner.scan(line)
    val expr = Parser.parse(tokens)

    val GE = (x: String) => {
      x match {
        case "+" => NativeClosure(plus)
        case "-" => NativeClosure(minus)
        case "/" => NativeClosure(div)
        case "*" => NativeClosure(mult)
        case _ => throw new Exception("Param not found")
      }
    }
    eval(expr, GE)
  }

  def extend(env: (String) => Result, names: List[String], values: List[Result]): (String) => Result = {
    val map = names.zip(values)
    (name: String) => {
      map.find(x => x._1.equals(name)) match {
        case Some(x) => x._2
        case None => env(name)
      }
    }
  }

  def eval(expr: Expr, env: String => Result): Result = {
    expr match {
      case NumExpr(x) => IntResult(x)
      case TrueExpr() => BoolResult(value = true)
      case FalseExpr() => BoolResult(value = false)
      case VoidExpr() => VoidResult()
      case StringExpr(x) => StringResult(x)
      case VarExpr(name) => env(name)
      case Lambda(params, body) => Closure(params, body, env)
      case AppExpr(oper, args) => {
        eval(oper, env) match {
          case Closure(names, body, env2) => {
            val values = args map (arg => eval(arg, env2))
            val newEnv = extend(env2, names, values)
            eval(body, newEnv)
          }
          case NativeClosure(body) => {
            val values = args map (arg => eval(arg, env))
            body(values)
          }
          case x => throw new Exception("Trying to apply non-lambda " + x)
        }
      }

      case IfExpr(cond, ift, iff) => eval(cond, env) match {
        case BoolResult(false) => eval(iff, env)
        case _ => eval(ift, env)
      }
      case _ => throw new Exception("Uknown expression " + expr)
    }
  }
}

object REPL {
  def format(result : Result) : String = {
    result match{
      case IntResult(x) => x + " : int"
      case BoolResult(b) => (if (b) "#t" else "#f") + " : boolean"
      case Closure(params, body, env) => "<closure>"
      case NativeClosure(_) => "<native closure>"
      case VoidResult() => ""
      case _ => ""
    }
  }

  def repl() {
    print("> ")
    for (line <- io.Source.stdin.getLines()) {
      line match {
        case _ if List("exit", "quit", "bye").contains(line) => {
          println("bye bye")
          sys.exit()
        }
        case "" => print("> ")
        case _ => {
          try {
            val res = Interpreter.eval(line)
            println("> " + format(res))
            print("> ")
          }
          catch {
            case e: Exception => {
              println(e.getMessage)
              print("> ")
            }
          }
        }
      }
    }
  }

  def main(args: Array[String]) = {REPL.repl()}
}

