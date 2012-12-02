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
    Character.isLetterOrDigit(char) || "*/+-?!".contains(char)
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

  def stateString(chars: List[Char], partials: List[Char]): List[Token] = {
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
      case '\'' :: rest => QuoteToken() :: stateInit(rest)
      case '.' :: rest => DotToken() :: stateInit(rest)
      case '"' :: rest => stateString(rest, List())
      case char :: rest if valid4symbol(char) => stateNumber(chars, List())
      case char :: rest => throw new Exception("Uknown char in stateInit [" + char + "], rest is " + rest)
    }
  }
}

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
      case QuoteToken() :: Lparen() :: rest =>
        extractExprsAndRest(rest) match {
          case (exprs, Rparen() :: rest2) => (convertToPairs(exprs), rest2)
          case (exprs, DotToken() :: rest2) => extractExprAndRest(rest2) match {
            case (expr, Rparen() :: rest3) => (convertToPairs(exprs, expr), rest3)
            case _ => throw new Exception("Can't find expr after a dot token  in " + tokens)
          }
          case _ => throw new Exception("Unclosed list after quote in " + tokens)
        }
      case QuoteToken() :: rest => extractExprAndRest(rest) match {
        case (VarExpr(x), rest2) => (SymbolExpr(x), rest2)
        case (expr: Expr, rest2) => (expr, rest2)
      }

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

      case Lparen() :: SymbolToken("define") :: rest =>
        extractExprsAndRest(rest) match {
          case (List(VarExpr(name), value), Rparen() :: rest2) => (DefExpr(name, value), rest2)
          case _ => throw new Exception("wrong syntax after define " + tokens)
        }

      case Lparen() :: SymbolToken("begin") :: rest =>
        extractExprsAndRest(rest) match {
          case (expr :: Nil, Rparen() :: rest2) => (expr, rest2)
          case (exprs, Rparen() :: rest2) => (SeqExpr(exprs), rest2)
          case _ => throw new Exception("missing rparen in begin statement " + tokens)
        }
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

object Interpreter {

  def is_pair(args: List[Result]) = {
    args match {
      case PairResult(a, d) :: Nil => BoolResult(value = true)
      case Nil => throw new IllegalArgumentException("Error: pair?: wrong number of arguments (expected: 1 got: 0)")
      case _ => BoolResult(value = false)
    }
  }

  def is_string(args: List[Result]) = {
    args match {
      case StringResult(x) :: Nil => BoolResult(value = true)
      case Nil => throw new IllegalArgumentException("Error: string?: wrong number of arguments (expected: 1 got: 0)")
      case _ => BoolResult(value = false)
    }
  }

  def list(args: List[Result]): Result = {
    args match {
      case Nil => NilResult()
      case head :: tail => PairResult(head, list(tail))
    }
  }

  def is_boolean(args: List[Result]) = {
    args match {
      case BoolResult(x) :: Nil => BoolResult(value = true)
      case Nil => throw new IllegalArgumentException("Error: boolean?: wrong number of arguments (expected: 1 got: 0)")
      case _ => BoolResult(value = false)
    }
  }

  def is_integer(args: List[Result]) = {
    args match {
      case IntResult(x) :: Nil => BoolResult(value = true)
      case Nil => throw new IllegalArgumentException("Error: integer?: wrong number of arguments (expected: 1 got: 0)")
      case _ => BoolResult(value = false)
    }
  }

  def string_length(args: List[Result]) = {
    args match {
      case StringResult(x) :: Nil => IntResult(x.length)
      case _ => throw new IllegalArgumentException("Error: string-length: string required, but got " + args)
    }
  }

  def min(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints.min
    IntResult(res)
  }

  def max(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints.max
    IntResult(res)
  }

  def plus(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ + _)
    IntResult(res)
  }

  def minus(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ - _)
    IntResult(res)
  }

  def div(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ / _)
    IntResult(res)
  }

  def mult(args: List[Result]) = {
    val ints = getIntsFrom(args)
    val res = ints reduce (_ * _)
    IntResult(res)
  }

  def is_zero(args: List[Result]) = {
    args match {
      case IntResult(0) :: Nil => BoolResult(value = true)
      case IntResult(x) :: Nil => BoolResult(value = false)
      case IntResult(x) :: tail => throw new IllegalArgumentException("Error: zero?: wrong number of arguments (expected: 1 got: " + args)
      case Nil => throw new IllegalArgumentException("Error: zero?: wrong number of arguments (expected: 1 got: 0)")
      case _ => throw new IllegalArgumentException("Error: zero?: wrong type of arguments (expected: integer got: " + args)
    }
  }

  def getIntsFrom(args: List[Result]): List[Int] = {
    args.map(res => res match {
      case IntResult(x) => x
      case _ => throw new IllegalArgumentException("Error : expected list of integers, got " + args)
    })
  }

  val defined_GE = new scala.collection.mutable.HashMap[String, Result]

  def eval(line: String): Result = {
    val tokens = Scanner.scan(line)
    val expr = Parser.parse(tokens)

    val GE = (x: String) => {
      debug("looking for a " + x + " in a GE")
      x match {
        case "+" => NativeClosure(this.plus)
        case "-" => NativeClosure(this.minus)
        case "/" => NativeClosure(this.div)
        case "*" => NativeClosure(this.mult)
        case "min" => NativeClosure(this.min)
        case "max" => NativeClosure(this.max)
        case "string-length" => NativeClosure(this.string_length)
        case "string?" => NativeClosure(this.is_string)
        case "integer?" => NativeClosure(this.is_integer)
        case "boolean?" => NativeClosure(this.is_boolean)
        case "pair?" => NativeClosure(this.is_pair)
        case "list" => NativeClosure(this.list)
        case "zero?" => NativeClosure(this.is_zero)
        case _ => defined_GE(x) match {
          case result: Result => result
          case _ => throw new Exception("Error: unbound symbol: " + x)
        }
      }
    }
    eval(expr, GE)
  }

  def extend(env: (String) => Result, names: List[String], values: List[Result]): (String) => Result = {
    val map = names.zip(values)
    (name: String) => {
      debug("lookin for a " + name + " in " + map)
      map.find(x => x._1.equals(name)) match {
        case Some(x) => x._2
        case None => env(name)
      }
    }
  }

  def updateGE(name: String, value: Result): Unit = {
    this.defined_GE += name -> value
  }

  def debug(s: String) = if (REPL.debug) println(s)

  def eval(expr: Expr, env: String => Result): Result = {
    expr match {
      case NumExpr(x) => IntResult(x)
      case TrueExpr() => BoolResult(value = true)
      case FalseExpr() => BoolResult(value = false)
      case VoidExpr() => VoidResult()
      case NilExpr() => NilResult()
      case SymbolExpr(x) => SymbolResult(x)
      case StringExpr(x) => StringResult(x)
      case DefExpr(name, value) => {
        updateGE(name, eval(value, env))
        VoidResult()
      }
      case VarExpr(name) => env(name)
      case PairExpr(a, d) => PairResult(eval(a, env), eval(d, env))
      case Lambda(params, body) => Closure(params, body, env)
      case AppExpr(oper, args) => {
        eval(oper, env) match {
          case Closure(names, body, env2) => {
            debug("applying (lambda(" + names + ") (" + body + ")")
            debug("evaluating args " + args)
            val values = args map (arg => eval(arg, env))
            debug("the values are " + values)
            val newEnv = extend(env2, names, values)
            eval(body, newEnv)
          }
          case NativeClosure(body) => {
            debug("applying native closure " + oper)
            debug("evaluating args " + args)
            val values = args map (arg => eval(arg, env))
            debug("the values are " + values)
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
  var debug = false

  def makeList(list: Result, items: List[Result]): String = {
    list match {
      case PairResult(a, NilResult()) => "(" + ((a :: items) map format).reverse.mkString(" ") + ")"
      case PairResult(a, d) => makeList(d, (a :: items))
      case NilResult() => "(" + (items map format).mkString(" ") + ")"
      case _: Result => "(" + (items map format).reverse.mkString(" ") + " . " + (format _) + ")"
    }
  }

  def format(result: Result): String = {
    result match {
      case IntResult(x) => x.toString
      case BoolResult(b) => (if (b) "#t" else "#f")
      case SymbolResult(x) => x
      case Closure(params, body, env) => "<closure>"
      case NativeClosure(_) => "<native closure>"
      case VoidResult() => ""
      case PairResult(a, d) => makeList(d, List(a))
      case NilResult() => "()"
      case StringResult(x) => "\"" + x + "\""
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
        case "debug-on" => debug = true; print("> ")
        case "debug-off" => debug = false; print("> ")
        case "" => print("> ")
        case _ => {
          try {
            val res = Interpreter.eval(line)
            res match {
              case VoidResult() => print("> ")
              case _ => println(" " + format(res)); print("> ")
            }

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

  def main(args: Array[String]): Unit = REPL.repl()
}

