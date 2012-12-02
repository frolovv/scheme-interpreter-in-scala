package com.vf.scheme

object Interpreter {
  val defined_GE = new scala.collection.mutable.HashMap[String, Result]

  def eval(line: String): Result = {
    val tokens = Scanner.scan(line)
    val expr = Parser.parse(tokens)

    val GE = (x: String) => {
      debug("looking for a " + x + " in a GE")
      x match {
        case "+" => NativeClosure(Builtins.plus)
        case "-" => NativeClosure(Builtins.minus)
        case "/" => NativeClosure(Builtins.div)
        case "*" => NativeClosure(Builtins.mult)
        case "min" => NativeClosure(Builtins.min)
        case "max" => NativeClosure(Builtins.max)
        case "string-length" => NativeClosure(Builtins.string_length)
        case "string?" => NativeClosure(Builtins.is_string)
        case "integer?" => NativeClosure(Builtins.is_integer)
        case "boolean?" => NativeClosure(Builtins.is_boolean)
        case "pair?" => NativeClosure(Builtins.is_pair)
        case "list" => NativeClosure(Builtins.list)
        case "zero?" => NativeClosure(Builtins.is_zero)
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

  def debug(s: String) = if (Repl.debug) println(s)

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

      case SeqExpr(exprs) => {
        val rev = exprs.reverse
        val last = rev.head
        val rest = rev.tail

        (rest reverse) foreach (expr => eval(expr, env))
        eval(last, env)
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
