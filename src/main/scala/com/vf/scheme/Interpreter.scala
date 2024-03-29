package com.vf.scheme

import java.io.{BufferedReader, FileReader, File}

object Interpreter {
  val defined_GE = new scala.collection.mutable.HashMap[String, Result]

  def eval(file: File): Result = {
    val reader = new BufferedReader(new FileReader(file))

    var line = reader.readLine
    var result: Result = null

    while (line != null) {
      result = eval(line)
      line = reader.readLine
    }
    result
  }

  def eval(line: String): Result = {
    val tokens = Scanner.scan(line)
    val expr = Parser.parse(tokens)

    val GE = (x: String) => {
      debug("looking for a " + x + " in a GE")

      if (Builtins.GE contains x) Builtins.GE(x)
      else if (defined_GE contains x) defined_GE(x)
      else throw new Exception("Error: unbound symbol: " + x)
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

  def updateGE(name: String, value: Result) {
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
      case LambdaSimple(params, body) => ClosureSimple(params, body, env)
      case LambdaVar(param, body) => ClosureVar(param, body, env)
      case AppExpr(oper, args) => {
        eval(oper, env) match {
          case ClosureSimple(names, body, env2) => {
            debug("applying (lambda(" + names + ") (" + body + ")")
            debug("evaluating args " + args)
            val values = args map (arg => eval(arg, env))
            debug("the values are " + values)
            val newEnv = extend(env2, names, values)
            eval(body, newEnv)
          }

          case ClosureVar(name, body, env2) => {
            debug("applying (lambda" + name + " (" + body + ")")
            debug("evaluating args " + args)
            val values = args map (arg => eval(arg, env))
            val value = Builtins.scalaListToScheme(values)
            debug("the values are " + value)
            val newEnv = extend(env2, List(name), List(value))
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
