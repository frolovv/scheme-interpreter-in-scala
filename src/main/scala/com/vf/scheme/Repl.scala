package com.vf.scheme

/*
* Parses and evaluates simple scheme expressions like (+ 1 2)
*
* */
object Repl {
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
      case ClosureSimple(params, body, env) => "<closure>"
      case ClosureVar(param, body, env) => "<closure>"
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

  def main(args: Array[String]): Unit = Repl.repl()
}

