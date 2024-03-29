package com.vf.scheme

import collection.immutable.HashMap

object Builtins {
  var GE = new HashMap[String, Result]

  def register(name: String)(builtin: (List[Result]) => Result) {
    GE += name -> NativeClosure(builtin)
  }

  def register(name: String, function: String) {
    val closure = Interpreter.eval(function)
    GE += name -> closure
  }

  register("string?") {
    case StringResult(x) :: Nil => BoolResult(value = true)
    case Nil => throw new IllegalArgumentException("Error: string?: wrong number of arguments (expected: 1 got: 0)")
    case _ => BoolResult(value = false)
  }

  register("pair?") {
    case PairResult(a, d) :: Nil => BoolResult(value = true)
    case Nil => throw new IllegalArgumentException("Error: pair?: wrong number of arguments (expected: 1 got: 0)")
    case _ => BoolResult(value = false)
  }

  register("list", "(lambda x x)")

  register("car") {
    case PairResult(a, d) :: Nil => a
    case x => throw new IllegalArgumentException("Error: car: wrong type of arguments (expected: pair got: " + x + ")")
  }

  register("cdr") {
    case PairResult(a, d) :: Nil => d
    case x => throw new IllegalArgumentException("Error: cdr: wrong type of arguments (expected: pair got: " + x + ")")
  }

  register("cons") {
    case a :: d :: Nil => PairResult(a, d)
    case x => throw new IllegalArgumentException("Error: cons: wrong number of arguments (expected: 2, got: " + x + ")")
  }

  register("null?") {
    case NilResult() :: Nil => BoolResult(value = true)
    case _ => BoolResult(value = false)
  }

  register("boolean?") {
    case BoolResult(x) :: Nil => BoolResult(value = true)
    case Nil => throw new IllegalArgumentException("Error: boolean?: wrong number of arguments (expected: 1 got: 0)")
    case _ => BoolResult(value = false)
  }

  register("integer?") {
    case IntResult(x) :: Nil => BoolResult(value = true)
    case Nil => throw new IllegalArgumentException("Error: integer?: wrong number of arguments (expected: 1 got: 0)")
    case _ => BoolResult(value = false)
  }

  register("string-length") {
    case StringResult(x) :: Nil => IntResult(x.length)
    case x => throw new IllegalArgumentException("Error: string-length: string required, but got " + x)
  }

  register("min") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints.min
      IntResult(res)
  }

  register("max") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints.max
      IntResult(res)
  }

  register("+") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints reduce (_ + _)
      IntResult(res)
  }

  register("-") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints reduce (_ - _)
      IntResult(res)
  }


  register("/") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints reduce (_ / _)
      IntResult(res)
  }


  register("*") {
    args: List[Result] =>
      val ints = getIntsFrom(args)
      val res = ints reduce (_ * _)
      IntResult(res)
  }

  register("load") {
    case StringResult(filename) :: Nil => Interpreter.eval(new java.io.File(filename))
    case x => throw new IllegalArgumentException("Error : load : wrong type of arguments (expected string, got " + x)
  }


  register("zero?") {
    case IntResult(0) :: Nil => BoolResult(value = true)
    case IntResult(x) :: Nil => BoolResult(value = false)
    case IntResult(x) :: tail => throw new IllegalArgumentException("Error: zero?: wrong number of arguments (expected: 1 got: " + (IntResult(x) :: tail))
    case Nil => throw new IllegalArgumentException("Error: zero?: wrong number of arguments (expected: 1 got: 0)")
    case x => throw new IllegalArgumentException("Error: zero?: wrong type of arguments (expected: integer got: " + x)
  }

  register("apply") {
    case ClosureSimple(names, body, env) :: args :: Nil => {
      val values = schemeListToScala(args)
      val env2 = Interpreter.extend(env, names, values)
      Interpreter.eval(body, env2)
    }
    case ClosureVar(name, body, env) :: args :: Nil => {
      val env2 = Interpreter.extend(env, List(name), List(args))
      Interpreter.eval(body, env2)
    }
    case NativeClosure(func) :: args :: Nil => {
      val values = schemeListToScala(args)
      func(values)
    }
    case x => throw new IllegalArgumentException("Error : apply : unexpected arguments, expected function and a list of parameters, got " + x)
  }

  register("with", "(lambda (s f) (apply f s))")
  register("add1", "(lambda (n) (+ n 1))")
  register("sub1", "(lambda (n) (- n 1))")
  register("not", "(lambda(x) (if x #f #t))")
  register("compose", "(lambda(f g) (lambda(args) (f (apply g args))))")
  register("curry", "(lambda(f x) (lambda(args) (apply f (cons x args))))")

  def apply(oper: Result, arg: Result): Result = {
    oper match {
      case NativeClosure(body) => body(List(arg))
      case ClosureSimple(params, body, env) => {
        val env2 = Interpreter.extend(env, params, List(arg))
        Interpreter.eval(body, env2)
      }
      case ClosureVar(param, body, env) => {
        val env2 = Interpreter.extend(env, List(param), List(arg))
        Interpreter.eval(body, env2)
      }
    }
  }

  register("map") {
    case oper :: args :: Nil => {
      val argsList = schemeListToScala(args)
      val mapped = argsList map {
        case e: Result => apply(oper, e)
      }
      scalaListToScheme(mapped)
    }
  }

  register("filter") {
    case oper :: argsScheme :: Nil => {
      val args = schemeListToScala(argsScheme)
      val filtered = args filter {
        e: Result => apply(oper, e) match {
          case BoolResult(true) => true
          case _ => false
        }
      }
      scalaListToScheme(filtered)
    }
    case _ => throw new IllegalArgumentException("Error : filter : expected operator and list of operands")
  }

  def getIntsFrom(args: List[Result]): List[Int] = {
    args.map(res => res match {
      case IntResult(x) => x
      case _ => throw new IllegalArgumentException("Error : expected list of integers, got " + args)
    })
  }

  def scalaListToScheme(args: List[Result]): Result = {
    args match {
      case Nil => NilResult()
      case head :: tail => PairResult(head, scalaListToScheme(tail))
    }
  }

  def schemeListToScala(args: Result): List[Result] = {
    args match {
      case NilResult() => Nil
      case PairResult(head, tail) => head :: schemeListToScala(tail)
      case _ => throw new IllegalArgumentException("Error : expected to receive a valid scheme list, instead received " + args)
    }
  }

  def toScheme(args: List[Any]): Result = {
    args match {
      case Nil => NilResult()
      case head :: tail => {
        val tailResult = toScheme(tail)
        head match {
          case x: Int => PairResult(IntResult(x), tailResult)
          case x: String => PairResult(StringResult(x), tailResult)
          case x: Boolean => PairResult(BoolResult(x), tailResult)
          case None => PairResult(VoidResult(), tailResult)
          case Nil => PairResult(NilResult(), tailResult)
          case _ => throw new UnsupportedOperationException("uknown type for toScheme " + head)
        }
      }
    }
  }
}
