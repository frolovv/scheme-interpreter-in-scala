package com.vf.scheme

object Builtins {
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

}
