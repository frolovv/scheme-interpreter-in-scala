package com.vf.scheme

import org.junit.Test
import org.hamcrest.Matchers
import org.hamcrest.MatcherAssert.assertThat


class InterpreterTests {
  val ConstSamples = List(
    ("123", IntResult(123)),
    ("#t", BoolResult(value = true)),
    ("\"abc\"", StringResult("abc"))
  )

  val PrimitiveSamples = List(
    ("(+ 1 2 3)", IntResult(6)),
    ("(string-length \"abc\")", IntResult(3)),
    ("(string? 123)", BoolResult(value = false)),
    ("(string? \"abc\")", BoolResult(value = true)),
    ("(min 1 2 3 4 5 6)", IntResult(1)),
    ("(max 1 2 3 4 5 6)", IntResult(6))
  )

  val IfSamples = List(
    ("(if 1 2 3)", IntResult(2)),
    ("(if #t #f)", BoolResult(value = false)),
    ("(if #f 1)", VoidResult())
  )

  val ClosureSamples = List(
    ("((lambda(x) x) 123)", IntResult(value = 123)),
    ("((lambda(x) ((lambda(y) (+ x y)) 2)) 1)", IntResult(value = 3))
  )

  def testSamples(samples: List[(String, Result)]) {
    for ((input, expected) <- samples) {
      val actual = Interpreter.eval(input)
      assertThat(actual, Matchers.is(expected))
    }
  }

  @Test
  def testAll() {
    testSamples(ClosureSamples)
    testSamples(ConstSamples)
    testSamples(PrimitiveSamples)
    testSamples(IfSamples)
  }
}
