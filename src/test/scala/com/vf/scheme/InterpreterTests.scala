package com.vf.scheme

import org.junit.Test
import org.hamcrest.Matchers
import org.hamcrest.MatcherAssert.assertThat
import com.vf.scheme.Builtins.toScheme


class InterpreterTests {
  val ConstSamples = List(
    ("123", IntResult(123)),
    ("#t", BoolResult(value = true)),
    ("\"abc\"", StringResult("abc")),
    ("'123", IntResult(123)),
    ("'abc", SymbolResult("abc")),
    ("'(1 . 2)", PairResult(IntResult(1), IntResult(2)))
  )

  val PrimitiveSamples = List(
    ("(+ 1 2 3)", IntResult(6)),
    ("(string-length \"abc\")", IntResult(3)),
    ("(string? 123)", BoolResult(value = false)),
    ("(string? \"abc\")", BoolResult(value = true)),
    ("(min 1 2 3 4 5 6)", IntResult(1)),
    ("(max 1 2 3 4 5 6)", IntResult(6)),
    ("(integer? 1)", BoolResult(value = true)),
    ("(boolean? (integer? 123))", BoolResult(value = true)),
    ("(list 1 2)", toScheme(List(1, 2))),
    ("(pair? '(1 . 2))", BoolResult(value = true)),
    ("(pair? (list 1 2))", BoolResult(value = true)),
    ("(zero? 0)", BoolResult(value = true)),
    ("(zero? '123)", BoolResult(value = false)),
    ("(apply + '(1 2 3))", IntResult(6)),
    ("(cons 1 2)", PairResult(IntResult(1), IntResult(2))),
    ("(pair? (cons 1 2))", BoolResult(value = true)),
    ("(with '(1 2 3) +)", IntResult(6)),
    ("(map (lambda(x) (+ x 1)) '(1 2 3))", toScheme(List(2, 3, 4))),
    ("(filter integer? '(1 2 #t))", toScheme(List(1, 2)))
  )

  val IfSamples = List(
    ("(if 1 2 3)", IntResult(2)),
    ("(if #t #f)", BoolResult(value = false)),
    ("(if #f 1)", VoidResult())
  )

  val ClosureSamples = List(
    ("((lambda(x) x) 123)", IntResult(value = 123)),
    ("((lambda(x) ((lambda(y) (+ x y)) 2)) 1)", IntResult(value = 3)),
    ("((lambda x x) 1 2)", toScheme(List(1, 2)))
  )

  val SeqSamples = List(
    ("1 2 3", IntResult(3)),
    ("(define x 1) x", IntResult(1)),
    ("(define x (lambda(x) x)) (x 1)", IntResult(1))
  )

  val LetSamples = List(
    ("(let ((x 1) (y 2)) (+ x y))", IntResult(3))
  )

  val LetStarSamples = List(
    ("(let* ((x 1) (y x)) (+ x y))", IntResult(2)),
    ("(let* ((x 1) (y 2)) (+ x y))", IntResult(3))
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
    testSamples(SeqSamples)
    testSamples(LetSamples)
    testSamples(LetStarSamples)
  }
}
