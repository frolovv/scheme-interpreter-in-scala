package com.vf.scheme

import org.junit.Test
import org.hamcrest.Matchers

class ParserTests {
  val ifSamples = List(
    ("(if 1 2 3)", IfExpr(NumExpr(1), NumExpr(2), NumExpr(3))),
    ("(if #t #f)", IfExpr(TrueExpr(), FalseExpr(), VoidExpr())),
    ("(if \"abc\" \"abc\" \"def\")", IfExpr(StringExpr("abc"), StringExpr("abc"), StringExpr("def")))
  )

  val AppSamples = List(
    ("(+ 1 2)", AppExpr(VarExpr("+"), List(NumExpr(1), NumExpr(2)))))

  val LambdaSamples = List(
    ("(lambda (x) x)", Lambda(List("x"), VarExpr("x")))
  )

  val QuoteSamples = List(
    ("'(1)", PairExpr(NumExpr(1), NilExpr())),
    ("'1", NumExpr(1)),
    ("'abc", SymbolExpr("abc")),
    ("'(1 . 2)", PairExpr(NumExpr(1), NumExpr(2)))
  )

  def testSamples(samples: List[(String, Expr)]) {
    for ((input, expected) <- samples) {
      val actual = Parser.parse(input)
      org.hamcrest.MatcherAssert.assertThat(actual, Matchers.is(expected))
    }
  }

  @Test
  def testAll() {
    testSamples(ifSamples)
    testSamples(AppSamples)
    testSamples(LambdaSamples)
    testSamples(QuoteSamples)
  }
}
