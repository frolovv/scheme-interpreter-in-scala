package com.vf.scheme

import org.junit.Test

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

  def testSamples(samples: List[(String, Expr)]) {
    for ((input, expected) <- samples) {
      val actual = Parser.parse(input)
      assert(actual.equals(expected))
    }
  }

  @Test
  def testAll() {
    testSamples(ifSamples)
    testSamples(AppSamples)
    testSamples(LambdaSamples)
  }
}
