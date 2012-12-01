package com.vf.scheme

import org.junit.Test


class ScannerTests {
  val NumberSamples = List(
    ("123", List(NumToken(123))),
    ("-123", List(NumToken(-123)))
  )

  val BooleanSamples = List(
    ("#t", List(BoolToken('t'))),
    ("#f", List(BoolToken('f'))))

  val SymbolSamples = List(
    (("+ - / *"), List(SymbolToken("+"), SymbolToken("-"), SymbolToken("/"), SymbolToken("*")))
  )

  val ParenSamples = List(
    ("()", List(Lparen(), Rparen()))
  )


  def testSamples(samples: List[(String, List[Token])]) {
    for ((input, expected) <- samples) {
      val actual = Scanner.scan(input)
      assert(actual.equals(expected))
    }
  }

  @Test
  def testAll() {
    testSamples(NumberSamples)
    testSamples(BooleanSamples)
    testSamples(SymbolSamples)
    testSamples(ParenSamples)
  }
}
