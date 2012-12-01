package com.vf.scheme

import org.junit.Test
import org.hamcrest.Matchers


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

  val StringSamples = List(
    ("\"abc\"", List(StringToken("abc")))
  )


  def testSamples(samples: List[(String, List[Token])]) {
    for ((input, expected) <- samples) {
      val actual = Scanner.scan(input)
      org.hamcrest.MatcherAssert.assertThat(expected, Matchers.is(actual))
    }
  }

  @Test
  def testAll() {
    testSamples(NumberSamples)
    testSamples(BooleanSamples)
    testSamples(SymbolSamples)
    testSamples(ParenSamples)
    testSamples(StringSamples)
  }
}
