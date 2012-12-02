package com.vf.scheme

object Scanner {
  def scan(line: String): List[Token] = {
    val chars = line.toCharArray

    stateInit(chars.toList)
  }

  def implode(chars: List[Char]): NumToken = {
    val num = chars.foldRight(0)((char, res) => res * 10 + char.getNumericValue)
    NumToken(num)
  }

  def implode2string(chars: List[Char]): String = {
    chars.mkString
  }

  def implode2int(chars: List[Char]): Int = {
    implode2string(chars).toInt
  }

  def toNumOrSymbolToken(chars: List[Char]): Token = {
    try {
      NumToken(implode2int(chars reverse))
    }
    catch {
      case _: Throwable => SymbolToken(implode2string(chars reverse))
    }
  }

  def valid4symbol(char: Char): Boolean = {
    Character.isLetterOrDigit(char) || "*/+-?!".contains(char)
  }

  def stateNumber(chars: List[Char], partials: List[Char]): List[Token] = {
    chars match {
      case char :: Nil if valid4symbol(char) => List(toNumOrSymbolToken(char :: partials))
      case char :: rest if valid4symbol(char) => stateNumber(rest, char :: partials)
      case char :: rest if List(' ', ')', '(').contains(char) => toNumOrSymbolToken(partials) :: stateInit(chars)
      case _ => throw new Exception("invalid character in stateNumber [" + chars + "], partial number is " + partials)
    }
  }

  def stateHash(chars: List[Char]): List[Token] = {
    chars match {
      case 't' :: rest => BoolToken('t') :: stateInit(rest)
      case 'f' :: rest => BoolToken('f') :: stateInit(rest)
      case _ => throw new Exception("Uknown char in stateHash " + chars)
    }
  }

  def stateString(chars: List[Char], partials: List[Char]): List[Token] = {
    chars match {
      case '"' :: rest => StringToken(partials.reverse.mkString("")) :: stateInit(rest)
      case char :: rest => stateString(rest, char :: partials)
      case _ => throw new Exception("Uknown char in stateString " + chars)
    }
  }

  def stateInit(chars: List[Char]): List[Token] = {

    chars match {
      case Nil => Nil
      case '(' :: rest => Lparen() :: stateInit(rest)
      case ')' :: rest => Rparen() :: stateInit(rest)
      case ' ' :: rest => stateInit(rest)
      case '#' :: rest => stateHash(rest)
      case '\'' :: rest => QuoteToken() :: stateInit(rest)
      case '.' :: rest => DotToken() :: stateInit(rest)
      case '"' :: rest => stateString(rest, List())
      case char :: rest if valid4symbol(char) => stateNumber(chars, List())
      case char :: rest => throw new Exception("Uknown char in stateInit [" + char + "], rest is " + rest)
    }
  }
}
