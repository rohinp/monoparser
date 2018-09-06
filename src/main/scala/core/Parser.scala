package core

import cats.implicits._

trait Parser[A] extends (String => List[(A, String)])

object Parsers{
  import core.ParserInstance._
  import core.Primitives._

  //basic derived parsers using the primitive combinator's
  def char: Char => Parser[Char] = ch => sat(x => x == ch)
  def digit:Parser[Char] = sat(x => x >= '0' && x <= '9')
  def lower:Parser[Char] = sat(x => x >= 'a' && x <= 'z')
  def upper:Parser[Char] = sat(x => x >= 'A' && x <= 'Z')

  //more parsers based on choice operator
  def letter:Parser[Char] = lower || upper
  def alphanum:Parser[Char] = letter || digit

  def word:Parser[String] = {
    def neWord:Parser[String] = for{
      _ <- letter
      r <- word
    } yield r

    neWord || result("")
  }

}