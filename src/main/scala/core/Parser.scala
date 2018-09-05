package core

import cats.Monad
import cats.implicits._


trait Parser[A] extends (String => List[(A, String)])

object Parsers{

  import core.ParserInstance._

  //Primitive Parsers
  def result[A](a: A): Parser[A] = implicitly[Monad[Parser]].point(a)

  def zero[A]: Parser[A] = _ => List()

  def item: Parser[Char] = inp => inp.toList match {
    case List() => List()
    case x :: xs => List((x, xs.mkString))
  }

  //primitive combinator
  def sat(f:Char => Boolean):Parser[Char] =
    for{
      c <- item
      r <- if(f(c)) result(c) else zero
    } yield r

  //basic derived parsers using the primitive combinator's
  def char: Char => Parser[Char] = ch => sat(x => x == ch)
  def digit:Parser[Char] = sat(x => x >= '0' && x <= '9')
  def lower:Parser[Char] = sat(x => x >= 'a' && x <= 'z')
  def upper:Parser[Char] = sat(x => x >= 'A' && x <= 'Z')

  /*plus :: Parser a -> Parser a -> Parser a
    p `plus` q = \inp -> (p inp ++ q inp)*/
  //choice combinator
  def plus[A]:Parser[A] => Parser[A] => Parser[A] = p1 => p2 => {
    str => p1(str) ++ p2(str)
  }
  //operator for choice operator
  implicit class ChoiceOperator[A](private val p1:Parser[A]) {
    def ||(p2:Parser[A]):Parser[A] = plus(p1)(p2)
  }

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