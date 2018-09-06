package core

import cats.Monad
import cats.implicits._

object Primitives {
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

  //choice combinator
  def plus[A]:Parser[A] => Parser[A] => Parser[A] = p1 => p2 => {
    str => p1(str) ++ p2(str)
  }

  //operator for choice operator
  implicit class ChoiceOperator[A](private val p1:Parser[A]) {
    def ||(p2:Parser[A]):Parser[A] = plus(p1)(p2)
  }
}
