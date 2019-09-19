package core

trait Parser[A] extends (String => List[(A, String)]) { self =>

  import Parser._

  def map[B](f:A => B): Parser[B] = self.flatMap(a => point(f(a)))

  def flatMap[B](f: A => Parser[B]): Parser[B] = inp =>
      for {
        (a, r1) <- self(inp)
        result <- f(a)(r1)
      } yield result
}

object Parser {
  def point[B](b:B):Parser[B] = inp => List((b,inp))
  def unit[B]:Parser[B] = _ => List()

  implicit class ParserAppendOps[A](p1:Parser[A]){
    def |+|(p2:Parser[A]):Parser[A] = input => p1(input) ++ p2(input)
  }
}

object Primitives {
  import Parser._

  //Primitive Parsers
  def result[A](a: A): Parser[A] = point(a)

  def zero[A]: Parser[A] = unit

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
  def plus[A]:Parser[A] => Parser[A] => Parser[A] = p1 => p2 => p1 |+| p2

  //operator for choice operator
  implicit class ChoiceOperator[A](private val p1:Parser[A]) {
    def ||(p2:Parser[A]):Parser[A] = plus(p1)(p2)
  }
}

object DerivedParsers{
  import Primitives._

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
