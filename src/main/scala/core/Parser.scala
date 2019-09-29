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
}
