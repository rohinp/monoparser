package core

import cats.{Functor, Monad, Monoid}

object ParserInstance {

  implicit val parserMonadInstance = new Monoid[Parser[_]] with Functor[Parser] with Monad[Parser]{
    override def empty: Parser[_] = _ => List()

    override def pure[A](x: A): Parser[A] = str => List((x, str))

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = inp =>
      for {
        (a, r1) <- fa(inp)
        result <- f(a)(r1)
      } yield result

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = inp => f(a)(inp) match {
      case List((Left(a),_)) => tailRecM(a)(f)(inp)
      case List((Right(b),_)) => pure(b)(inp)
    }

    override def combine(x: Parser[_], y: Parser[_]): Parser[_] = str => x(str) ++ y(str)
  }

}
